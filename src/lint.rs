use crate::language::{
    ast::{ImportPath, Module},
    errors::SyntaxError,
    lexer::lex,
    parser::parse_module,
    span::Span,
};
use crate::project::{find_manifest, manifest::PackageManifest};
use miette::{Diagnostic, NamedSource, Report, SourceSpan};
use notify::{Config, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use std::{
    collections::HashSet,
    error::Error,
    fs,
    path::{Path, PathBuf},
    sync::mpsc,
    time::Duration,
};
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
#[error("Lexing error: {message}")]
#[diagnostic(code(prime::lint::lex))]
struct LexDiagnostic {
    #[source_code]
    src: NamedSource<String>,
    #[label("{message}")]
    span: SourceSpan,
    message: String,
}

#[derive(Debug, Error, Diagnostic)]
#[error("{message}")]
#[diagnostic(code(prime::lint::manifest))]
struct ManifestDiagnostic {
    #[source_code]
    src: NamedSource<String>,
    #[label("{message}")]
    span: SourceSpan,
    message: String,
}

pub fn run_lint(file_path: &Path, watch: bool) -> Result<(), Box<dyn Error + Send + Sync>> {
    run_single_check(file_path)?;

    if !watch {
        return Ok(());
    }

    let path_buf = file_path.to_path_buf();
    let (tx, rx) = mpsc::channel();
    let mut watcher = build_watcher(tx)?;
    watcher.watch(&path_buf, RecursiveMode::NonRecursive)?;

    println!(
        "Watching {} for changes. Press Ctrl+C to stop…",
        path_buf.display()
    );

    for event in rx {
        match event {
            Ok(evt) => {
                if matches!(
                    evt.kind,
                    EventKind::Modify(_) | EventKind::Create(_) | EventKind::Remove(_)
                ) {
                    if let Err(err) = run_single_check(&path_buf) {
                        eprintln!("lint error: {err}");
                    }
                }
            }
            Err(err) => eprintln!("watch error: {err}"),
        }
    }

    Ok(())
}

fn build_watcher(
    tx: mpsc::Sender<Result<notify::Event, notify::Error>>,
) -> notify::Result<RecommendedWatcher> {
    notify::recommended_watcher(move |res| {
        let _ = tx.send(res);
    })
    .map(|mut watcher| {
        watcher
            .configure(Config::default().with_poll_interval(Duration::from_millis(200)))
            .ok();
        watcher
    })
}

fn run_single_check(path: &Path) -> Result<(), Box<dyn Error + Send + Sync>> {
    let source = fs::read_to_string(path)?;
    let module_name = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main")
        .to_string();

    let _tokens = match lex(&source) {
        Ok(tokens) => tokens,
        Err(errors) => {
            report_lex_errors(path, &source, &errors);
            return Ok(());
        }
    };

    match parse_module(&module_name, path.to_path_buf(), &source) {
        Ok(module) => {
            let mut emitted = false;
            emitted |= lint_manifest_issues(path, &source, &module);
            if !emitted {
                println!("{}: no diagnostics", path.display());
            }
        }
        Err(errs) => emit_syntax_errors_for_source(path, &source, &errs.errors),
    }

    Ok(())
}

fn report_lex_errors(path: &Path, source: &str, errors: &[crate::language::lexer::LexError]) {
    let named = NamedSource::new(path.display().to_string(), source.to_string());
    for err in errors {
        let diag = LexDiagnostic {
            src: named.clone(),
            span: (err.span.start, err.span.len()).into(),
            message: err.message.clone(),
        };
        eprintln!("{:?}", Report::new(diag));
    }
}

fn emit_syntax_errors_for_source(path: &Path, source: &str, errors: &[SyntaxError]) {
    let named = NamedSource::new(path.display().to_string(), source.to_string());
    for err in errors {
        let diag = crate::diagnostics::SyntaxDiagnostic::from_error(named.clone(), err.clone());
        eprintln!("{:?}", Report::new(diag));
    }
}

fn lint_manifest_issues(path: &Path, source: &str, module: &Module) -> bool {
    let Some(manifest_path) = find_manifest(path) else {
        return false;
    };
    let Ok(manifest) = PackageManifest::load(&manifest_path) else {
        return false;
    };
    let named = NamedSource::new(path.display().to_string(), source.to_string());
    let mut emitted = false;
    let header_span = module
        .declared_span
        .or_else(|| module.declared_name.as_ref().map(|_| Span::new(0, 0)));
    match (
        manifest.module_name_for_path(path).as_deref(),
        module.declared_name.as_deref(),
    ) {
        (Some(expected), Some(actual)) if expected != actual => {
            emit_manifest_diag(
                &named,
                module.declared_span,
                format!(
                    "Module declared as `{actual}` but manifest maps this file to `{expected}`"
                ),
            );
            emitted = true;
        }
        (Some(expected), None) => {
            emit_manifest_diag(
                &named,
                header_span,
                format!(
                    "Manifest maps this file to `{expected}` but file is missing `module {expected};`"
                ),
            );
            emitted = true;
        }
        (None, Some(actual)) => {
            emit_manifest_diag(
                &named,
                module.declared_span,
                format!(
                    "Module `{actual}` is declared here but not listed in prime.toml ({}).",
                    manifest.path.display()
                ),
            );
            emitted = true;
        }
        _ => {}
    }
    for span in &module.redundant_module_spans {
        emit_manifest_diag(
            &named,
            Some(*span),
            "Duplicate `module` declaration; only the first declaration is used".into(),
        );
        emitted = true;
    }
    let mut seen = HashSet::new();
    for import in &module.imports {
        let key = import.path.to_string();
        if !seen.insert(key.clone()) {
            emit_manifest_diag(
                &named,
                Some(import.span),
                format!("Duplicate import `{key}`"),
            );
            emitted = true;
            continue;
        }
        if manifest.module_path(&key).is_some() {
            continue;
        }
        let resolved = resolve_import_path(path, &import.path);
        if resolved.exists() {
            let rel = manifest_relative_string(&resolved, &manifest);
            emit_manifest_diag(
                &named,
                Some(import.span),
                format!(
                    "Module `{}` exists at `{}` but is missing from prime.toml",
                    key, rel
                ),
            );
        } else {
            emit_manifest_diag(
                &named,
                Some(import.span),
                format!("Cannot resolve import `{}`", key),
            );
        }
        emitted = true;
    }
    emitted
}

fn emit_manifest_diag(named: &NamedSource<String>, span: Option<Span>, message: String) {
    let source_span = span
        .map(|sp| SourceSpan::from((sp.start, sp.end.saturating_sub(sp.start))))
        .unwrap_or_else(|| SourceSpan::from((0, 0)));
    let diag = ManifestDiagnostic {
        src: named.clone(),
        span: source_span,
        message,
    };
    eprintln!("{:?}", Report::new(diag));
}

fn resolve_import_path(base: &Path, import: &ImportPath) -> PathBuf {
    let mut path = import.to_relative_path();
    if path.extension().and_then(|ext| ext.to_str()) != Some("prime") {
        path.set_extension("prime");
    }
    if path.is_absolute() {
        return path;
    }
    let base_dir = base
        .parent()
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| PathBuf::from("."));
    base_dir.join(path)
}

fn manifest_relative_string(path: &Path, manifest: &PackageManifest) -> String {
    let relative = path.strip_prefix(manifest.root()).unwrap_or(path);
    relative
        .components()
        .map(|component| component.as_os_str().to_string_lossy())
        .collect::<Vec<_>>()
        .join("/")
}
