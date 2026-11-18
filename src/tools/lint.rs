use crate::{
    language::{errors::SyntaxError, lexer::lex, parser::parse_module},
    project::{diagnostics::analyze_manifest_issues, find_manifest, manifest::PackageManifest},
    tools::diagnostics::{emit_manifest_issues, manifest_issue_severity},
};
use miette::{Diagnostic, NamedSource, Report, Severity, SourceSpan};
use notify::{Config, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use std::{error::Error, fs, path::Path, sync::mpsc, time::Duration};
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

pub fn run_lint(file_path: &Path, watch: bool) -> Result<(), Box<dyn Error + Send + Sync>> {
    let path = std::path::absolute(file_path)?;
    if !watch {
        return run_single_check(&path);
    }

    // Editors often save by replacing the file. Watch its directory so the
    // next edit is still visible after the original inode disappears.
    let parent = path
        .parent()
        .ok_or("cannot watch a path without a parent")?
        .canonicalize()?;
    let path = parent.join(path.file_name().ok_or("missing file name")?);
    let (tx, rx) = mpsc::channel();
    let mut watcher = build_watcher(tx)?;
    watcher.watch(&parent, RecursiveMode::NonRecursive)?;

    let check = || {
        if let Err(err) = run_single_check(&path) {
            eprintln!("lint error: {err}");
        }
    };
    check();
    println!(
        "Watching {} for changes. Press Ctrl+C to stop…",
        path.display()
    );

    for event in rx {
        let event = event?;
        if event.paths.contains(&path)
            && matches!(
                event.kind,
                EventKind::Modify(_) | EventKind::Create(_) | EventKind::Remove(_)
            )
        {
            check();
        }
    }

    Err("file watcher disconnected".into())
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
    let source = fs::read_to_string(path).map_err(|err| format!("{}: {err}", path.display()))?;
    let module_name = path.file_stem().and_then(|s| s.to_str()).unwrap_or("main");
    let module = parse_module(module_name, path.to_path_buf(), &source).map_err(|errs| {
        if let Err(errors) = lex(&source) {
            report_lex_errors(path, &source, &errors);
            "failed to lex module"
        } else {
            emit_syntax_errors_for_source(path, &source, &errs.errors);
            "failed to parse module"
        }
    })?;
    let manifest = find_manifest(path)
        .map(|manifest_path| PackageManifest::load(&manifest_path))
        .transpose()?;
    let issues = analyze_manifest_issues(&module, path, manifest.as_ref());
    if issues.is_empty() {
        println!("{}: no diagnostics", path.display());
    } else {
        let named = NamedSource::new(path.display().to_string(), source);
        emit_manifest_issues(&named, &issues);
        if issues
            .iter()
            .any(|issue| manifest_issue_severity(&issue.kind) == Severity::Error)
        {
            return Err("module has manifest errors".into());
        }
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
        let diag =
            crate::tools::diagnostics::SyntaxDiagnostic::from_error(named.clone(), err.clone());
        eprintln!("{:?}", Report::new(diag));
    }
}
