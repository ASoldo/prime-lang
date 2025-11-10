use crate::language::{errors::SyntaxError, lexer::lex, parser::parse_module};
use miette::{Diagnostic, NamedSource, Report, SourceSpan};
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
        Ok(_) => {
            println!("{}: no diagnostics", path.display());
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
