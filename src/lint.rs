use crate::diagnostics::emit_parse_errors;
use crate::parser::{LexToken, Token, parse, tokenize};
use miette::{Diagnostic, NamedSource, Report, Result, SourceSpan};
use notify::{Config, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use std::error::Error;
use std::fs;
use std::path::Path;
use std::sync::mpsc;
use std::time::Duration;
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
#[error("Unknown token spotted:")]
#[diagnostic(code(main::prime::error), url(docsrs))]
struct UnknownTokenDiagnostic {
    #[source_code]
    src: NamedSource<String>,
    #[label("unknown token")]
    bad_bit: SourceSpan,
}

pub fn run_lint(file_path: &Path, watch: bool) -> Result<(), Box<dyn Error + Sync + Send>> {
    run_single_check(file_path)?;

    if !watch {
        return Ok(());
    }

    let path_buf = file_path.to_path_buf();
    let (tx, rx) = mpsc::channel();
    let mut watcher = build_watcher(tx)?;
    watcher.watch(&path_buf, RecursiveMode::NonRecursive)?;

    println!(
        "Watching {} for changes. Press Ctrl+C to stop...",
        path_buf.display()
    );

    for res in rx {
        match res {
            Ok(event) => match event.kind {
                EventKind::Modify(_) | EventKind::Create(_) | EventKind::Remove(_) => {
                    if let Err(err) = run_single_check(&path_buf) {
                        eprintln!("lint error: {err}");
                    }
                }
                _ => {}
            },
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

fn run_single_check(file_path: &Path) -> Result<(), Box<dyn Error + Sync + Send>> {
    let content = fs::read_to_string(file_path)?;
    let tokens = tokenize(&content);

    let mut emitted = emit_unknown_tokens(file_path, &content, &tokens);

    if let Err(errors) = parse(&tokens) {
        emit_parse_errors(file_path, &content, &errors);
        emitted = true;
    }

    if !emitted {
        println!("{}: no diagnostics", file_path.display());
    }

    Ok(())
}

fn emit_unknown_tokens(file_path: &Path, content: &str, tokens: &[LexToken]) -> bool {
    let mut found = false;
    for (index, token) in tokens.iter().enumerate() {
        if matches!(token.token, Token::Unknown) {
            found = true;
            println!("Element 'Unknown' at position {}", index);
            let span: SourceSpan = (
                token.span.start,
                token.span.end.saturating_sub(token.span.start),
            )
                .into();
            emit_unknown_diagnostic(file_path, content, span);
        }
    }
    found
}

fn emit_unknown_diagnostic(file_path: &Path, content: &str, span: SourceSpan) {
    let diag = UnknownTokenDiagnostic {
        src: NamedSource::new(file_path.display().to_string(), content.to_string()),
        bad_bit: span,
    };
    eprintln!("{:?}", Report::new(diag));
}
