use crate::{
    language::{errors::SyntaxError, span::Span, typecheck::TypeError},
    project::{FileErrors, diagnostics::ManifestIssue},
    runtime::error::RuntimeError,
};
use miette::{Diagnostic, NamedSource, Report, SourceSpan};
use std::path::Path;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic, Clone)]
#[error("{message}")]
pub struct SyntaxDiagnostic {
    #[source_code]
    src: NamedSource<String>,
    #[label("{label}")]
    span: SourceSpan,
    #[help]
    help: Option<String>,
    message: String,
    label: String,
}

impl SyntaxDiagnostic {
    pub fn from_error(src: NamedSource<String>, err: SyntaxError) -> Self {
        Self {
            src,
            span: err.to_source_span(),
            help: err.help.clone(),
            message: err.message.clone(),
            label: err.label,
        }
    }
}

pub fn emit_syntax_errors(errors: &[FileErrors]) {
    for file in errors {
        let src = NamedSource::new(file.path.display().to_string(), file.source.clone());
        for err in &file.errors {
            let diagnostic = SyntaxDiagnostic::from_error(src.clone(), err.clone());
            eprintln!("{:?}", Report::new(diagnostic));
        }
    }
}

pub fn report_runtime_error(error: &RuntimeError) {
    eprintln!("Runtime error: {}", error);
}

pub fn report_io_error(path: &Path, error: &std::io::Error) {
    eprintln!("Failed to access {}: {}", path.display(), error);
}

#[derive(Debug, Error, Diagnostic, Clone)]
#[error("{message}")]
pub struct ManifestDiagnostic {
    #[source_code]
    src: NamedSource<String>,
    #[label("{message}")]
    span: SourceSpan,
    message: String,
}

pub fn emit_manifest_issues(named: &NamedSource<String>, issues: &[ManifestIssue]) -> bool {
    let mut emitted = false;
    for issue in issues {
        let span = issue
            .span
            .map(span_to_source_span)
            .unwrap_or_else(|| SourceSpan::from((0, 0)));
        let diagnostic = ManifestDiagnostic {
            src: named.clone(),
            span,
            message: issue.kind.message(),
        };
        eprintln!("{:?}", Report::new(diagnostic));
        emitted = true;
    }
    emitted
}

#[derive(Debug, Error, Diagnostic, Clone)]
#[error("{message}")]
pub struct TypeDiagnostic {
    #[source_code]
    src: NamedSource<String>,
    #[label("{label}")]
    span: SourceSpan,
    message: String,
    label: String,
    #[help]
    help: Option<String>,
}

pub fn emit_type_errors(errors: &[TypeError]) {
    for err in errors {
        match std::fs::read_to_string(&err.path) {
            Ok(contents) => {
                let named = NamedSource::new(err.path.display().to_string(), contents);
                let diagnostic = TypeDiagnostic {
                    span: span_to_source_span(err.span),
                    message: err.display_message(),
                    label: err.label.clone(),
                    src: named,
                    help: err.help.clone(),
                };
                eprintln!("{:?}", Report::new(diagnostic));
            }
            Err(read_err) => {
                eprintln!(
                    "type error at {}:{} - {}",
                    err.path.display(),
                    err.span.start,
                    read_err
                );
            }
        }
    }
}

fn span_to_source_span(span: Span) -> SourceSpan {
    SourceSpan::from((span.start, span.end.saturating_sub(span.start)))
}
