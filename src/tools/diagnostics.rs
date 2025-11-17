use crate::{language::errors::SyntaxError, project::FileErrors, runtime::error::RuntimeError};
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
