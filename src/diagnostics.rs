use crate::parser::ParseError;
use miette::{Diagnostic, NamedSource, Report};
use std::path::Path;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
#[error("{message}")]
pub struct ParserDiagnostic {
    pub message: String,
    pub label: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[label("{label}")]
    pub span: miette::SourceSpan,
    #[help("{help_msg}")]
    pub help_msg: Option<String>,
}

impl ParserDiagnostic {
    pub fn from_error(src: NamedSource<String>, err: ParseError) -> Self {
        Self {
            message: err.message,
            label: err.label,
            span: err.span,
            src,
            help_msg: err.help,
        }
    }
}

pub fn emit_parse_errors(path: &Path, source: &str, errors: &[ParseError]) {
    let named = NamedSource::new(path.display().to_string(), source.to_string());
    for err in errors {
        let diagnostic = ParserDiagnostic::from_error(named.clone(), err.clone());
        eprintln!("{:?}", Report::new(diagnostic));
    }
}
