use crate::language::span::Span;
use miette::SourceSpan;

#[derive(Clone, Debug)]
pub struct SyntaxError {
    pub message: String,
    pub span: Span,
    pub help: Option<String>,
}

impl SyntaxError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
            help: None,
        }
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn to_source_span(&self) -> SourceSpan {
        (self.span.start, self.span.len()).into()
    }
}

#[derive(Clone, Debug)]
pub struct SyntaxErrors {
    pub errors: Vec<SyntaxError>,
}

impl SyntaxErrors {
    pub fn new(errors: Vec<SyntaxError>) -> Self {
        Self { errors }
    }
}
