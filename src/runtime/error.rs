use thiserror::Error;

use crate::language::span::Span;

pub type RuntimeResult<T> = Result<T, RuntimeError>;

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("Unknown symbol `{name}`")]
    UnknownSymbol { name: String },
    #[error("Immutable binding `{name}` cannot be reassigned")]
    ImmutableBinding { name: String },
    #[error("Type mismatch: {message}")]
    TypeMismatch { message: String },
    #[error("Operation not supported: {message}")]
    Unsupported { message: String },
    #[error("Runtime panic: {message}")]
    Panic { message: String },
    #[error("Match error: {message}")]
    MatchError { message: String },
    #[error("Function `{name}` expected {expected} arguments but received {received}")]
    ArityMismatch {
        name: String,
        expected: usize,
        received: usize,
    },
    #[error("{message}")]
    WithSpan { message: String, span: Span },
}

impl RuntimeError {
    pub fn with_span(self, span: Span) -> Self {
        match self {
            RuntimeError::WithSpan { message, .. } => RuntimeError::WithSpan { message, span },
            other => RuntimeError::WithSpan {
                message: other.to_string(),
                span,
            },
        }
    }
}
