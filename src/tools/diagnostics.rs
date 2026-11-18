use crate::{
    language::{errors::SyntaxError, span::Span, typecheck::TypeError},
    project::{
        FileErrors,
        diagnostics::{ManifestIssue, ManifestIssueKind},
    },
    runtime::error::RuntimeError,
};
use miette::{Diagnostic, MietteDiagnostic, NamedSource, Report, Severity, SourceSpan};
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
#[error("{details}")]
#[diagnostic(forward(details))]
pub struct ManifestDiagnostic {
    #[source_code]
    src: NamedSource<String>,
    #[label("{details}")]
    span: SourceSpan,
    details: MietteDiagnostic,
}

// Match the manifest diagnostic levels used by the language server.
pub(super) fn manifest_issue_severity(kind: &ManifestIssueKind) -> Severity {
    use ManifestIssueKind::*;
    match kind {
        ModuleKindMismatch { .. } | UnknownImport { .. } => Severity::Error,
        ModuleNameMismatch { .. }
        | MissingModuleHeader { .. }
        | DeclaredModuleNotInManifest { .. }
        | DuplicateModuleDeclaration
        | DuplicateImport { .. }
        | ManifestMissingModule { .. } => Severity::Warning,
    }
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
            details: MietteDiagnostic::new(issue.kind.message())
                .with_severity(manifest_issue_severity(&issue.kind)),
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
    emit_type_errors_with_source(errors, |path| std::fs::read_to_string(path));
}

pub fn emit_type_errors_with_source(
    errors: &[TypeError],
    read_source: impl Fn(&Path) -> std::io::Result<String>,
) {
    for err in errors {
        match read_source(&err.path) {
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
                    "type error at {}:{} - {} (could not read source: {})",
                    err.path.display(),
                    err.span.start,
                    err.display_message(),
                    read_err
                );
            }
        }
    }
}

fn span_to_source_span(span: Span) -> SourceSpan {
    SourceSpan::from((span.start, span.end.saturating_sub(span.start)))
}
