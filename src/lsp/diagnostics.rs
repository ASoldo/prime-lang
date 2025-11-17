use crate::language::{
    ast::Module,
    errors::SyntaxError,
    lexer::{LexError, lex},
    span::Span,
};
use serde_json::json;
use std::{collections::HashSet, fs, path::PathBuf};
use tower_lsp_server::{
    lsp_types::{
        CodeAction, CodeActionKind, CodeActionOrCommand, Diagnostic, DiagnosticSeverity,
        NumberOrString, Range, TextEdit, Uri, WorkspaceEdit,
    },
    UriExt,
};

use super::{
    parser::parse_module_from_uri,
    text::{
        adjust_zero_length_offset, first_non_whitespace_span, manifest_context_for_uri,
        manifest_relative_string, offset_to_position, resolve_import_relative_path, span_to_range,
    },
};

pub const CODE_MISSING_MODULE_HEADER: &str = "prime.missingModuleHeader";
pub const CODE_MANIFEST_MISSING_MODULE: &str = "prime.manifestMissingModule";
pub const CODE_DUPLICATE_IMPORT: &str = "prime.duplicateImport";
pub const CODE_UNKNOWN_IMPORT: &str = "prime.unknownImport";

pub fn collect_parse_and_manifest_diagnostics(
    uri: &Uri,
    text: &str,
) -> (Option<Module>, Vec<Diagnostic>) {
    let mut diags = Vec::new();
    let tokens = match lex(text) {
        Ok(tokens) => tokens,
        Err(errors) => {
            diags.extend(errors.into_iter().map(|err| lex_error_to_lsp(text, err)));
            return (None, diags);
        }
    };
    let module = match parse_module_from_uri(uri, text) {
        Ok(module) => Some(module),
        Err(errors) => {
            diags.extend(
                errors
                    .errors
                    .into_iter()
                    .map(|err| syntax_error_to_lsp(text, err)),
            );
            None
        }
    };
    if let Some(module) = &module {
        diags.extend(manifest_declaration_diagnostics(uri, text, module));
        diags.extend(import_manifest_diagnostics(uri, text, module));
    }
    drop(tokens);
    (module, diags)
}

pub fn diagnostic_code(diagnostic: &Diagnostic) -> Option<&str> {
    match diagnostic.code.as_ref()? {
        NumberOrString::String(code) => Some(code.as_str()),
        _ => None,
    }
}

pub struct ManifestEntryAction {
    module_name: String,
    module_path: String,
    manifest_path: PathBuf,
    visibility: String,
    diagnostic: Diagnostic,
}

impl ManifestEntryAction {
    pub fn to_code_action(self) -> Option<CodeActionOrCommand> {
        let manifest_text = fs::read_to_string(&self.manifest_path).ok()?;
        let manifest_uri = Uri::from_file_path(&self.manifest_path)?;
        let end_pos = offset_to_position(&manifest_text, manifest_text.len());
        let entry = format!(
            "[[modules]]\nname = \"{}\"\npath = \"{}\"\nvisibility = \"{}\"\n",
            self.module_name, self.module_path, self.visibility
        );
        let mut insert = String::new();
        if !manifest_text.is_empty() && !manifest_text.ends_with('\n') {
            insert.push('\n');
        }
        insert.push('\n');
        insert.push_str(&entry);
        if !insert.ends_with('\n') {
            insert.push('\n');
        }
        let edit = TextEdit {
            range: Range::new(end_pos, end_pos),
            new_text: insert,
        };
        Some(CodeActionOrCommand::CodeAction(CodeAction {
            title: format!("Add `{}` to manifest", self.module_name),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: Some(vec![self.diagnostic]),
            edit: Some(WorkspaceEdit {
                changes: Some([(manifest_uri, vec![edit])].into_iter().collect()),
                ..Default::default()
            }),
            ..Default::default()
        }))
    }
}

pub fn manifest_entry_action(diagnostic: &Diagnostic) -> Option<ManifestEntryAction> {
    let data = diagnostic.data.as_ref()?;
    let module_name = data.get("module_name")?.as_str()?.to_string();
    let module_path = data.get("module_path")?.as_str()?.to_string();
    let manifest_path = PathBuf::from(data.get("manifest_path")?.as_str()?);
    let visibility = data
        .get("visibility")
        .and_then(|v| v.as_str())
        .unwrap_or("pub")
        .to_string();
    Some(ManifestEntryAction {
        module_name,
        module_path,
        manifest_path,
        visibility,
        diagnostic: diagnostic.clone(),
    })
}

fn lex_error_to_lsp(text: &str, err: LexError) -> Diagnostic {
    Diagnostic {
        range: span_to_range(text, err.span),
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("prime-lang".into()),
        message: super::text::error_with_context(&err.message, None, text, err.span),
        ..Default::default()
    }
}

fn syntax_error_to_lsp(text: &str, err: SyntaxError) -> Diagnostic {
    let mut message = super::text::prettify_error_message(&err.message);
    if let Some(help) = &err.help {
        if !help.trim().is_empty() {
            message.push('\n');
            message.push_str(help);
        }
    }
    let span = if err.span.start < err.span.end && err.message.starts_with("Expected") {
        let fallback = adjust_zero_length_offset(text, err.span.start);
        Span::new(fallback, fallback.saturating_add(1).min(text.len()))
    } else {
        err.span
    };
    Diagnostic {
        range: span_to_range(text, span),
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("prime-lang".into()),
        message,
        ..Default::default()
    }
}

fn manifest_declaration_diagnostics(uri: &Uri, text: &str, module: &Module) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let Some((manifest, file_path)) = manifest_context_for_uri(uri) else {
        return diags;
    };
    let expected = manifest.module_name_for_path(&file_path);
    let declared = module.declared_name.as_deref();
    match (expected.as_deref(), declared) {
        (Some(expected_name), Some(actual)) if expected_name != actual => {
            diags.push(module_mismatch_diagnostic(
                text,
                module.declared_span,
                &format!(
                    "Module declared as `{actual}` but manifest maps this file to `{expected_name}`"
                ),
                None,
                None,
            ));
        }
        (Some(expected_name), None) => {
            diags.push(module_mismatch_diagnostic(
                text,
                None,
                &format!(
                    "Manifest maps this file to `{expected_name}` but the file is missing `module {expected_name};`"
                ),
                Some(CODE_MISSING_MODULE_HEADER),
                Some(json!({
                    "module_name": expected_name,
                })),
            ));
        }
        (None, Some(actual)) => {
            let module_path = manifest_relative_string(&file_path, &manifest);
            diags.push(module_mismatch_diagnostic(
                text,
                module.declared_span,
                &format!("Module `{actual}` is declared in this file but not listed in prime.toml"),
                Some(CODE_MANIFEST_MISSING_MODULE),
                Some(json!({
                    "module_name": actual,
                    "module_path": module_path,
                    "manifest_path": manifest.path.to_string_lossy().to_string(),
                    "visibility": "pub",
                })),
            ));
        }
        _ => {}
    }
    for span in &module.redundant_module_spans {
        diags.push(Diagnostic {
            range: span_to_range(text, *span),
            severity: Some(DiagnosticSeverity::WARNING),
            source: Some("prime-lang".into()),
            message: "Duplicate `module` declaration; only the first declaration is used".into(),
            ..Default::default()
        });
    }
    diags
}

fn import_manifest_diagnostics(uri: &Uri, text: &str, module: &Module) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let Some((manifest, file_path)) = manifest_context_for_uri(uri) else {
        return diags;
    };
    let mut seen = HashSet::new();
    for import in &module.imports {
        let import_name = import.path.to_string();
        if !seen.insert(import_name.clone()) {
            diags.push(Diagnostic {
                range: span_to_range(text, import.span),
                severity: Some(DiagnosticSeverity::WARNING),
                source: Some("prime-lang".into()),
                message: format!("Duplicate import `{import_name}`"),
                code: Some(NumberOrString::String(CODE_DUPLICATE_IMPORT.into())),
                ..Default::default()
            });
            continue;
        }
        if manifest.module_path(&import_name).is_some() {
            continue;
        }
        let resolved = resolve_import_relative_path(&file_path, &import.path);
        if resolved.exists() {
            let module_path = manifest_relative_string(&resolved, &manifest);
            diags.push(Diagnostic {
                range: span_to_range(text, import.span),
                severity: Some(DiagnosticSeverity::WARNING),
                source: Some("prime-lang".into()),
                message: format!(
                    "Module `{import_name}` exists on disk but is not listed in prime.toml"
                ),
                code: Some(NumberOrString::String(
                    CODE_MANIFEST_MISSING_MODULE.to_string(),
                )),
                data: Some(json!({
                    "module_name": import_name,
                    "module_path": module_path,
                    "manifest_path": manifest.path.to_string_lossy().to_string(),
                    "visibility": "pub",
                })),
                ..Default::default()
            });
        } else {
            diags.push(Diagnostic {
                range: span_to_range(text, import.span),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("prime-lang".into()),
                message: format!(
                    "Cannot resolve import `{import_name}` — no manifest entry or file found"
                ),
                code: Some(NumberOrString::String(CODE_UNKNOWN_IMPORT.into())),
                ..Default::default()
            });
        }
    }
    diags
}

fn module_mismatch_diagnostic(
    text: &str,
    span: Option<Span>,
    message: &str,
    code: Option<&str>,
    data: Option<serde_json::Value>,
) -> Diagnostic {
    let highlight = span.unwrap_or_else(|| first_non_whitespace_span(text));
    Diagnostic {
        range: span_to_range(text, highlight),
        severity: Some(DiagnosticSeverity::WARNING),
        source: Some("prime-lang".into()),
        message: message.to_string(),
        code: code.map(|c| NumberOrString::String(c.to_string())),
        data,
        ..Default::default()
    }
}
