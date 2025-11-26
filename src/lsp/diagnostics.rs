use crate::{
    language::{
        ast::{ImportPath, Module, Program},
        errors::SyntaxError,
        lexer::{LexError, lex},
        macro_expander,
        parser::parse_module,
        span::Span,
        typecheck::{TypeError, check_program},
    },
    project::{
        diagnostics::{
            CODE_DUPLICATE_IMPORT, CODE_MANIFEST_MISSING_MODULE, CODE_MISSING_MODULE_HEADER,
            CODE_UNKNOWN_IMPORT, ManifestIssue, ManifestIssueKind, analyze_manifest_issues,
        },
        manifest::{ModuleVisibility, PackageManifest, manifest_key_for},
    },
};
use serde_json::json;
use std::{
    collections::{HashSet, VecDeque},
    fs,
    path::{Path, PathBuf},
};
use tower_lsp_server::{
    UriExt,
    lsp_types::{
        CodeAction, CodeActionKind, CodeActionOrCommand, Diagnostic, DiagnosticSeverity,
        NumberOrString, Position, Range, TextEdit, Uri, WorkspaceEdit,
    },
};
use toml_edit::{DocumentMut, InlineTable, Item as TomlItem, Table as TomlTable, Value as TomlValue};

use super::{
    parser::parse_module_from_uri,
    text::{
        adjust_zero_length_offset, first_non_whitespace_span, manifest_context_for_uri,
        offset_to_position, span_to_range,
    },
};

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
    let manifest_context = manifest_context_for_uri(uri);
    if let Some(module) = &module {
        if let Some((manifest, file_path)) = &manifest_context {
            let issues = analyze_manifest_issues(module, &file_path, Some(&manifest));
            diags.extend(
                issues
                    .into_iter()
                    .map(|issue| manifest_issue_to_diagnostic(text, issue)),
            );
        }
        let manifest_ref = manifest_context.as_ref().map(|(manifest, _)| manifest);
        let path_ref = manifest_context.as_ref().map(|(_, path)| path.as_path());
        diags.extend(type_diagnostics(text, module, manifest_ref, path_ref));
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
        let mut doc: DocumentMut = manifest_text.parse().ok()?;
        let mut inline = InlineTable::new();
        inline.insert("name", TomlValue::from(self.module_name.clone()));
        inline.insert("path", TomlValue::from(self.module_path.clone()));
        inline.insert("visibility", TomlValue::from(self.visibility.clone()));
        let key = manifest_key_for(&self.module_name);
        let entry = doc
            .entry("libraries")
            .or_insert(TomlItem::Table(TomlTable::new()));
        let Some(table) = entry.as_table_like_mut() else {
            return None;
        };
        table.insert(&key, TomlItem::Value(toml_edit::Value::InlineTable(inline)));
        let end_pos = offset_to_position(&manifest_text, manifest_text.len());
        let new_manifest = doc.to_string();
        let edit = TextEdit {
            range: Range::new(Position::new(0, 0), end_pos),
            new_text: new_manifest,
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

fn manifest_issue_to_diagnostic(text: &str, issue: ManifestIssue) -> Diagnostic {
    match issue.kind {
        ManifestIssueKind::ModuleNameMismatch { expected, actual } => module_mismatch_diagnostic(
            text,
            issue.span,
            &format!("Module declared as `{actual}` but manifest maps this file to `{expected}`"),
            None,
            None,
        ),
        ManifestIssueKind::MissingModuleHeader { expected } => module_mismatch_diagnostic(
            text,
            issue.span,
            &format!(
                "Manifest maps this file to `{expected}` but the file is missing `module {expected};`"
            ),
            Some(CODE_MISSING_MODULE_HEADER),
            Some(json!({ "module_name": expected })),
        ),
        ManifestIssueKind::DeclaredModuleNotInManifest {
            declared,
            manifest_path,
            module_path,
        } => module_mismatch_diagnostic(
            text,
            issue.span,
            &format!(
                "Module `{declared}` is declared here but not listed in prime.toml ({})",
                manifest_path.display()
            ),
            Some(CODE_MANIFEST_MISSING_MODULE),
            Some(json!({
                "module_name": declared,
                "module_path": module_path,
                "manifest_path": manifest_path.to_string_lossy().to_string(),
                "visibility": "pub",
            })),
        ),
        ManifestIssueKind::DuplicateModuleDeclaration => Diagnostic {
            range: span_to_range(
                text,
                issue
                    .span
                    .unwrap_or_else(|| first_non_whitespace_span(text)),
            ),
            severity: Some(DiagnosticSeverity::WARNING),
            source: Some("prime-lang".into()),
            message: "Duplicate `module` declaration; only the first declaration is used".into(),
            ..Default::default()
        },
        ManifestIssueKind::DuplicateImport { module } => Diagnostic {
            range: span_to_range(
                text,
                issue
                    .span
                    .unwrap_or_else(|| first_non_whitespace_span(text)),
            ),
            severity: Some(DiagnosticSeverity::WARNING),
            source: Some("prime-lang".into()),
            message: format!("Duplicate import `{module}`"),
            code: Some(NumberOrString::String(CODE_DUPLICATE_IMPORT.into())),
            ..Default::default()
        },
        ManifestIssueKind::ManifestMissingModule {
            module,
            module_path,
            manifest_path,
            visibility,
        } => Diagnostic {
            range: span_to_range(
                text,
                issue
                    .span
                    .unwrap_or_else(|| first_non_whitespace_span(text)),
            ),
            severity: Some(DiagnosticSeverity::WARNING),
            source: Some("prime-lang".into()),
            message: format!(
                "Module `{module}` exists at `{module_path}` but is not listed in prime.toml ({})",
                manifest_path.display()
            ),
            code: Some(NumberOrString::String(
                CODE_MANIFEST_MISSING_MODULE.to_string(),
            )),
            data: Some(json!({
                "module_name": module,
                "module_path": module_path,
                "manifest_path": manifest_path.to_string_lossy().to_string(),
                "visibility": module_visibility_label(visibility),
            })),
            ..Default::default()
        },
        ManifestIssueKind::UnknownImport { module } => Diagnostic {
            range: span_to_range(
                text,
                issue
                    .span
                    .unwrap_or_else(|| first_non_whitespace_span(text)),
            ),
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("prime-lang".into()),
            message: format!("Cannot resolve import `{module}` — no manifest entry or file found"),
            code: Some(NumberOrString::String(CODE_UNKNOWN_IMPORT.into())),
            ..Default::default()
        },
        ManifestIssueKind::ModuleKindMismatch { expected, actual } => Diagnostic {
            range: span_to_range(
                text,
                issue
                    .span
                    .unwrap_or_else(|| first_non_whitespace_span(text)),
            ),
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("prime-lang".into()),
            message: format!("Header declares `{actual}` but manifest lists `{expected}`"),
            ..Default::default()
        },
    }
}

fn module_visibility_label(vis: ModuleVisibility) -> &'static str {
    match vis {
        ModuleVisibility::Public => "pub",
        ModuleVisibility::Package => "package",
        ModuleVisibility::Private => "private",
    }
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

fn type_diagnostics(
    text: &str,
    module: &Module,
    manifest: Option<&PackageManifest>,
    current_path: Option<&Path>,
) -> Vec<Diagnostic> {
    let module_path = module.path.clone();
    let program = if let Some(manifest) = manifest {
        build_program_for_manifest(module, manifest, current_path)
    } else {
        Program {
            modules: vec![module.clone()],
        }
    };
    let expanded = match macro_expander::expand_program(&program) {
        Ok(prog) => prog,
        Err(errors) => return macro_errors_to_lsp(errors, text, &module.path),
    };
    match check_program(&expanded) {
        Ok(_) => Vec::new(),
        Err(errors) => errors
            .into_iter()
            .filter(|err| err.path == module_path)
            .map(|err| type_error_to_lsp(text, err))
            .collect(),
    }
}

fn type_error_to_lsp(text: &str, err: TypeError) -> Diagnostic {
    let span = if err.span.start == err.span.end {
        let fallback = adjust_zero_length_offset(text, err.span.start);
        Span::new(fallback, fallback.saturating_add(1).min(text.len()))
    } else {
        err.span
    };
    let mut message = err.display_message();
    if let Some(help) = &err.help {
        if !help.is_empty() {
            message.push_str("\n\n");
            message.push_str(help);
        }
    }
    Diagnostic {
        range: span_to_range(text, span),
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("prime-lang".into()),
        message,
        code: err.code.clone().map(NumberOrString::String),
        ..Default::default()
    }
}

fn macro_errors_to_lsp(
    errors: Vec<macro_expander::MacroExpansionError>,
    text: &str,
    module_path: &Path,
) -> Vec<Diagnostic> {
    errors
        .into_iter()
        .filter(|err| err.path == module_path)
        .flat_map(|err| err.errors)
        .map(|err| Diagnostic {
            range: span_to_range(text, err.span),
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("prime-lang".into()),
            message: err.message,
            ..Default::default()
        })
        .collect()
}

fn build_program_for_manifest(
    current: &Module,
    manifest: &PackageManifest,
    current_path: Option<&Path>,
) -> Program {
    let mut modules = Vec::new();
    let mut queue = VecDeque::new();
    let mut scheduled = HashSet::new();
    let mut visited = HashSet::new();
    let initial_path = current_path
        .map(|path| path.to_path_buf())
        .or_else(|| manifest.module_path(&current.name))
        .unwrap_or_else(|| current.path.clone());
    let canonical_initial = canonical_path(&initial_path);
    queue.push_back((current.clone(), canonical_initial.clone()));
    scheduled.insert(canonical_initial.clone());

    while let Some((module, module_path)) = queue.pop_front() {
        if !visited.insert(module_path.clone()) {
            continue;
        }
        for import in &module.imports {
            if let Some(resolved) =
                resolve_import_for_manifest(manifest, &module_path, &import.path)
            {
                if visited.contains(&resolved.canonical_path)
                    || scheduled.contains(&resolved.canonical_path)
                {
                    continue;
                }
                if let Ok(source) = fs::read_to_string(&resolved.path) {
                    if let Ok(parsed) = parse_module(
                        &resolved.module_name,
                        resolved.canonical_path.clone(),
                        &source,
                    ) {
                        scheduled.insert(resolved.canonical_path.clone());
                        queue.push_back((parsed, resolved.canonical_path));
                    }
                }
            }
        }
        modules.push(module);
    }

    Program { modules }
}

struct ResolvedImport {
    module_name: String,
    path: PathBuf,
    canonical_path: PathBuf,
}

fn resolve_import_for_manifest(
    manifest: &PackageManifest,
    base_path: &Path,
    import_path: &ImportPath,
) -> Option<ResolvedImport> {
    let module_name = import_path.to_string();
    if let Some(path) = manifest.module_path(&module_name) {
        return Some(ResolvedImport {
            module_name,
            canonical_path: path.clone(),
            path,
        });
    }
    let relative = resolve_relative_import_path(base_path, import_path);
    let canonical = canonical_path(&relative);
    Some(ResolvedImport {
        module_name: manifest
            .module_name_for_path(&canonical)
            .unwrap_or(module_name),
        path: relative,
        canonical_path: canonical,
    })
}

fn resolve_relative_import_path(base: &Path, import_path: &ImportPath) -> PathBuf {
    let mut path = if import_path.segments.is_empty() {
        PathBuf::new()
    } else {
        import_path.to_relative_path()
    };
    if path.extension().is_none() {
        path.set_extension("prime");
    }
    if path.is_absolute() {
        path
    } else {
        base.parent()
            .map(|dir| dir.join(&path))
            .unwrap_or_else(|| PathBuf::from(".").join(&path))
    }
}

fn canonical_path(path: &Path) -> PathBuf {
    path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
}

#[cfg(test)]
mod tests {
    use super::collect_parse_and_manifest_diagnostics;
    use std::fs;
    use tempfile::tempdir;
    use tower_lsp_server::{UriExt, lsp_types::Uri};

    #[test]
    fn reports_syntax_errors_for_missing_semicolon() {
        let dir = tempdir().expect("tempdir");
        let file_path = dir.path().join("main.prime");
        fs::write(&file_path, "").expect("write file");
        let uri = Uri::from_file_path(&file_path).expect("uri");
        let text = "module test::main;\nfn main() {\n  let value = 42\n  out(value);\n}\n";
        let (_module, diags) = collect_parse_and_manifest_diagnostics(&uri, text);
        assert!(
            diags
                .iter()
                .any(|diag| diag.message.contains("Expected") || diag.severity.is_some()),
            "expected at least one diagnostic for syntax error, found {diags:?}"
        );
    }

    #[test]
    fn reports_missing_semicolon_between_statements() {
        let dir = tempdir().expect("tempdir");
        let file_path = dir.path().join("main.prime");
        fs::write(&file_path, "").expect("write file");
        let uri = Uri::from_file_path(&file_path).expect("uri");
        let text = "module test::main;\nfn main() {\n  out(1)\n  out(2);\n}\n";
        let (_module, diags) = collect_parse_and_manifest_diagnostics(&uri, text);
        assert!(
            diags
                .iter()
                .any(|diag| diag.message.contains("Expected") || diag.severity.is_some()),
            "expected diagnostics for missing semi between statements, found {diags:?}"
        );
    }

    #[test]
    fn reports_missing_semicolon_in_module_declaration() {
        let dir = tempdir().expect("tempdir");
        let file_path = dir.path().join("main.prime");
        fs::write(&file_path, "").expect("write file");
        let uri = Uri::from_file_path(&file_path).expect("uri");
        let text = "module test::main\nfn main() {}\n";
        let (_module, diags) = collect_parse_and_manifest_diagnostics(&uri, text);
        assert!(
            diags
                .iter()
                .any(|diag| diag.message.contains("Expected Semicolon")),
            "expected module header semicolon diagnostic, found {diags:?}"
        );
    }

    #[test]
    fn allows_mutable_destructuring_without_diagnostics() {
        let dir = tempdir().expect("tempdir");
        let file_path = dir.path().join("main.prime");
        fs::write(&file_path, "").expect("write file");
        let uri = Uri::from_file_path(&file_path).expect("uri");
        let text = r#"
module test::main;

fn main() {
  let mut (left, right) = (1, 2);
  let mut #{ "hp": hp, "mp": mp } = #{
    "hp": 10,
    "mp": 5,
  };
  out(left + right + hp + mp);
}
"#;
        let (_module, diags) = collect_parse_and_manifest_diagnostics(&uri, text);
        assert!(
            diags.is_empty(),
            "expected mutable destructuring parse to succeed without diagnostics, found {diags:?}"
        );
    }

    #[test]
    fn reports_type_errors_from_checker() {
        let dir = tempdir().expect("tempdir");
        let file_path = dir.path().join("main.prime");
        fs::write(&file_path, "").expect("write file");
        let uri = Uri::from_file_path(&file_path).expect("uri");
        let text = r#"
module test::main;

fn main() {
  let value: int32 = "oops";
}
"#;
        let (_module, diags) = collect_parse_and_manifest_diagnostics(&uri, text);
        assert!(
            diags
                .iter()
                .any(|diag| diag.message.contains("expected `int32`")),
            "expected type error diagnostic, found {diags:?}"
        );
    }

    #[test]
    fn reports_borrow_errors_from_checker() {
        let dir = tempdir().expect("tempdir");
        let file_path = dir.path().join("main.prime");
        fs::write(&file_path, "").expect("write file");
        let uri = Uri::from_file_path(&file_path).expect("uri");
        let text = r#"
module test::main;

fn main() {
  let mut int32 value = 0;
  let &mut int32 alias = &mut value;
  let &mut int32 second = &mut value;
  *second = 1;
}
"#;
        let (_module, diags) = collect_parse_and_manifest_diagnostics(&uri, text);
        assert!(
            diags
                .iter()
                .any(|diag| diag.message.contains("already mutably borrowed")),
            "expected borrow-checker diagnostic, found {diags:?}"
        );
    }
}
