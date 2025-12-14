use super::{
    analysis::{
        collect_identifier_spans as collect_identifier_spans_ast,
        collect_identifier_spans_for_decl, find_local_decl, find_local_definition_span,
        find_module_item_span, identifier_at_offset, unused_variable_diagnostics,
    },
    completion::{
        ModulePathCompletionKind, collect_interface_info, collect_struct_info, completion_prefix,
        completion_trigger_characters, enum_variant_completion_items, expression_chain_before_dot,
        format_function_param, format_function_signature, general_completion_items,
        keyword_completion_items, member_completion_items, module_completion_items_from_manifest,
        module_path_completion_context, module_selector_items_from_modules,
    },
    diagnostics::{collect_parse_and_manifest_diagnostics, diagnostic_code, manifest_entry_action},
    hover::{collect_var_infos, hover_for_token},
    parser::parse_module_from_uri,
    text::{
        collect_identifier_spans, full_range, identifier_at, is_valid_identifier,
        manifest_context_for_uri, position_to_offset, prefix_identifier, span_to_range, token_at,
        url_to_path,
    },
};
use crate::project::{
    diagnostics::{CODE_MANIFEST_MISSING_MODULE, CODE_MISSING_MODULE_HEADER},
    find_manifest, load_package,
    manifest::PackageManifest,
};
use crate::target::BuildOptions;
use crate::{
    language::{
        ast::{Item, Module, ModuleKind, Visibility},
        lexer::lex,
        parser::parse_module,
        span::Span,
        token::{Token, TokenKind},
    },
    tools::formatter::format_module,
};
use serde_json::{Value, json};
use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
};
use tokio::sync::RwLock;
use tower_lsp_server::jsonrpc::Result as RpcResult;
use tower_lsp_server::ls_types::request::{
    GotoDeclarationParams, GotoDeclarationResponse, GotoImplementationParams,
    GotoTypeDefinitionParams,
};
use tower_lsp_server::ls_types::{
    CodeAction, CodeActionKind, CodeActionOptions, CodeActionOrCommand, CodeActionParams,
    CodeActionProviderCapability, CodeActionResponse, CodeLens, CodeLensOptions, CodeLensParams,
    Command, CompletionOptions, CompletionParams, CompletionResponse, DeclarationCapability,
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams,
    DocumentSymbolParams, DocumentSymbolResponse, ExecuteCommandOptions, ExecuteCommandParams,
    GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverParams, HoverProviderCapability,
    ImplementationProviderCapability, InitializeParams, InitializeResult, InitializedParams,
    Location, MessageType, OneOf, ParameterInformation, ParameterLabel, Position,
    PrepareRenameResponse, Range, ReferenceParams, RenameParams, ServerCapabilities, SignatureHelp,
    SignatureHelpOptions, SignatureHelpParams, SignatureInformation, SymbolInformation, SymbolKind,
    TextDocumentPositionParams, TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit,
    TypeDefinitionProviderCapability, Uri, WorkspaceEdit, WorkspaceSymbolParams,
    WorkspaceSymbolResponse,
};
use tower_lsp_server::{Client, LanguageServer};

mod server;
mod signature;
mod state;
mod symbols;

use signature::{call_context, signature_help_from_modules};
use state::{Documents, ModuleCache, SymbolIndex, select_symbol_location};
use symbols::collect_symbols;

pub struct Backend {
    client: Client,
    docs: Arc<Documents>,
    modules: Arc<ModuleCache>,
    symbols: Arc<SymbolIndex>,
    package_preloaded: Arc<RwLock<HashSet<PathBuf>>>,
    workspace_manifests: Arc<RwLock<HashSet<PathBuf>>>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            docs: Arc::new(Documents::default()),
            modules: Arc::new(ModuleCache::default()),
            symbols: Arc::new(SymbolIndex::default()),
            package_preloaded: Arc::new(RwLock::new(HashSet::new())),
            workspace_manifests: Arc::new(RwLock::new(HashSet::new())),
        }
    }

    async fn actions_for_diagnostic(
        &self,
        uri: &Uri,
        text: &str,
        diagnostic: &Diagnostic,
    ) -> RpcResult<Vec<CodeActionOrCommand>> {
        let mut actions = Vec::new();
        if let Some(name) = unused_variable_name(&diagnostic.message) {
            let mut edit_map = HashMap::new();
            edit_map.insert(
                uri.clone(),
                vec![TextEdit {
                    range: diagnostic.range,
                    new_text: prefix_identifier(text, &diagnostic.range),
                }],
            );
            actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                title: format!("Prefix `{name}` with `_`"),
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: Some(vec![diagnostic.clone()]),
                is_preferred: Some(true),
                edit: Some(WorkspaceEdit {
                    changes: Some(edit_map),
                    ..Default::default()
                }),
                ..Default::default()
            }));
        }

        if let Some(code) = diagnostic_code(diagnostic) {
            match code {
                CODE_MISSING_MODULE_HEADER => {
                    if let Some(module_name) = diagnostic
                        .data
                        .as_ref()
                        .and_then(|data| data.get("module_name"))
                        .and_then(|val| val.as_str())
                    {
                        let edit = TextEdit {
                            range: Range::new(Position::new(0, 0), Position::new(0, 0)),
                            new_text: format!("module {module_name};\n\n"),
                        };
                        actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                            title: format!("Insert `module {module_name};` header"),
                            kind: Some(CodeActionKind::QUICKFIX),
                            diagnostics: Some(vec![diagnostic.clone()]),
                            edit: Some(WorkspaceEdit {
                                changes: Some([(uri.clone(), vec![edit])].into_iter().collect()),
                                ..Default::default()
                            }),
                            ..Default::default()
                        }));
                    }
                }
                CODE_MANIFEST_MISSING_MODULE => {
                    if let Some(action) =
                        manifest_entry_action(diagnostic).and_then(|entry| entry.into_code_action())
                    {
                        actions.push(action);
                    }
                }
                _ => {}
            }
        }

        Ok(actions)
    }

    async fn publish_diagnostics(&self, uri: &Uri) {
        let Some(text) = self.docs.get(uri).await else {
            return;
        };
        let (module, mut diagnostics) = collect_parse_and_manifest_diagnostics(uri, &text);
        if let Some(module) = module {
            diagnostics.extend(unused_variable_diagnostics(&module, &text));
            for (name, span, others) in self.symbols.duplicates_for_uri(uri).await {
                let elsewhere = others
                    .iter()
                    .filter_map(|u| url_to_path(u).map(|p| p.display().to_string()))
                    .collect::<Vec<_>>()
                    .join(", ");
                diagnostics.push(Diagnostic {
                    range: span_to_range(&text, span),
                    severity: Some(DiagnosticSeverity::WARNING),
                    source: Some("prime-lang".into()),
                    message: format!("`{}` is defined in multiple files: {}", name, elsewhere),
                    ..Default::default()
                });
            }
        }
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }

    async fn format_document(&self, uri: &Uri, text: &str) -> Option<String> {
        let path = url_to_path(uri)?;
        let module_name = path.file_stem()?.to_str()?.to_string();
        let module = parse_module(&module_name, path, text).ok()?;
        Some(format_module(&module))
    }

    async fn parse_cached_module(&self, uri: &Uri, text: &str) -> Option<Module> {
        let manifest_context = manifest_context_for_uri(uri);
        if let Some((manifest, _)) = manifest_context.as_ref() {
            self.ensure_manifest_modules(manifest).await;
        }
        let file_path = manifest_context
            .as_ref()
            .map(|(_, path)| path.clone())
            .or_else(|| url_to_path(uri));
        if let Some(path) = file_path.as_ref() {
            self.ensure_package_modules(path).await;
        }
        match parse_module_from_uri(uri, text) {
            Ok(module) => {
                self.modules.insert(uri.clone(), module.clone()).await;
                self.symbols.update_module(uri, &module).await;
                if let Some((manifest, _)) = manifest_context.as_ref() {
                    self.ensure_import_modules(manifest, &module).await;
                }
                Some(module)
            }
            Err(_) => self.modules.get(uri).await,
        }
    }

    async fn text_for_uri(&self, uri: &Uri) -> Option<String> {
        if let Some(text) = self.docs.get(uri).await {
            return Some(text);
        }
        let path = url_to_path(uri)?;
        fs::read_to_string(path).ok()
    }

    async fn ensure_manifest_modules(&self, manifest: &PackageManifest) {
        for entry in manifest.module_entries() {
            let Some(uri) = Uri::from_file_path(&entry.path) else {
                continue;
            };
            if self.modules.get(&uri).await.is_some() {
                continue;
            }
            let source = match fs::read_to_string(&entry.path) {
                Ok(src) => src,
                Err(err) => {
                    let message = format!(
                        "prime-lang: failed to read module `{}` at `{}`: {}",
                        entry.name,
                        entry.path.display(),
                        err,
                    );
                    let _ = self.client.log_message(MessageType::WARNING, message).await;
                    continue;
                }
            };
            let module = match parse_module(&entry.name, entry.path.clone(), &source) {
                Ok(module) => module,
                Err(errs) => {
                    let message = format!(
                        "prime-lang: failed to parse module `{}` at `{}`: {} error(s)",
                        entry.name,
                        entry.path.display(),
                        errs.errors.len()
                    );
                    let _ = self.client.log_message(MessageType::WARNING, message).await;
                    continue;
                }
            };
            self.modules.insert(uri.clone(), module.clone()).await;
            self.symbols.update_module(&uri, &module).await;
        }
    }

    async fn ensure_import_modules(&self, manifest: &PackageManifest, module: &Module) {
        for import in &module.imports {
            let module_name = import.path.to_string();
            let Some(path) = manifest.module_path(&module_name) else {
                continue;
            };
            let Some(uri) = Uri::from_file_path(&path) else {
                continue;
            };
            if self.modules.get(&uri).await.is_some() {
                continue;
            }
            let source = match fs::read_to_string(&path) {
                Ok(src) => src,
                Err(_) => continue,
            };
            let parsed = match parse_module(&module_name, path.clone(), &source) {
                Ok(module) => module,
                Err(_) => continue,
            };
            self.modules.insert(uri.clone(), parsed.clone()).await;
            self.symbols.update_module(&uri, &parsed).await;
        }
    }

    async fn ensure_package_modules(&self, path: &Path) {
        let Ok(canonical) = path.canonicalize() else {
            return;
        };
        {
            let guard = self.package_preloaded.read().await;
            if guard.contains(&canonical) {
                return;
            }
        }
        {
            let mut guard = self.package_preloaded.write().await;
            if !guard.insert(canonical.clone()) {
                return;
            }
        }
        let package = match load_package(&canonical) {
            Ok(pkg) => pkg,
            Err(err) => {
                let message = format!(
                    "prime-lang: failed to load package at `{}`: {:?}",
                    canonical.display(),
                    err
                );
                let _ = self.client.log_message(MessageType::WARNING, message).await;
                return;
            }
        };
        for unit in package.modules {
            if let Some(uri) = Uri::from_file_path(&unit.module.path) {
                self.modules.insert(uri.clone(), unit.module.clone()).await;
                self.symbols.update_module(&uri, &unit.module).await;
            }
        }
    }

    async fn preload_workspace_manifest(&self, root: &Path) {
        let manifest_path = if root.join("prime.toml").exists() {
            root.join("prime.toml")
        } else {
            match find_manifest(root) {
                Some(path) => path,
                None => return,
            }
        };
        {
            let guard = self.workspace_manifests.read().await;
            if guard.contains(&manifest_path) {
                return;
            }
        }
        {
            let mut guard = self.workspace_manifests.write().await;
            if !guard.insert(manifest_path.clone()) {
                return;
            }
        }
        let Ok(manifest) = PackageManifest::load(&manifest_path) else {
            return;
        };
        self.ensure_manifest_modules(&manifest).await;
    }

    async fn preload_for_navigation(&self, uri: &Uri) {
        if let Some((manifest, path)) = manifest_context_for_uri(uri) {
            self.ensure_manifest_modules(&manifest).await;
            self.ensure_package_modules(&path).await;
        } else if let Some(path) = url_to_path(uri) {
            self.ensure_package_modules(&path).await;
        }
    }

    async fn preload_workspace(&self) {
        let manifest_paths: Vec<PathBuf> = {
            let guard = self.workspace_manifests.read().await;
            guard.iter().cloned().collect()
        };
        for manifest_path in manifest_paths {
            if let Ok(manifest) = PackageManifest::load(&manifest_path) {
                self.ensure_manifest_modules(&manifest).await;
            }
        }
    }

    async fn resolve_symbol_location(&self, uri: &Uri, position: Position) -> Option<Location> {
        let text = self.docs.get(uri).await?;
        let tokens = lex(&text).ok()?;
        let module = self.parse_cached_module(uri, &text).await;
        let offset = position_to_offset(&text, position);
        let name = if let Some((name, _)) = identifier_at(&tokens, offset) {
            name
        } else if let Some(module) = module.as_ref() {
            if let Some((name, _)) = identifier_at_offset(module, offset) {
                name
            } else {
                return None;
            }
        } else {
            return None;
        };
        if let Some(module) = module.as_ref() {
            if let Some(loc) = self.import_definition_location(module, uri, offset).await {
                return Some(loc);
            }
            if let Some(span) = find_local_definition_span(module, &name, offset) {
                return Some(Location::new(uri.clone(), span_to_range(&text, span)));
            }
            if let Some(span) = find_module_item_span(module, &name) {
                return Some(Location::new(uri.clone(), span_to_range(&text, span)));
            }
        }
        self.preload_for_navigation(uri).await;
        let candidates = self.symbols.lookup(&name).await;
        let symbol = select_symbol_location(uri, module.as_ref(), &candidates)?;
        let range = if symbol.uri == *uri {
            span_to_range(&text, symbol.span)
        } else if let Some(def_text) = self.text_for_uri(&symbol.uri).await {
            span_to_range(&def_text, symbol.span)
        } else {
            return None;
        };
        Some(Location::new(symbol.uri.clone(), range))
    }

    async fn import_definition_location(
        &self,
        module: &Module,
        uri: &Uri,
        offset: usize,
    ) -> Option<Location> {
        let (manifest, _) = manifest_context_for_uri(uri)?;
        for import in &module.imports {
            if offset < import.span.start || offset > import.span.end {
                continue;
            }
            let target = import.path.to_string();
            let Some(path) = manifest.module_path(&target) else {
                continue;
            };
            let Some(target_uri) = Uri::from_file_path(&path) else {
                continue;
            };
            let target_text = fs::read_to_string(&path).ok()?;
            let target_module = self
                .parse_cached_module(&target_uri, &target_text)
                .await
                .unwrap_or_else(|| {
                    parse_module(&target, path.clone(), &target_text)
                        .unwrap_or_else(|_| module.clone())
                });
            let span = target_module
                .declared_span
                .unwrap_or_else(|| Span::new(0, target_text.len().min(1)));
            return Some(Location::new(target_uri, span_to_range(&target_text, span)));
        }
        None
    }

    async fn resolve_type_location(&self, uri: &Uri, position: Position) -> Option<Location> {
        let text = self.docs.get(uri).await?;
        let tokens = lex(&text).ok()?;
        let module = self.parse_cached_module(uri, &text).await?;
        let offset = position_to_offset(&text, position);
        let (name, _) = identifier_at(&tokens, offset)?;
        // prefer local type defs (struct/enum/interface)
        for item in &module.items {
            match item {
                Item::Struct(def) if def.name == name => {
                    return Some(Location::new(uri.clone(), span_to_range(&text, def.span)));
                }
                Item::Enum(def) if def.name == name => {
                    return Some(Location::new(uri.clone(), span_to_range(&text, def.span)));
                }
                Item::Interface(def) if def.name == name => {
                    return Some(Location::new(uri.clone(), span_to_range(&text, def.span)));
                }
                _ => {}
            }
        }
        self.preload_for_navigation(uri).await;
        for (other_uri, other_module) in self.modules.snapshot().await {
            for item in &other_module.items {
                let span = match item {
                    Item::Struct(def) if def.name == name => def.span,
                    Item::Enum(def) if def.name == name => def.span,
                    Item::Interface(def) if def.name == name => def.span,
                    _ => continue,
                };
                let def_text = self.text_for_uri(&other_uri).await?;
                return Some(Location::new(
                    other_uri.clone(),
                    span_to_range(&def_text, span),
                ));
            }
        }
        None
    }

    async fn resolve_impl_location(&self, uri: &Uri, position: Position) -> Option<Location> {
        let text = self.docs.get(uri).await?;
        let tokens = lex(&text).ok()?;
        let module = self.parse_cached_module(uri, &text).await?;
        let offset = position_to_offset(&text, position);
        let (name, _) = identifier_at(&tokens, offset)?;
        for item in &module.items {
            if let Item::Impl(block) = item {
                if block.target == name {
                    if let Some(method) = block.methods.first() {
                        return Some(Location::new(
                            uri.clone(),
                            span_to_range(&text, method.span),
                        ));
                    }
                }
            }
        }
        self.preload_for_navigation(uri).await;
        for (other_uri, other_module) in self.modules.snapshot().await {
            for item in &other_module.items {
                if let Item::Impl(block) = item {
                    if block.target == name {
                        if let Some(method) = block.methods.first() {
                            let def_text = self.text_for_uri(&other_uri).await?;
                            return Some(Location::new(
                                other_uri.clone(),
                                span_to_range(&def_text, method.span),
                            ));
                        }
                    }
                }
            }
        }
        None
    }
}

fn unused_variable_name(message: &str) -> Option<String> {
    let prefix = "Variable `";
    let suffix = "` is never used";
    if message.starts_with(prefix) && message.ends_with(suffix) {
        let inner = &message[prefix.len()..message.len() - suffix.len()];
        if !inner.is_empty() {
            return Some(inner.to_string());
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::language::parser::parse_module;
    use std::path::PathBuf;

    #[test]
    fn signature_help_tracks_active_parameter_for_nested_calls() {
        let src = r#"
module demo::sig;

fn add(a: int32, b: int32) -> int32 { a + b }

fn main() {
  add(1, add(2, 3));
}
"#;
        let module =
            parse_module("demo::sig", PathBuf::from("sig.prime"), src).expect("parse module");
        let modules = vec![module];

        let inner_offset = src.find("add(2, 3").expect("inner call offset") + "add(2".len();
        let inner_ctx = call_context(src, inner_offset).expect("call context for inner call");
        assert_eq!(inner_ctx.name, "add");
        assert_eq!(inner_ctx.arg_index, 0);
        let inner_help =
            signature_help_from_modules(&modules, &inner_ctx).expect("signature help for inner");
        assert_eq!(inner_help.active_parameter, Some(0));
        assert_eq!(
            inner_help.signatures[0]
                .parameters
                .as_ref()
                .map(|p| p.len()),
            Some(2)
        );

        let outer_offset =
            src.find("add(1, add(2, 3)").expect("outer call offset") + "add(1, add(2, 3)".len();
        let outer_ctx = call_context(src, outer_offset).expect("call context for outer call");
        assert_eq!(outer_ctx.name, "add");
        assert_eq!(outer_ctx.arg_index, 1);
        let outer_help =
            signature_help_from_modules(&modules, &outer_ctx).expect("signature help for outer");
        assert_eq!(outer_help.active_parameter, Some(1));
    }
}
