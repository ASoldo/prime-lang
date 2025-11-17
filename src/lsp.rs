use crate::project::{
    find_manifest, load_package,
    manifest::{ModuleVisibility, PackageManifest},
};
use crate::{
    formatter::format_module,
    language::{
        ast::{
            Block, ConstDef, EnumDef, EnumVariant, Expr, FunctionBody, FunctionDef, FunctionParam,
            ImportPath, InterfaceDef, InterfaceMethod, Item, LetStmt, Literal, Module, Pattern,
            RangeExpr, Statement, StructDef, StructLiteralKind, Visibility,
        },
        errors::{SyntaxError, SyntaxErrors},
        lexer::{LexError, lex},
        parser::parse_module,
        span::Span,
        token::{Token, TokenKind},
        types::{Mutability, TypeExpr},
    },
};
use serde_json::{Value, json};
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    error::Error,
    fs,
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
};
use tokio::runtime::Runtime;
use tokio::sync::RwLock;
use tower_lsp_server::jsonrpc::Result as RpcResult;
use tower_lsp_server::lsp_types::{
    CodeAction, CodeActionKind, CodeActionOptions, CodeActionOrCommand, CodeActionParams,
    CodeActionProviderCapability, CodeActionResponse, CodeLens, CodeLensOptions, CodeLensParams,
    Command, CompletionContext, CompletionItem, CompletionItemKind, CompletionOptions,
    CompletionParams, CompletionResponse, CompletionTextEdit, Diagnostic, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, DocumentFormattingParams, DocumentSymbolParams,
    DocumentSymbolResponse, ExecuteCommandOptions, ExecuteCommandParams, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverContents, HoverParams, HoverProviderCapability,
    InitializeParams, InitializeResult, InitializedParams, Location, MarkupContent, MarkupKind,
    MessageType, NumberOrString, OneOf, ParameterInformation, ParameterLabel, Position,
    PrepareRenameResponse, Range, ReferenceParams, RenameParams, ServerCapabilities, SignatureHelp,
    SignatureHelpOptions, SignatureHelpParams, SignatureInformation, SymbolInformation, SymbolKind,
    TextDocumentPositionParams, TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Uri,
    WorkspaceEdit, WorkspaceSymbol, WorkspaceSymbolParams,
};
use tower_lsp_server::{Client, LanguageServer, LspService, Server, UriExt};

pub fn serve_stdio() -> Result<(), Box<dyn Error + Send + Sync>> {
    let runtime = Runtime::new()?;
    runtime.block_on(async {
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();
        let (service, socket) = LspService::new(|client| Backend::new(client));
        Server::new(stdin, stdout, socket).serve(service).await;
        Ok(())
    })
}

#[derive(Default)]
struct Documents {
    inner: RwLock<HashMap<Uri, String>>,
}

impl Documents {
    async fn insert(&self, uri: Uri, text: String) {
        self.inner.write().await.insert(uri, text);
    }

    async fn remove(&self, uri: &Uri) {
        self.inner.write().await.remove(uri);
    }

    async fn get(&self, uri: &Uri) -> Option<String> {
        self.inner.read().await.get(uri).cloned()
    }
}

fn select_interface_info<'a>(
    interfaces: &'a HashMap<String, Vec<InterfaceInfo>>,
    name: &str,
    module_name: &str,
) -> Option<&'a InterfaceInfo> {
    let list = interfaces.get(name)?;
    list.iter()
        .find(|info| info.module_name == module_name)
        .or_else(|| list.first())
}

#[derive(Default)]
struct ModuleCache {
    inner: RwLock<HashMap<Uri, Module>>,
}

impl ModuleCache {
    async fn insert(&self, uri: Uri, module: Module) {
        self.inner.write().await.insert(uri, module);
    }

    async fn get(&self, uri: &Uri) -> Option<Module> {
        self.inner.read().await.get(uri).cloned()
    }

    async fn remove(&self, uri: &Uri) {
        self.inner.write().await.remove(uri);
    }

    async fn snapshot(&self) -> Vec<(Uri, Module)> {
        self.inner
            .read()
            .await
            .iter()
            .map(|(uri, module)| (uri.clone(), module.clone()))
            .collect()
    }
}

#[derive(Clone)]
struct SymbolLocation {
    uri: Uri,
    span: Span,
    kind: SymbolKind,
    module_name: String,
    visibility: Visibility,
}

#[derive(Default)]
struct SymbolIndex {
    inner: RwLock<HashMap<String, Vec<SymbolLocation>>>,
}

const CODE_MISSING_MODULE_HEADER: &str = "prime.missingModuleHeader";
const CODE_MANIFEST_MISSING_MODULE: &str = "prime.manifestMissingModule";
const CODE_DUPLICATE_IMPORT: &str = "prime.duplicateImport";
const CODE_UNKNOWN_IMPORT: &str = "prime.unknownImport";

impl SymbolIndex {
    async fn update_module(&self, uri: &Uri, module: &Module) {
        let mut map = self.inner.write().await;
        for locations in map.values_mut() {
            locations.retain(|loc| &loc.uri != uri);
        }
        map.retain(|_, locations| !locations.is_empty());
        for (name, span, kind, visibility) in module_symbol_definitions(module) {
            map.entry(name).or_default().push(SymbolLocation {
                uri: uri.clone(),
                span,
                kind,
                module_name: module.name.clone(),
                visibility,
            });
        }
    }

    async fn remove_module(&self, uri: &Uri) {
        let mut map = self.inner.write().await;
        map.retain(|_, locations| {
            locations.retain(|loc| &loc.uri != uri);
            !locations.is_empty()
        });
    }

    async fn lookup(&self, name: &str) -> Vec<SymbolLocation> {
        self.inner
            .read()
            .await
            .get(name)
            .cloned()
            .unwrap_or_default()
    }

    async fn search(&self, query: &str) -> Vec<(String, SymbolLocation)> {
        let needle = query.to_lowercase();
        self.inner
            .read()
            .await
            .iter()
            .filter(|(name, _)| name.to_lowercase().contains(&needle))
            .flat_map(|(name, locs)| locs.iter().cloned().map(move |loc| (name.clone(), loc)))
            .collect()
    }
}

struct Backend {
    client: Client,
    docs: Arc<Documents>,
    modules: Arc<ModuleCache>,
    symbols: Arc<SymbolIndex>,
    package_preloaded: Arc<RwLock<HashSet<PathBuf>>>,
    workspace_manifests: Arc<RwLock<HashSet<PathBuf>>>,
}

impl Backend {
    fn new(client: Client) -> Self {
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
                        manifest_entry_action(diagnostic).and_then(|entry| entry.to_code_action())
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
        let diagnostics = collect_diagnostics(uri, &text);
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
}

impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> RpcResult<InitializeResult> {
        if let Some(root_uri) = params.root_uri {
            if let Some(root_path) = url_to_path(&root_uri) {
                self.preload_workspace_manifest(&root_path).await;
            }
        }
        let completion_trigger_characters = completion_trigger_characters();
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                code_action_provider: Some(CodeActionProviderCapability::Options(
                    CodeActionOptions {
                        code_action_kinds: Some(vec![
                            CodeActionKind::QUICKFIX,
                            CodeActionKind::SOURCE,
                        ]),
                        ..Default::default()
                    },
                )),
                code_lens_provider: Some(CodeLensOptions {
                    resolve_provider: Some(false),
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".into(), ",".into()]),
                    retrigger_characters: Some(vec![",".into()]),
                    ..Default::default()
                }),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(completion_trigger_characters),
                    ..Default::default()
                }),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["prime.showReferences".into()],
                    work_done_progress_options: Default::default(),
                }),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        let _ = self
            .client
            .log_message(MessageType::INFO, "prime-lang LSP ready")
            .await;
    }

    async fn shutdown(&self) -> RpcResult<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        self.docs.insert(uri.clone(), text.clone()).await;
        let _ = self.parse_cached_module(&uri, &text).await;
        self.publish_diagnostics(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.last() {
            let uri = params.text_document.uri.clone();
            let text = change.text.clone();
            self.docs.insert(uri.clone(), text.clone()).await;
            let _ = self.parse_cached_module(&uri, &text).await;
            self.publish_diagnostics(&uri).await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if let Some(text) = params.text {
            let uri = params.text_document.uri.clone();
            let text_clone = text.clone();
            self.docs.insert(uri.clone(), text_clone.clone()).await;
            let _ = self.parse_cached_module(&uri, &text_clone).await;
            self.publish_diagnostics(&uri).await;
        } else {
            let uri = params.text_document.uri.clone();
            if let Some(current) = self.docs.get(&uri).await {
                let _ = self.parse_cached_module(&uri, &current).await;
            }
            self.publish_diagnostics(&uri).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.docs.remove(&params.text_document.uri).await;
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn hover(&self, params: HoverParams) -> RpcResult<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some(text) = self.docs.get(&uri).await else {
            return Ok(None);
        };
        let tokens = match lex(&text) {
            Ok(tokens) => tokens,
            Err(_) => return Ok(None),
        };
        self.preload_for_navigation(&uri).await;
        let module = self.parse_cached_module(&uri, &text).await;
        let struct_modules = self
            .modules
            .snapshot()
            .await
            .into_iter()
            .map(|(_, module)| module)
            .collect::<Vec<_>>();
        let struct_info_map = collect_struct_info(&struct_modules);
        let struct_info = if struct_info_map.is_empty() {
            None
        } else {
            Some(struct_info_map)
        };
        let vars = collect_var_infos(&text, &tokens);
        let offset = position_to_offset(&text, position);
        if let Some(token) = token_at(&tokens, offset) {
            if let Some(hover) =
                hover_for_token(&text, token, &vars, module.as_ref(), struct_info.as_ref())
            {
                return Ok(Some(hover));
            }
        }
        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> RpcResult<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some(text) = self.docs.get(&uri).await else {
            return Ok(None);
        };
        let tokens = match lex(&text) {
            Ok(tokens) => tokens,
            Err(_) => return Ok(None),
        };
        let module = self.parse_cached_module(&uri, &text).await;
        let offset = position_to_offset(&text, position);
        let Some((name, _)) = identifier_at(&tokens, offset) else {
            return Ok(None);
        };
        if let Some(module) = module.as_ref() {
            if let Some(span) = find_local_definition_span(module, &name, offset) {
                let location = Location::new(uri.clone(), span_to_range(&text, span));
                return Ok(Some(GotoDefinitionResponse::Scalar(location)));
            }
            if let Some(span) = find_module_item_span(module, &name) {
                let location = Location::new(uri.clone(), span_to_range(&text, span));
                return Ok(Some(GotoDefinitionResponse::Scalar(location)));
            }
        }
        self.preload_for_navigation(&uri).await;
        let candidates = self.symbols.lookup(&name).await;
        if let Some(symbol) = select_symbol_location(&uri, module.as_ref(), &candidates) {
            let range = if symbol.uri == uri {
                span_to_range(&text, symbol.span)
            } else if let Some(def_text) = self.text_for_uri(&symbol.uri).await {
                span_to_range(&def_text, symbol.span)
            } else {
                return Ok(None);
            };
            let location = Location::new(symbol.uri.clone(), range);
            return Ok(Some(GotoDefinitionResponse::Scalar(location)));
        }
        Ok(None)
    }

    async fn formatting(
        &self,
        params: DocumentFormattingParams,
    ) -> RpcResult<Option<Vec<TextEdit>>> {
        let Some(text) = self.docs.get(&params.text_document.uri).await else {
            return Ok(None);
        };
        if let Some(formatted) = self.format_document(&params.text_document.uri, &text).await {
            let range = full_range(&text);
            Ok(Some(vec![TextEdit {
                range,
                new_text: formatted,
            }]))
        } else {
            Ok(None)
        }
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> RpcResult<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;
        let Some(text) = self.docs.get(&uri).await else {
            return Ok(None);
        };
        let Some(module) = self.parse_cached_module(&uri, &text).await else {
            return Ok(None);
        };
        let symbols = collect_symbols(&uri, &text, &module);
        Ok(Some(DocumentSymbolResponse::Flat(symbols)))
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> RpcResult<Option<PrepareRenameResponse>> {
        let uri = params.text_document.uri;
        let position = params.position;
        let Some(text) = self.docs.get(&uri).await else {
            return Ok(None);
        };
        let tokens = lex(&text).ok();
        let Some(tokens) = tokens else {
            return Ok(None);
        };
        let offset = position_to_offset(&text, position);
        if let Some((name, span)) = identifier_at(&tokens, offset) {
            if !is_valid_identifier(&name) {
                return Ok(None);
            }
            let range = span_to_range(&text, span);
            return Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
                range,
                placeholder: name,
            }));
        }
        Ok(None)
    }

    async fn rename(&self, params: RenameParams) -> RpcResult<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = params.new_name;
        if new_name.trim().is_empty() || !is_valid_identifier(&new_name) {
            return Ok(None);
        }
        let Some(text) = self.docs.get(&uri).await else {
            return Ok(None);
        };
        let tokens = lex(&text).ok();
        let Some(tokens) = tokens else {
            return Ok(None);
        };
        let offset = position_to_offset(&text, position);
        let Some((target_name, _)) = identifier_at(&tokens, offset) else {
            return Ok(None);
        };
        let spans = collect_identifier_spans(&tokens, &target_name);
        if spans.is_empty() {
            return Ok(None);
        }
        let edits = spans
            .into_iter()
            .map(|span| TextEdit {
                range: span_to_range(&text, span),
                new_text: new_name.clone(),
            })
            .collect();
        Ok(Some(WorkspaceEdit {
            changes: Some([(uri, edits)].into_iter().collect()),
            ..Default::default()
        }))
    }

    async fn completion(&self, params: CompletionParams) -> RpcResult<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let context = params.context;
        let Some(text) = self.docs.get(&uri).await else {
            return Ok(None);
        };
        if let Some((manifest, _)) = manifest_context_for_uri(&uri) {
            self.ensure_manifest_modules(&manifest).await;
        } else if let Some(path) = url_to_path(&uri) {
            self.ensure_package_modules(&path).await;
        }
        let module_opt = self.parse_cached_module(&uri, &text).await;
        let offset = position_to_offset(&text, position);
        if let Some(ctx) = module_path_completion_context(&text, offset) {
            if let Some((manifest, file_path)) = manifest_context_for_uri(&uri) {
                let expected = matches!(ctx.kind, ModulePathCompletionKind::Declaration)
                    .then(|| manifest.module_name_for_path(&file_path))
                    .flatten();
                let items = module_completion_items_from_manifest(
                    &manifest,
                    ctx.prefix.as_deref(),
                    expected.as_deref(),
                );
                if !items.is_empty() {
                    return Ok(Some(CompletionResponse::Array(items)));
                }
            }
        }
        let prefix = completion_prefix(&text, offset, context.as_ref());
        let prefix_ref = prefix.as_deref();
        let struct_modules = self
            .modules
            .snapshot()
            .await
            .into_iter()
            .map(|(_, module)| module)
            .collect::<Vec<_>>();
        let struct_info = collect_struct_info(&struct_modules);
        let interface_info = collect_interface_info(&struct_modules);
        if let Some(module) = module_opt {
            if let Some(chain) = expression_chain_before_dot(&text, offset) {
                if let Some(items) = member_completion_items(
                    &text,
                    &chain,
                    &struct_info,
                    &interface_info,
                    prefix_ref,
                    &module,
                    offset,
                ) {
                    return Ok(Some(CompletionResponse::Array(items)));
                }
            }

            let general_items = general_completion_items(&module, Some(offset), prefix_ref);
            Ok(Some(CompletionResponse::Array(general_items)))
        } else {
            let keywords = keyword_completion_items(prefix_ref);
            if keywords.is_empty() {
                Ok(None)
            } else {
                Ok(Some(CompletionResponse::Array(keywords)))
            }
        }
    }
    async fn signature_help(
        &self,
        params: SignatureHelpParams,
    ) -> RpcResult<Option<SignatureHelp>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some(text) = self.docs.get(&uri).await else {
            return Ok(None);
        };
        let offset = position_to_offset(&text, position);
        let Some(ctx) = call_context(&text, offset) else {
            return Ok(None);
        };
        let modules = self.modules.snapshot().await;
        let mut signatures = Vec::new();
        for (_, module) in modules {
            for item in &module.items {
                if let Item::Function(func) = item {
                    if func.name == ctx.name {
                        let params_info = func
                            .params
                            .iter()
                            .map(|param| ParameterInformation {
                                label: ParameterLabel::Simple(format_function_param(param)),
                                documentation: None,
                            })
                            .collect();
                        signatures.push(SignatureInformation {
                            label: format_function_signature(func),
                            documentation: None,
                            parameters: Some(params_info),
                            active_parameter: None,
                        });
                    }
                }
            }
        }
        if signatures.is_empty() {
            return Ok(None);
        }
        let active_param = signatures[0]
            .parameters
            .as_ref()
            .map(|params| ctx.arg_index.min(params.len().saturating_sub(1)));
        Ok(Some(SignatureHelp {
            signatures,
            active_signature: Some(0),
            active_parameter: active_param.map(|idx| idx as u32),
        }))
    }

    async fn code_action(&self, params: CodeActionParams) -> RpcResult<Option<CodeActionResponse>> {
        let uri = params.text_document.uri.clone();
        let Some(text) = self.docs.get(&uri).await else {
            return Ok(None);
        };
        let mut actions = Vec::new();
        for diagnostic in &params.context.diagnostics {
            actions.extend(self.actions_for_diagnostic(&uri, &text, diagnostic).await?);
        }
        if actions.is_empty() {
            Ok(None)
        } else {
            Ok(Some(actions))
        }
    }

    async fn references(&self, params: ReferenceParams) -> RpcResult<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let Some(text) = self.docs.get(&uri).await else {
            return Ok(None);
        };
        let tokens = match lex(&text) {
            Ok(tokens) => tokens,
            Err(_) => return Ok(None),
        };
        let offset = position_to_offset(&text, position);
        let Some((target_name, _)) = identifier_at(&tokens, offset) else {
            return Ok(None);
        };
        self.preload_for_navigation(&uri).await;
        let mut locations = Vec::new();
        let mut seen = HashSet::new();
        for span in collect_identifier_spans(&tokens, &target_name) {
            let key = (uri.to_string(), span.start);
            if seen.insert(key) {
                locations.push(Location::new(uri.clone(), span_to_range(&text, span)));
            }
        }
        let modules = self.modules.snapshot().await;
        for (module_uri, _) in modules {
            if module_uri == uri {
                continue;
            }
            let Some(module_text) = self.text_for_uri(&module_uri).await else {
                continue;
            };
            let tokens = match lex(&module_text) {
                Ok(tokens) => tokens,
                Err(_) => continue,
            };
            for span in collect_identifier_spans(&tokens, &target_name) {
                let key = (module_uri.to_string(), span.start);
                if seen.insert(key) {
                    locations.push(Location::new(
                        module_uri.clone(),
                        span_to_range(&module_text, span),
                    ));
                }
            }
        }
        if locations.is_empty() {
            Ok(None)
        } else {
            Ok(Some(locations))
        }
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> RpcResult<Option<OneOf<Vec<SymbolInformation>, Vec<WorkspaceSymbol>>>> {
        self.preload_workspace().await;
        let entries = self.symbols.search(&params.query).await;
        if entries.is_empty() {
            return Ok(None);
        }
        let mut symbols = Vec::new();
        for (name, loc) in entries {
            if let Some(text) = self.text_for_uri(&loc.uri).await {
                let range = span_to_range(&text, loc.span);
                #[allow(deprecated)]
                symbols.push(SymbolInformation {
                    name,
                    kind: loc.kind,
                    location: Location::new(loc.uri.clone(), range),
                    container_name: None,
                    deprecated: None,
                    tags: None,
                });
            }
        }
        if symbols.is_empty() {
            Ok(None)
        } else {
            Ok(Some(OneOf::Left(symbols)))
        }
    }

    async fn code_lens(&self, params: CodeLensParams) -> RpcResult<Option<Vec<CodeLens>>> {
        let uri = params.text_document.uri;
        let Some(text) = self.docs.get(&uri).await else {
            return Ok(None);
        };
        let tokens = match lex(&text) {
            Ok(tokens) => tokens,
            Err(_) => return Ok(None),
        };
        let module = match self.parse_cached_module(&uri, &text).await {
            Some(module) => module,
            None => return Ok(None),
        };
        let mut lenses = Vec::new();
        for item in &module.items {
            if let Item::Function(func) = item {
                let spans = collect_identifier_spans(&tokens, &func.name);
                let count = spans.len().saturating_sub(1);
                lenses.push(CodeLens {
                    range: span_to_range(&text, func.span),
                    command: Some(Command::new(
                        format!("{} reference{}", count, if count == 1 { "" } else { "s" }),
                        "prime.showReferences".into(),
                        Some(vec![json!({
                            "uri": uri.to_string(),
                            "name": func.name,
                        })]),
                    )),
                    data: None,
                });
            }
        }
        Ok(Some(lenses))
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> RpcResult<Option<Value>> {
        match params.command.as_str() {
            "prime.showReferences" => {
                if let Some(Value::Object(obj)) = params.arguments.into_iter().next() {
                    if let (Some(Value::String(name)), Some(Value::String(uri_str))) =
                        (obj.get("name"), obj.get("uri"))
                    {
                        if let Ok(uri) = Uri::from_str(uri_str.as_str()) {
                            if let Some(text) = self.text_for_uri(&uri).await {
                                if let Ok(tokens) = lex(&text) {
                                    let spans = collect_identifier_spans(&tokens, name);
                                    let count = spans.len().saturating_sub(1);
                                    let message = format!(
                                        "{} reference{} found for `{}`",
                                        count,
                                        if count == 1 { "" } else { "s" },
                                        name
                                    );
                                    let _ =
                                        self.client.show_message(MessageType::INFO, message).await;
                                }
                            }
                        }
                    }
                }
                Ok(None)
            }
            _ => Ok(None),
        }
    }
}

fn collect_diagnostics(uri: &Uri, text: &str) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let tokens = match lex(text) {
        Ok(tokens) => tokens,
        Err(errors) => {
            diags.extend(errors.into_iter().map(|err| lex_error_to_lsp(text, err)));
            return diags;
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
    if let Some(module) = module {
        diags.extend(manifest_declaration_diagnostics(uri, text, &module));
        diags.extend(import_manifest_diagnostics(uri, text, &module));
        diags.extend(unused_variable_diagnostics(&module, text));
    }
    // Avoid keeping the entire token vec if not needed elsewhere.
    drop(tokens);
    diags
}

fn lex_error_to_lsp(text: &str, err: LexError) -> Diagnostic {
    Diagnostic {
        range: span_to_range(text, err.span),
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("prime-lang".into()),
        message: error_with_context(&err.message, None, text, err.span),
        ..Default::default()
    }
}

fn syntax_error_to_lsp(text: &str, err: SyntaxError) -> Diagnostic {
    let mut message = prettify_error_message(&err.message);
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
    data: Option<Value>,
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

fn manifest_relative_string(path: &Path, manifest: &PackageManifest) -> String {
    let relative = path.strip_prefix(manifest.root()).unwrap_or(path);
    relative
        .components()
        .map(|component| component.as_os_str().to_string_lossy())
        .collect::<Vec<_>>()
        .join("/")
}

fn resolve_import_relative_path(base: &Path, import_path: &ImportPath) -> PathBuf {
    let mut path = import_path.to_relative_path();
    if path.extension().and_then(|ext| ext.to_str()) != Some("prime") {
        path.set_extension("prime");
    }
    if path.is_absolute() {
        return path;
    }
    let base_dir = base
        .parent()
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| PathBuf::from("."));
    base_dir.join(path)
}

fn diagnostic_code(diagnostic: &Diagnostic) -> Option<&str> {
    match diagnostic.code.as_ref()? {
        NumberOrString::String(code) => Some(code.as_str()),
        _ => None,
    }
}

struct ManifestEntryAction {
    module_name: String,
    module_path: String,
    manifest_path: PathBuf,
    visibility: String,
    diagnostic: Diagnostic,
}

impl ManifestEntryAction {
    fn to_code_action(self) -> Option<CodeActionOrCommand> {
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

fn manifest_entry_action(diagnostic: &Diagnostic) -> Option<ManifestEntryAction> {
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

fn select_symbol_location<'a>(
    current_uri: &Uri,
    module: Option<&Module>,
    candidates: &'a [SymbolLocation],
) -> Option<&'a SymbolLocation> {
    if candidates.is_empty() {
        return None;
    }
    if let Some(loc) = candidates.iter().find(|loc| &loc.uri == current_uri) {
        return Some(loc);
    }
    if let Some(module) = module {
        if let Some(loc) = candidates.iter().find(|loc| loc.module_name == module.name) {
            return Some(loc);
        }
    }
    if let Some(loc) = candidates
        .iter()
        .find(|loc| matches!(loc.visibility, Visibility::Public))
    {
        return Some(loc);
    }
    candidates.first()
}

fn collect_symbols(uri: &Uri, text: &str, module: &Module) -> Vec<SymbolInformation> {
    let mut symbols = Vec::new();
    for item in &module.items {
        match item {
            Item::Function(func) => symbols.push(make_symbol(
                uri,
                text,
                &func.name,
                SymbolKind::FUNCTION,
                func.span,
            )),
            Item::Struct(def) => symbols.push(make_symbol(
                uri,
                text,
                &def.name,
                SymbolKind::STRUCT,
                def.span,
            )),
            Item::Enum(def) => symbols.push(make_symbol(
                uri,
                text,
                &def.name,
                SymbolKind::ENUM,
                def.span,
            )),
            Item::Interface(def) => symbols.push(make_symbol(
                uri,
                text,
                &def.name,
                SymbolKind::INTERFACE,
                def.span,
            )),
            Item::Const(def) => symbols.push(make_symbol(
                uri,
                text,
                &def.name,
                SymbolKind::CONSTANT,
                def.span,
            )),
            Item::Impl(_) => {}
        }
    }
    symbols
}

fn make_symbol(
    uri: &Uri,
    text: &str,
    name: &str,
    kind: SymbolKind,
    span: Span,
) -> SymbolInformation {
    #[allow(deprecated)]
    SymbolInformation {
        name: name.to_string(),
        kind,
        location: Location::new(uri.clone(), span_to_range(text, span)),
        container_name: None,
        deprecated: None,
        tags: None,
    }
}

fn parse_module_from_uri(uri: &Uri, text: &str) -> Result<Module, SyntaxErrors> {
    let path = url_to_path(uri).ok_or_else(|| SyntaxErrors::new(vec![]))?;
    let module_name = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main")
        .to_string();
    parse_module(&module_name, path, text)
}

fn unused_variable_diagnostics(module: &Module, text: &str) -> Vec<Diagnostic> {
    let decls = collect_decl_spans(module);
    if decls.is_empty() {
        return Vec::new();
    }
    let used = collect_used_identifiers(module);
    decls
        .into_iter()
        .filter(|decl| !used.contains(&decl.name))
        .map(|decl| Diagnostic {
            range: span_to_range(text, decl.span),
            severity: Some(DiagnosticSeverity::WARNING),
            source: Some("prime-lang".into()),
            message: format!("Variable `{}` is never used", decl.name),
            ..Default::default()
        })
        .collect()
}

#[derive(Debug, Clone)]
struct DeclInfo {
    name: String,
    span: Span,
    scope: Span,
    available_from: usize,
    ty: Option<TypeExpr>,
    value_span: Option<Span>,
    mutability: Mutability,
    kind: DeclKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DeclKind {
    Param,
    Let,
    ForBinding,
    Pattern,
}

fn collect_decl_spans(module: &Module) -> Vec<DeclInfo> {
    let mut decls = Vec::new();
    for item in &module.items {
        match item {
            Item::Function(func) => collect_decl_from_function(func, None, &mut decls),
            Item::Impl(block) => {
                let target_ty = Some(TypeExpr::named(block.target.clone()));
                for func in &block.methods {
                    collect_decl_from_function(func, target_ty.clone(), &mut decls);
                }
            }
            _ => {}
        }
    }
    decls
}

fn collect_decl_from_function(
    func: &FunctionDef,
    receiver_override: Option<TypeExpr>,
    decls: &mut Vec<DeclInfo>,
) {
    let body_span = match &func.body {
        FunctionBody::Block(block) => block.span,
        FunctionBody::Expr(expr) => expr.span,
    };
    let available_from = body_span.start;
    for param in &func.params {
        let mut param_ty = param.ty.ty.clone();
        if param.name == "self" {
            if let Some(override_ty) = receiver_override.clone() {
                param_ty = override_ty;
            } else if let Some(resolved) = receiver_type_name(&param.ty.ty).map(TypeExpr::named) {
                param_ty = resolved;
            }
        }
        decls.push(DeclInfo {
            name: param.name.clone(),
            span: param.span,
            scope: body_span,
            available_from,
            ty: Some(param_ty),
            value_span: None,
            mutability: param.mutability,
            kind: DeclKind::Param,
        });
    }
    if let FunctionBody::Block(block) = &func.body {
        collect_decl_from_block(block, decls);
    }
}

fn scope_contains(scope: Span, offset: usize) -> bool {
    offset >= scope.start && offset < scope.end
}

fn span_contains(span: Span, offset: usize) -> bool {
    offset >= span.start && offset <= span.end
}

fn find_local_decl(module: &Module, name: &str, offset: usize) -> Option<DeclInfo> {
    let decls: Vec<_> = collect_decl_spans(module)
        .into_iter()
        .filter(|decl| decl.name == name)
        .collect();
    let strict = decls
        .iter()
        .filter(|decl| scope_contains(decl.scope, offset))
        .filter(|decl| offset >= decl.available_from || span_contains(decl.span, offset))
        .max_by_key(|decl| decl.span.start)
        .cloned();
    strict.or_else(|| decls.into_iter().max_by_key(|decl| decl.span.start))
}

fn find_local_definition_span(module: &Module, name: &str, offset: usize) -> Option<Span> {
    find_local_decl(module, name, offset).map(|decl| decl.span)
}

fn find_module_item_span(module: &Module, name: &str) -> Option<Span> {
    for item in &module.items {
        match item {
            Item::Function(func) if func.name == name => return Some(func.span),
            Item::Struct(def) if def.name == name => return Some(def.span),
            Item::Enum(def) if def.name == name => return Some(def.span),
            Item::Interface(def) if def.name == name => return Some(def.span),
            Item::Const(def) if def.name == name => return Some(def.span),
            Item::Impl(_) => {}
            _ => {}
        }
    }
    None
}

fn collect_decl_from_block(block: &Block, decls: &mut Vec<DeclInfo>) {
    let scope = block.span;
    for statement in &block.statements {
        match statement {
            Statement::Let(stmt) => {
                if let Some(value) = &stmt.value {
                    collect_decl_from_expr(value, decls);
                }
                let mut ty = stmt.ty.as_ref().map(|annotation| annotation.ty.clone());
                if ty.is_none() {
                    ty = infer_type_from_let_value(stmt);
                }
                decls.push(DeclInfo {
                    name: stmt.name.clone(),
                    span: stmt.span,
                    scope,
                    available_from: stmt.span.end,
                    ty,
                    value_span: stmt.value.as_ref().map(expr_span),
                    mutability: stmt.mutability,
                    kind: DeclKind::Let,
                });
            }
            Statement::Assign(stmt) => {
                collect_decl_from_expr(&stmt.target, decls);
                collect_decl_from_expr(&stmt.value, decls);
            }
            Statement::Expr(expr_stmt) => collect_decl_from_expr(&expr_stmt.expr, decls),
            Statement::Return(ret) => {
                for value in &ret.values {
                    collect_decl_from_expr(value, decls);
                }
            }
            Statement::If(if_stmt) => {
                collect_decl_from_expr(&if_stmt.condition, decls);
                collect_decl_from_block(&if_stmt.then_branch, decls);
                if let Some(else_branch) = &if_stmt.else_branch {
                    collect_decl_from_block(else_branch, decls);
                }
            }
            Statement::While(while_stmt) => {
                collect_decl_from_expr(&while_stmt.condition, decls);
                collect_decl_from_block(&while_stmt.body, decls);
            }
            Statement::ForRange(for_stmt) => {
                collect_decl_from_range(&for_stmt.range, decls);
                let body_span = for_stmt.body.span;
                decls.push(DeclInfo {
                    name: for_stmt.binding.clone(),
                    span: for_stmt.span,
                    scope: body_span,
                    available_from: body_span.start,
                    ty: None,
                    value_span: None,
                    mutability: Mutability::Immutable,
                    kind: DeclKind::ForBinding,
                });
                collect_decl_from_block(&for_stmt.body, decls);
            }
            Statement::Match(match_stmt) => {
                collect_decl_from_expr(&match_stmt.expr, decls);
                for arm in &match_stmt.arms {
                    let arm_scope = arm.body.span;
                    collect_pattern_decls(&arm.pattern, arm_scope, arm_scope.start, decls);
                    if let Some(guard) = &arm.guard {
                        collect_decl_from_expr(guard, decls);
                    }
                    collect_decl_from_block(&arm.body, decls);
                }
            }
            Statement::Defer(defer_stmt) => collect_decl_from_expr(&defer_stmt.expr, decls),
            Statement::Block(inner) => collect_decl_from_block(inner, decls),
            Statement::Break(_) | Statement::Continue(_) => {}
        }
    }
    if let Some(tail) = &block.tail {
        collect_decl_from_expr(tail, decls);
    }
}

fn collect_decl_from_expr(expr: &Expr, decls: &mut Vec<DeclInfo>) {
    match expr {
        Expr::Identifier(_) | Expr::Literal(_) => {}
        Expr::Binary { left, right, .. } => {
            collect_decl_from_expr(left, decls);
            collect_decl_from_expr(right, decls);
        }
        Expr::Unary { expr: inner, .. } => collect_decl_from_expr(inner, decls),
        Expr::Call {
            callee,
            type_args: _,
            args,
            ..
        } => {
            collect_decl_from_expr(callee, decls);
            for arg in args {
                collect_decl_from_expr(arg, decls);
            }
        }
        Expr::FieldAccess { base, .. } => collect_decl_from_expr(base, decls),
        Expr::StructLiteral { fields, .. } => match fields {
            StructLiteralKind::Named(named) => {
                for field in named {
                    collect_decl_from_expr(&field.value, decls);
                }
            }
            StructLiteralKind::Positional(values) => {
                for value in values {
                    collect_decl_from_expr(value, decls);
                }
            }
        },
        Expr::EnumLiteral { values, .. } => {
            for value in values {
                collect_decl_from_expr(value, decls);
            }
        }
        Expr::Block(block) => collect_decl_from_block(block, decls),
        Expr::If(if_expr) => {
            collect_decl_from_expr(&if_expr.condition, decls);
            collect_decl_from_block(&if_expr.then_branch, decls);
            if let Some(else_branch) = &if_expr.else_branch {
                collect_decl_from_block(else_branch, decls);
            }
        }
        Expr::Match(match_expr) => {
            collect_decl_from_expr(&match_expr.expr, decls);
            for arm in &match_expr.arms {
                let value_span = expr_span(&arm.value);
                collect_pattern_decls(&arm.pattern, value_span, value_span.start, decls);
                if let Some(guard) = &arm.guard {
                    collect_decl_from_expr(guard, decls);
                }
                collect_decl_from_expr(&arm.value, decls);
            }
        }
        Expr::Tuple(values, _) | Expr::ArrayLiteral(values, _) => {
            for value in values {
                collect_decl_from_expr(value, decls);
            }
        }
        Expr::Range(range) => collect_decl_from_range(range, decls),
        Expr::Reference { expr: inner, .. } => collect_decl_from_expr(inner, decls),
        Expr::Deref { expr: inner, .. } => collect_decl_from_expr(inner, decls),
        Expr::Move { expr: inner, .. } => collect_decl_from_expr(inner, decls),
    }
}

fn infer_type_from_let_value(stmt: &LetStmt) -> Option<TypeExpr> {
    let value = stmt.value.as_ref()?;
    if let Expr::StructLiteral { name, .. } = value {
        return Some(TypeExpr::Named(name.clone(), Vec::new()));
    }
    None
}

fn collect_decl_from_range(range: &RangeExpr, decls: &mut Vec<DeclInfo>) {
    collect_decl_from_expr(&range.start, decls);
    collect_decl_from_expr(&range.end, decls);
}

fn collect_pattern_decls(
    pattern: &Pattern,
    scope: Span,
    available_from: usize,
    decls: &mut Vec<DeclInfo>,
) {
    match pattern {
        Pattern::Identifier(name, span) => decls.push(DeclInfo {
            name: name.clone(),
            span: *span,
            scope,
            available_from,
            ty: None,
            value_span: None,
            mutability: Mutability::Immutable,
            kind: DeclKind::Pattern,
        }),
        Pattern::EnumVariant { bindings, .. } => {
            for binding in bindings {
                collect_pattern_decls(binding, scope, available_from, decls);
            }
        }
        _ => {}
    }
}

fn collect_used_identifiers(module: &Module) -> HashSet<String> {
    let mut used = HashSet::new();
    for item in &module.items {
        collect_used_in_item(item, &mut used);
    }
    used
}

fn collect_used_in_item(item: &Item, used: &mut HashSet<String>) {
    match item {
        Item::Function(func) => collect_used_in_function(func, used),
        Item::Impl(block) => {
            for method in &block.methods {
                collect_used_in_function(method, used);
            }
        }
        Item::Const(def) => collect_expr_idents(&def.value, used),
        _ => {}
    }
}

fn collect_used_in_function(func: &FunctionDef, used: &mut HashSet<String>) {
    match &func.body {
        FunctionBody::Block(block) => collect_used_in_block(block, used),
        FunctionBody::Expr(expr) => collect_expr_idents(&expr.node, used),
    }
}

fn collect_used_in_block(block: &Block, used: &mut HashSet<String>) {
    for statement in &block.statements {
        collect_used_in_statement(statement, used);
    }
    if let Some(tail) = &block.tail {
        collect_expr_idents(tail, used);
    }
}

fn collect_used_in_statement(statement: &Statement, used: &mut HashSet<String>) {
    match statement {
        Statement::Let(stmt) => {
            if let Some(value) = &stmt.value {
                collect_expr_idents(value, used);
            }
        }
        Statement::Assign(stmt) => {
            collect_expr_idents(&stmt.target, used);
            collect_expr_idents(&stmt.value, used);
        }
        Statement::Expr(expr) => collect_expr_idents(&expr.expr, used),
        Statement::Return(ret) => {
            for value in &ret.values {
                collect_expr_idents(value, used);
            }
        }
        Statement::If(if_stmt) => {
            collect_expr_idents(&if_stmt.condition, used);
            collect_used_in_block(&if_stmt.then_branch, used);
            if let Some(else_branch) = &if_stmt.else_branch {
                collect_used_in_block(else_branch, used);
            }
        }
        Statement::While(while_stmt) => {
            collect_expr_idents(&while_stmt.condition, used);
            collect_used_in_block(&while_stmt.body, used);
        }
        Statement::ForRange(for_stmt) => {
            collect_range_expr(&for_stmt.range, used);
            collect_used_in_block(&for_stmt.body, used);
        }
        Statement::Match(match_stmt) => {
            collect_expr_idents(&match_stmt.expr, used);
            for arm in &match_stmt.arms {
                if let Some(guard) = &arm.guard {
                    collect_expr_idents(guard, used);
                }
                collect_used_in_block(&arm.body, used);
            }
        }
        Statement::Defer(defer_stmt) => collect_expr_idents(&defer_stmt.expr, used),
        Statement::Block(block) => collect_used_in_block(block, used),
        Statement::Break(_) | Statement::Continue(_) => {}
    }
}

fn collect_expr_idents(expr: &Expr, used: &mut HashSet<String>) {
    match expr {
        Expr::Identifier(ident) => {
            used.insert(ident.name.clone());
        }
        Expr::Literal(_) => {}
        Expr::Binary { left, right, .. } => {
            collect_expr_idents(left, used);
            collect_expr_idents(right, used);
        }
        Expr::Unary { expr, .. } => collect_expr_idents(expr, used),
        Expr::Call {
            callee,
            type_args: _,
            args,
            ..
        } => {
            collect_expr_idents(callee, used);
            for arg in args {
                collect_expr_idents(arg, used);
            }
        }
        Expr::FieldAccess { base, .. } => collect_expr_idents(base, used),
        Expr::StructLiteral { fields, .. } => match fields {
            StructLiteralKind::Named(named) => {
                for field in named {
                    collect_expr_idents(&field.value, used);
                }
            }
            StructLiteralKind::Positional(values) => {
                for value in values {
                    collect_expr_idents(value, used);
                }
            }
        },
        Expr::EnumLiteral { values, .. } => {
            for value in values {
                collect_expr_idents(value, used);
            }
        }
        Expr::Block(block) => collect_used_in_block(block, used),
        Expr::If(if_expr) => {
            collect_expr_idents(&if_expr.condition, used);
            collect_used_in_block(&if_expr.then_branch, used);
            if let Some(else_branch) = &if_expr.else_branch {
                collect_used_in_block(else_branch, used);
            }
        }
        Expr::Match(match_expr) => {
            collect_expr_idents(&match_expr.expr, used);
            for arm in &match_expr.arms {
                if let Some(guard) = &arm.guard {
                    collect_expr_idents(guard, used);
                }
                collect_expr_idents(&arm.value, used);
            }
        }
        Expr::Tuple(values, _) | Expr::ArrayLiteral(values, _) => {
            for value in values {
                collect_expr_idents(value, used);
            }
        }
        Expr::Range(range) => collect_range_expr(range, used),
        Expr::Reference { expr, .. } => collect_expr_idents(expr, used),
        Expr::Deref { expr, .. } => collect_expr_idents(expr, used),
        Expr::Move { expr, .. } => collect_expr_idents(expr, used),
    }
}

fn collect_range_expr(range: &RangeExpr, used: &mut HashSet<String>) {
    collect_expr_idents(&range.start, used);
    collect_expr_idents(&range.end, used);
}

fn collect_var_infos(text: &str, tokens: &[Token]) -> Vec<VarInfo> {
    let mut vars = Vec::new();
    let mut idx = 0;
    while idx < tokens.len() {
        if matches!(tokens[idx].kind, TokenKind::Let) {
            let mut cursor = idx + 1;
            if matches!(tokens.get(cursor).map(|t| &t.kind), Some(TokenKind::Mut)) {
                cursor += 1;
            }
            let Some(name_token) = tokens.get(cursor) else {
                idx += 1;
                continue;
            };
            let name = if let TokenKind::Identifier(name) = &name_token.kind {
                name.clone()
            } else {
                idx += 1;
                continue;
            };
            cursor += 1;

            let mut ty = None;
            if matches!(tokens.get(cursor).map(|t| &t.kind), Some(TokenKind::Colon)) {
                cursor += 1;
                let type_start = tokens
                    .get(cursor)
                    .map(|t| t.span.start)
                    .unwrap_or(name_token.span.end);
                let mut type_end = type_start;
                while cursor < tokens.len()
                    && !matches!(tokens[cursor].kind, TokenKind::Eq | TokenKind::Semi)
                {
                    type_end = tokens[cursor].span.end;
                    cursor += 1;
                }
                if type_end > type_start {
                    ty = Some(extract_text(text, type_start, type_end));
                }
            }

            let mut expr_text = None;
            if matches!(tokens.get(cursor).map(|t| &t.kind), Some(TokenKind::Eq)) {
                cursor += 1;
                let expr_start = tokens
                    .get(cursor)
                    .map(|t| t.span.start)
                    .unwrap_or(name_token.span.end);
                let mut expr_end = expr_start;
                while cursor < tokens.len() && !matches!(tokens[cursor].kind, TokenKind::Semi) {
                    expr_end = tokens[cursor].span.end;
                    cursor += 1;
                }
                if expr_end > expr_start {
                    expr_text = Some(extract_text(text, expr_start, expr_end));
                }
            }

            vars.push(VarInfo {
                name,
                ty,
                expr_text,
            });
        }
        idx += 1;
    }
    vars
}

#[derive(Debug, Clone)]
struct VarInfo {
    name: String,
    ty: Option<String>,
    expr_text: Option<String>,
}

fn hover_for_token(
    text: &str,
    token: &Token,
    vars: &[VarInfo],
    module: Option<&Module>,
    struct_info: Option<&HashMap<String, StructInfo>>,
) -> Option<Hover> {
    let span = token.span;
    let hover = match &token.kind {
        TokenKind::Identifier(name) => {
            if name == "out" {
                Some(
                    "Built-in output function **out(expr)**\n\nPrints the evaluated expression."
                        .to_string(),
                )
            } else if let Some(module) = module {
                if let Some(field_hover) =
                    hover_for_struct_field_definition(text, span, name, module)
                {
                    return Some(field_hover);
                }
                if let Some(method_hover) =
                    hover_for_interface_method_definition(text, span, name, module)
                {
                    return Some(method_hover);
                }
                if let Some(struct_info) = struct_info {
                    if let Some(hover) =
                        hover_for_field_usage(text, span, struct_info, module, span.start)
                    {
                        return Some(hover);
                    }
                }
                if let Some(decl) = find_local_decl(module, name, span.start) {
                    return Some(hover_for_local_decl(text, span, &decl));
                }
                if let Some(hover) = hover_for_module_symbol(text, span, module, name) {
                    return Some(hover);
                }
                if let Some(info) = vars.iter().rev().find(|var| var.name == *name) {
                    let mut snippet = String::from("```prime\nlet ");
                    snippet.push_str(&info.name);
                    if let Some(ty) = &info.ty {
                        snippet.push_str(": ");
                        snippet.push_str(ty);
                    }
                    if let Some(expr) = &info.expr_text {
                        snippet.push_str(" = ");
                        snippet.push_str(expr);
                    }
                    snippet.push_str(";\n```\n");
                    if let Some(ty) = &info.ty {
                        snippet.push_str(&format!("Type: `{ty}`"));
                    } else {
                        snippet.push_str("Type: inferred");
                    }
                    Some(snippet)
                } else {
                    Some(format!("Identifier `{name}`"))
                }
            } else if let Some(info) = vars.iter().rev().find(|var| var.name == *name) {
                let mut snippet = String::from("```prime\nlet ");
                snippet.push_str(&info.name);
                if let Some(ty) = &info.ty {
                    snippet.push_str(": ");
                    snippet.push_str(ty);
                }
                if let Some(expr) = &info.expr_text {
                    snippet.push_str(" = ");
                    snippet.push_str(expr);
                }
                snippet.push_str(";\n```\n");
                if let Some(ty) = &info.ty {
                    snippet.push_str(&format!("Type: `{ty}`"));
                } else {
                    snippet.push_str("Type: inferred");
                }
                Some(snippet)
            } else {
                Some(format!("Identifier `{name}`"))
            }
        }
        TokenKind::Let => Some("Keyword **let**\n\nIntroduces a new binding.".to_string()),
        TokenKind::Fn => Some("Keyword **fn**\n\nDefines a function.".to_string()),
        TokenKind::Struct => Some("Keyword **struct**\n\nDeclares a structure.".to_string()),
        TokenKind::Enum => Some("Keyword **enum**\n\nDeclares an enum.".to_string()),
        TokenKind::Interface => {
            Some("Keyword **interface**\n\nDeclares an interface of required methods.".to_string())
        }
        TokenKind::Impl => {
            Some("Keyword **impl**\n\nImplements an interface for a concrete struct.".to_string())
        }
        TokenKind::Pub => Some("Keyword **pub**\n\nMarks an item as publicly visible.".to_string()),
        TokenKind::Const => Some("Keyword **const**\n\nDeclares a constant value.".to_string()),
        TokenKind::Return => Some("Keyword **return**\n\nExits the current function.".to_string()),
        TokenKind::If => Some("Keyword **if**\n\nConditional execution.".to_string()),
        TokenKind::Else => Some("Keyword **else**\n\nAlternate branch for `if`.".to_string()),
        TokenKind::While => {
            Some("Keyword **while**\n\nLoop while the condition holds.".to_string())
        }
        TokenKind::For => Some("Keyword **for**\n\nRange-based loop.".to_string()),
        TokenKind::Match => Some("Keyword **match**\n\nPattern matching expression.".to_string()),
        TokenKind::Defer => Some("Keyword **defer**\n\nRun code when leaving scope.".to_string()),
        TokenKind::Import => {
            Some("Keyword **import**\n\nBring another module into scope.".to_string())
        }
        TokenKind::Using => {
            Some("Keyword **using**\n\nRe-export or alias imported symbols.".to_string())
        }
        TokenKind::True | TokenKind::False => Some("Boolean literal.".to_string()),
        TokenKind::Integer(value) => {
            return Some(markdown_hover(
                text,
                span,
                format!("Integer literal `{value}`"),
            ));
        }
        TokenKind::Float(value) => {
            return Some(markdown_hover(
                text,
                span,
                format!("Float literal `{value}`"),
            ));
        }
        TokenKind::String(value) => {
            return Some(markdown_hover(
                text,
                span,
                format!("String literal \"{value}\""),
            ));
        }
        TokenKind::Rune(value) => {
            return Some(markdown_hover(
                text,
                span,
                format!("Rune literal `'{}'`", value),
            ));
        }
        _ => None,
    }?;
    Some(markdown_hover(text, span, hover.to_string()))
}

fn hover_for_module_symbol(
    text: &str,
    usage_span: Span,
    module: &Module,
    name: &str,
) -> Option<Hover> {
    for item in &module.items {
        match item {
            Item::Function(func) if func.name == name => {
                let signature = format_function_signature(func);
                let value = format!("```prime\n{}\n```", signature);
                return Some(markdown_hover(text, usage_span, value));
            }
            Item::Struct(def) if def.name == name => {
                let snippet = format_struct_hover(def);
                let value = format!("```prime\n{}\n```", snippet);
                return Some(markdown_hover(text, usage_span, value));
            }
            Item::Interface(def) if def.name == name => {
                let snippet = format_interface_hover(def);
                let value = format!("```prime\n{}\n```", snippet);
                return Some(markdown_hover(text, usage_span, value));
            }
            Item::Enum(def) => {
                if def.name == name {
                    let snippet = format_enum_hover(def);
                    let value = format!("```prime\n{}\n```", snippet);
                    return Some(markdown_hover(text, usage_span, value));
                }
                if let Some(variant) = def.variants.iter().find(|variant| variant.name == name) {
                    let signature = format_enum_variant_signature(variant);
                    let params = format_type_params(&def.type_params);
                    let mut value = String::from("```prime\n");
                    value.push_str(&def.name);
                    value.push_str(&params);
                    value.push_str(" :: ");
                    value.push_str(&signature);
                    value.push_str("\n```");
                    return Some(markdown_hover(text, usage_span, value));
                }
            }
            Item::Const(def) if def.name == name => {
                let snippet = format_const_snippet(text, def);
                let value = format!("```prime\n{}\n```", snippet);
                return Some(markdown_hover(text, usage_span, value));
            }
            Item::Impl(block) => {
                for method in &block.methods {
                    if method.name == name {
                        let signature = format_function_signature(method);
                        let header = format!(
                            "impl {}{} for {}",
                            block.interface,
                            format_type_arguments(&block.type_args),
                            block.target
                        );
                        let value = format!("```prime\n{}\n{}\n```", header, signature);
                        return Some(markdown_hover(text, usage_span, value));
                    }
                }
            }
            _ => {}
        }
    }
    None
}

fn hover_for_local_decl(text: &str, usage_span: Span, decl: &DeclInfo) -> Hover {
    let mut snippet = extract_text(text, decl.span.start, decl.span.end)
        .trim()
        .to_string();
    if snippet.is_empty() {
        snippet = decl.name.clone();
    }
    let mut value = String::from("```prime\n");
    value.push_str(&snippet);
    if !snippet.ends_with('\n') {
        value.push('\n');
    }
    value.push_str("```\n");
    value.push_str(&format!("Name: `{}`", decl.name));
    if let Some(ty) = &decl.ty {
        value.push_str(&format!("\nType: `{}`", format_type_expr(ty)));
    } else {
        value.push_str("\nType: `inferred`");
    }
    value.push_str(&format!(
        "\nMutable: {}",
        if decl.mutability.is_mutable() {
            "`mut`"
        } else {
            "`let`"
        }
    ));
    value.push_str(&format!("\nKind: {}", format_decl_kind(decl.kind)));
    if let Some(span) = decl.value_span {
        let raw = extract_text(text, span.start, span.end).trim().to_string();
        if !raw.is_empty() {
            let single_line = raw.split_whitespace().collect::<Vec<_>>().join(" ");
            value.push_str(&format!("\nValue: `{single_line}`"));
        }
    }
    markdown_hover(text, usage_span, value)
}

fn hover_for_field_usage(
    text: &str,
    span: Span,
    struct_info: &HashMap<String, StructInfo>,
    module: &Module,
    offset: usize,
) -> Option<Hover> {
    let chain = chain_for_field_token(text, span)?;
    let (_, field_info) = resolve_chain_from_scope(&chain, module, struct_info, offset)?;
    let Some((parent_name, field)) = field_info else {
        return None;
    };
    let mut value = String::new();
    value.push_str(&format!("Field `{}`\n\n", field.name));
    value.push_str(&format!("Type: `{}`", format_type_expr(&field.ty)));
    value.push_str(&format!("\nDefined on: `{}`", field.declared_in));
    if field.declared_in != parent_name {
        value.push_str(&format!("\nPromoted through: `{}`", parent_name));
    } else {
        value.push_str(&format!("\nParent: `{}`", parent_name));
    }
    Some(markdown_hover(text, span, value))
}

fn hover_for_struct_field_definition(
    text: &str,
    usage_span: Span,
    name: &str,
    module: &Module,
) -> Option<Hover> {
    for item in &module.items {
        if let Item::Struct(def) = item {
            for field in &def.fields {
                if let Some(field_name) = &field.name {
                    if field_name == name && span_contains(field.span, usage_span.start) {
                        let mut value = String::new();
                        value.push_str(&format!("Field `{}`\n\n", name));
                        value.push_str(&format!("Type: `{}`", format_type_expr(&field.ty.ty)));
                        value.push_str(&format!("\nStruct: `{}`", def.name));
                        if field.embedded {
                            value.push_str("\nEmbedded field");
                        }
                        return Some(markdown_hover(text, usage_span, value));
                    }
                }
            }
        }
    }
    None
}

fn hover_for_interface_method_definition(
    text: &str,
    usage_span: Span,
    name: &str,
    module: &Module,
) -> Option<Hover> {
    for item in &module.items {
        if let Item::Interface(def) = item {
            for method in &def.methods {
                if method.name == name && span_contains(method.span, usage_span.start) {
                    let mut value = String::new();
                    value.push_str("```prime\n");
                    value.push_str(&format_interface_method_signature(method, None));
                    value.push_str("\n```\n");
                    value.push_str(&format!(
                        "Interface: `{}`{}",
                        def.name,
                        format_type_params(&def.type_params)
                    ));
                    return Some(markdown_hover(text, usage_span, value));
                }
            }
        }
    }
    None
}

fn format_decl_kind(kind: DeclKind) -> &'static str {
    match kind {
        DeclKind::Param => "parameter",
        DeclKind::Let => "local binding",
        DeclKind::ForBinding => "loop binding",
        DeclKind::Pattern => "pattern binding",
    }
}

fn format_function_signature(func: &FunctionDef) -> String {
    let mut signature = String::from("fn ");
    signature.push_str(&func.name);
    signature.push_str(&format_type_params(&func.type_params));
    signature.push('(');
    let params = func
        .params
        .iter()
        .map(format_function_param)
        .collect::<Vec<_>>()
        .join(", ");
    signature.push_str(&params);
    signature.push(')');
    if !func.returns.is_empty() {
        signature.push_str(" -> ");
        if func.returns.len() == 1 {
            signature.push_str(&format_type_expr(&func.returns[0].ty));
        } else {
            let returns = func
                .returns
                .iter()
                .map(|ret| format_type_expr(&ret.ty))
                .collect::<Vec<_>>()
                .join(", ");
            signature.push('(');
            signature.push_str(&returns);
            signature.push(')');
        }
    }
    signature
}

fn format_function_param(param: &FunctionParam) -> String {
    if param.name == "self" {
        if let Some(shorthand) = format_self_param(&param.ty.ty) {
            let mut text = String::new();
            if param.mutability.is_mutable() {
                text.push_str("mut ");
            }
            text.push_str(&shorthand);
            return text;
        }
    }
    let mut text = String::new();
    if param.mutability.is_mutable() {
        text.push_str("mut ");
    }
    text.push_str(&param.name);
    text.push_str(": ");
    text.push_str(&format_type_expr(&param.ty.ty));
    text
}

fn format_interface_method_signature(
    method: &InterfaceMethod,
    subst: Option<&HashMap<String, TypeExpr>>,
) -> String {
    let mut signature = String::from("fn ");
    signature.push_str(&method.name);
    signature.push('(');
    let params = method
        .params
        .iter()
        .map(|param| {
            let mut cloned = param.clone();
            if let Some(map) = subst {
                cloned.ty = cloned.ty.substitute(map);
            }
            format_function_param(&cloned)
        })
        .collect::<Vec<_>>()
        .join(", ");
    signature.push_str(&params);
    signature.push(')');
    if !method.returns.is_empty() {
        signature.push_str(" -> ");
        if method.returns.len() == 1 {
            let mut ret = method.returns[0].clone();
            if let Some(map) = subst {
                ret = ret.substitute(map);
            }
            signature.push_str(&format_type_expr(&ret.ty));
        } else {
            let returns = method
                .returns
                .iter()
                .map(|ret| {
                    let mut clone = ret.clone();
                    if let Some(map) = subst {
                        clone = clone.substitute(map);
                    }
                    format_type_expr(&clone.ty)
                })
                .collect::<Vec<_>>()
                .join(", ");
            signature.push('(');
            signature.push_str(&returns);
            signature.push(')');
        }
    }
    signature
}

fn format_self_param(ty: &TypeExpr) -> Option<String> {
    match ty {
        TypeExpr::SelfType => Some("self".into()),
        TypeExpr::Reference { mutable, ty } => {
            if matches!(ty.as_ref(), TypeExpr::SelfType) {
                if *mutable {
                    Some("&mut self".into())
                } else {
                    Some("&self".into())
                }
            } else {
                None
            }
        }
        TypeExpr::Pointer { mutable, ty } => {
            if matches!(ty.as_ref(), TypeExpr::SelfType) {
                if *mutable {
                    Some("*mut self".into())
                } else {
                    Some("*self".into())
                }
            } else {
                None
            }
        }
        _ => None,
    }
}

fn format_struct_hover(def: &StructDef) -> String {
    let mut lines = Vec::new();
    let mut header = format!(
        "struct {}{}",
        def.name,
        format_type_params(&def.type_params)
    );
    header.push_str(" {");
    lines.push(header);
    for field in &def.fields {
        let mut line = String::from("  ");
        if let Some(name) = &field.name {
            line.push_str(name);
            line.push_str(": ");
            line.push_str(&format_type_expr(&field.ty.ty));
        } else if field.embedded {
            line.push_str(&format_type_expr(&field.ty.ty));
        } else {
            line.push_str("<anonymous field>");
        }
        line.push(';');
        lines.push(line);
    }
    lines.push("}".into());
    lines.join("\n")
}

fn format_enum_hover(def: &EnumDef) -> String {
    let mut lines = Vec::new();
    let mut header = format!("enum {}{}", def.name, format_type_params(&def.type_params));
    header.push_str(" {");
    lines.push(header);
    for variant in &def.variants {
        lines.push(format!("  {},", format_enum_variant_signature(variant)));
    }
    lines.push("}".into());
    lines.join("\n")
}

fn format_interface_hover(def: &InterfaceDef) -> String {
    let mut lines = Vec::new();
    let mut header = format!(
        "interface {}{}",
        def.name,
        format_type_params(&def.type_params)
    );
    header.push_str(" {");
    lines.push(header);
    for method in &def.methods {
        lines.push(format!(
            "  {};",
            format_interface_method_signature(method, None)
        ));
    }
    lines.push("}".into());
    lines.join("\n")
}

fn format_enum_variant_signature(variant: &EnumVariant) -> String {
    if variant.fields.is_empty() {
        return variant.name.clone();
    }
    let fields = variant
        .fields
        .iter()
        .map(|field| format_type_expr(&field.ty))
        .collect::<Vec<_>>()
        .join(", ");
    format!("{}({})", variant.name, fields)
}

fn format_const_snippet(text: &str, def: &ConstDef) -> String {
    let snippet = extract_text(text, def.span.start, def.span.end)
        .trim()
        .to_string();
    if snippet.is_empty() {
        let mut fallback = format!("const {}", def.name);
        if let Some(ty) = &def.ty {
            fallback.push_str(": ");
            fallback.push_str(&format_type_expr(&ty.ty));
        }
        fallback
    } else {
        snippet
    }
}

fn markdown_hover(text: &str, span: Span, value: String) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: Some(span_to_range(text, span)),
    }
}

fn token_at<'a>(tokens: &'a [Token], offset: usize) -> Option<&'a Token> {
    tokens
        .iter()
        .find(|token| offset >= token.span.start && offset < token.span.end)
}

fn identifier_at(tokens: &[Token], offset: usize) -> Option<(String, Span)> {
    token_at(tokens, offset).and_then(|token| match &token.kind {
        TokenKind::Identifier(name) => Some((name.clone(), token.span)),
        _ => None,
    })
}

fn collect_identifier_spans(tokens: &[Token], target: &str) -> Vec<Span> {
    let mut spans: Vec<Span> = tokens
        .iter()
        .filter_map(|token| match &token.kind {
            TokenKind::Identifier(name) if name == target => Some(token.span),
            _ => None,
        })
        .collect();
    spans.sort_by(|a, b| a.start.cmp(&b.start).then_with(|| a.end.cmp(&b.end)));
    spans.dedup();
    spans
}

fn is_valid_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some(ch) if ch.is_ascii_alphabetic() || ch == '_' => (),
        _ => return false,
    }
    chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
}

fn span_to_range(text: &str, span: Span) -> Range {
    let len = text.len();
    let start_offset = span.start.min(len);
    let end_offset = span.end.min(len);
    if start_offset < end_offset {
        Range {
            start: offset_to_position(text, start_offset),
            end: offset_to_position(text, end_offset),
        }
    } else {
        let reference = adjust_zero_length_offset(text, start_offset);
        let (line_start, line_end) = line_bounds_at(text, reference);
        Range {
            start: offset_to_position(text, line_start),
            end: offset_to_position(text, line_end),
        }
    }
}

fn full_range(text: &str) -> Range {
    Range {
        start: Position::new(0, 0),
        end: offset_to_position(text, text.len()),
    }
}

fn offset_to_position(text: &str, offset: usize) -> Position {
    let mut line = 0u32;
    let mut col = 0u32;
    for (idx, ch) in text.char_indices() {
        if idx >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    Position::new(line, col)
}

fn position_to_offset(text: &str, position: Position) -> usize {
    let mut offset = 0usize;
    let mut current_line = 0u32;
    for line in text.split_inclusive('\n') {
        if current_line == position.line {
            let mut col_bytes = 0usize;
            for ch in line.chars().take(position.character as usize) {
                col_bytes += ch.len_utf8();
            }
            offset += col_bytes;
            return offset;
        }
        offset += line.len();
        current_line += 1;
    }
    text.len()
}

fn completion_trigger_characters() -> Vec<String> {
    const TRIGGER_CHARS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.";
    TRIGGER_CHARS.chars().map(|ch| ch.to_string()).collect()
}

#[derive(Debug)]
struct CallContext {
    name: String,
    arg_index: usize,
}

fn call_context(text: &str, offset: usize) -> Option<CallContext> {
    if text.is_empty() {
        return None;
    }
    let mut idx = offset.min(text.len());
    let bytes = text.as_bytes();
    let mut depth = 0i32;
    while idx > 0 {
        idx -= 1;
        match bytes[idx] {
            b')' | b']' | b'}' => depth += 1,
            b'(' => {
                if depth == 0 {
                    let mut end = idx;
                    while end > 0 && bytes[end - 1].is_ascii_whitespace() {
                        end -= 1;
                    }
                    let mut start = end;
                    while start > 0 && is_ident_char(bytes[start - 1]) {
                        start -= 1;
                    }
                    if start == end {
                        return None;
                    }
                    let name = text[start..end].to_string();
                    let args_slice = &text[idx + 1..offset];
                    let arg_index = argument_index(args_slice);
                    return Some(CallContext { name, arg_index });
                } else {
                    depth -= 1;
                }
            }
            _ => {}
        }
    }
    None
}

fn argument_index(segment: &str) -> usize {
    let mut depth = 0i32;
    let mut count = 0usize;
    for ch in segment.chars() {
        match ch {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => {
                if depth > 0 {
                    depth -= 1;
                }
            }
            ',' if depth == 0 => count += 1,
            _ => {}
        }
    }
    count
}

fn completion_prefix(
    text: &str,
    offset: usize,
    context: Option<&CompletionContext>,
) -> Option<String> {
    let base = identifier_prefix_slice(text, offset).map(str::to_string);
    let trigger = context
        .and_then(|ctx| ctx.trigger_character.as_ref())
        .filter(|ch| is_ident_string(ch));

    match (base, trigger) {
        (Some(mut prefix), Some(trigger)) => {
            if !prefix.ends_with(trigger) {
                prefix.push_str(trigger);
            }
            Some(prefix)
        }
        (Some(prefix), None) => Some(prefix),
        (None, Some(trigger)) => Some(trigger.to_string()),
        _ => None,
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

fn range_to_offsets(text: &str, range: &Range) -> Option<(usize, usize)> {
    let start = position_to_offset(text, range.start);
    let end = position_to_offset(text, range.end);
    if start <= end && end <= text.len() {
        Some((start, end))
    } else {
        None
    }
}

fn prefix_identifier(text: &str, range: &Range) -> String {
    if let Some((start, end)) = range_to_offsets(text, range) {
        let original = text[start..end].to_string();
        if original.starts_with('_') {
            original
        } else {
            format!("_{original}")
        }
    } else {
        "_".into()
    }
}

fn identifier_prefix_slice<'a>(text: &'a str, offset: usize) -> Option<&'a str> {
    if text.is_empty() {
        return None;
    }
    let len = text.len();
    let mut end = offset.min(len);
    while end > 0 && !text.is_char_boundary(end) {
        end -= 1;
    }
    let bytes = text.as_bytes();
    let mut start = end;
    while start > 0 {
        let ch = bytes[start - 1];
        if is_ident_char(ch) {
            start -= 1;
        } else {
            break;
        }
    }
    if start == end {
        None
    } else {
        text.get(start..end)
    }
}

fn prefix_matches(name: &str, prefix: Option<&str>) -> bool {
    match prefix {
        Some(prefix) => name.starts_with(prefix),
        None => true,
    }
}

fn is_ident_string(value: &str) -> bool {
    !value.is_empty() && value.bytes().all(is_ident_char)
}
fn extract_text(text: &str, start: usize, end: usize) -> String {
    let len = text.len();
    if start >= len || start >= end {
        return String::new();
    }
    let end = end.min(len);
    text[start..end].trim().to_string()
}

fn first_non_whitespace_span(text: &str) -> Span {
    for (idx, ch) in text.char_indices() {
        if !ch.is_whitespace() {
            let end = idx + ch.len_utf8();
            return Span::new(idx, end);
        }
    }
    let end = text.chars().next().map(|ch| ch.len_utf8()).unwrap_or(0);
    Span::new(0, end)
}

fn url_to_path(url: &Uri) -> Option<PathBuf> {
    url.to_file_path().map(|cow: Cow<'_, Path>| match cow {
        Cow::Owned(path) => path,
        Cow::Borrowed(path) => path.to_path_buf(),
    })
}

fn manifest_context_for_uri(uri: &Uri) -> Option<(PackageManifest, PathBuf)> {
    let path = url_to_path(uri)?;
    let manifest_path = find_manifest(&path)?;
    let manifest = PackageManifest::load(&manifest_path).ok()?;
    Some((manifest, path))
}

fn adjust_zero_length_offset(text: &str, offset: usize) -> usize {
    if text.is_empty() {
        return 0;
    }
    let len = text.len();
    let mut idx = offset.min(len);
    if idx > 0 {
        idx = prev_char_boundary(text, idx);
    }
    let prefix = &text[..idx];
    for (byte_idx, ch) in prefix.char_indices().rev() {
        if ch == '\n' {
            return byte_idx.saturating_sub(1);
        }
        if !ch.is_whitespace() {
            return byte_idx;
        }
    }
    0
}

fn prev_char_boundary(text: &str, mut idx: usize) -> usize {
    let len = text.len();
    if idx > len {
        idx = len;
    }
    while idx > 0 && !text.is_char_boundary(idx) {
        idx -= 1;
    }
    idx
}

fn line_bounds_at(text: &str, mut offset: usize) -> (usize, usize) {
    if text.is_empty() {
        return (0, 0);
    }
    let len = text.len();
    if offset >= len {
        offset = len - 1;
    }
    let bytes = text.as_bytes();
    let mut start = offset;
    while start > 0 && bytes[start - 1] != b'\n' {
        start -= 1;
    }
    let mut end = offset;
    while end < len && bytes[end] != b'\n' {
        end += 1;
    }
    (start, end)
}

fn error_with_context(base: &str, help: Option<&str>, _text: &str, _span: Span) -> String {
    match help {
        Some(help) if !help.trim().is_empty() => {
            let mut msg = base.to_string();
            msg.push_str("\n");
            msg.push_str(help);
            msg
        }
        _ => base.to_string(),
    }
}

fn prettify_error_message(message: &str) -> String {
    match message {
        "Expected Semi" => "Expected Semicolon ';'".to_string(),
        other => other.to_string(),
    }
}

#[derive(Clone)]
struct StructFieldInfo {
    name: String,
    ty: TypeExpr,
    declared_in: String,
}

#[derive(Clone)]
struct MethodInfo {
    name: String,
    signature: String,
    declared_in: String,
}

#[derive(Clone)]
struct StructInfo {
    fields: Vec<StructFieldInfo>,
    methods: Vec<MethodInfo>,
}

#[derive(Clone)]
struct InterfaceInfo {
    module_name: String,
    type_params: Vec<String>,
    methods: Vec<InterfaceMethod>,
}

#[derive(Default, Clone)]
struct RawStructInfo {
    fields: Vec<StructFieldInfo>,
    embedded: Vec<String>,
    methods: Vec<MethodInfo>,
}

fn collect_struct_info(modules: &[Module]) -> HashMap<String, StructInfo> {
    let mut raw = HashMap::new();
    for module in modules {
        for item in &module.items {
            if let Item::Struct(def) = item {
                let mut struct_fields = Vec::new();
                let mut embedded = Vec::new();
                for field in &def.fields {
                    if let Some(name) = &field.name {
                        struct_fields.push(StructFieldInfo {
                            name: name.clone(),
                            ty: field.ty.ty.clone(),
                            declared_in: def.name.clone(),
                        });
                    } else if field.embedded {
                        if let Some(target) = struct_name_from_type(&field.ty.ty) {
                            embedded.push(target.to_string());
                        }
                    }
                }
                raw.insert(
                    def.name.clone(),
                    RawStructInfo {
                        fields: struct_fields,
                        embedded,
                        methods: Vec::new(),
                    },
                );
            }
        }
    }
    for module in modules {
        for item in &module.items {
            if let Item::Function(func) = item {
                if let Some(first_param) = func.params.first() {
                    if let Some(receiver) = receiver_type_name(&first_param.ty.ty) {
                        if let Some(entry) = raw.get_mut(&receiver) {
                            entry.methods.push(MethodInfo {
                                name: func.name.clone(),
                                signature: format_function_signature(func),
                                declared_in: receiver,
                            });
                        }
                    }
                }
            }
        }
    }
    let mut info = HashMap::new();
    let mut fields_cache = HashMap::new();
    let mut methods_cache = HashMap::new();
    let struct_names: Vec<String> = raw.keys().cloned().collect();
    for name in struct_names {
        let mut field_stack = HashSet::new();
        let fields = flatten_struct_fields(&name, &raw, &mut fields_cache, &mut field_stack);
        let mut method_stack = HashSet::new();
        let methods = flatten_struct_methods(&name, &raw, &mut methods_cache, &mut method_stack);
        info.insert(name.clone(), StructInfo { fields, methods });
    }
    info
}

fn collect_interface_info(modules: &[Module]) -> HashMap<String, Vec<InterfaceInfo>> {
    let mut map = HashMap::new();
    for module in modules {
        for item in &module.items {
            if let Item::Interface(def) = item {
                map.entry(def.name.clone())
                    .or_insert_with(Vec::new)
                    .push(InterfaceInfo {
                        module_name: module.name.clone(),
                        type_params: def.type_params.clone(),
                        methods: def.methods.clone(),
                    });
            }
        }
    }
    map
}

fn module_symbol_definitions(module: &Module) -> Vec<(String, Span, SymbolKind, Visibility)> {
    let mut defs = Vec::new();
    for item in &module.items {
        match item {
            Item::Function(func) => defs.push((
                func.name.clone(),
                func.span,
                SymbolKind::FUNCTION,
                func.visibility,
            )),
            Item::Struct(def) => defs.push((
                def.name.clone(),
                def.span,
                SymbolKind::STRUCT,
                def.visibility,
            )),
            Item::Enum(def) => {
                defs.push((def.name.clone(), def.span, SymbolKind::ENUM, def.visibility));
                for variant in &def.variants {
                    defs.push((
                        variant.name.clone(),
                        variant.span,
                        SymbolKind::ENUM_MEMBER,
                        def.visibility,
                    ));
                }
            }
            Item::Interface(def) => {
                defs.push((
                    def.name.clone(),
                    def.span,
                    SymbolKind::INTERFACE,
                    def.visibility,
                ));
            }
            Item::Const(def) => defs.push((
                def.name.clone(),
                def.span,
                SymbolKind::CONSTANT,
                def.visibility,
            )),
            Item::Impl(_) => {}
        }
    }
    defs
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ModulePathCompletionKind {
    Declaration,
    Import,
}

struct ModulePathCompletionContext {
    kind: ModulePathCompletionKind,
    prefix: Option<String>,
}

fn module_path_completion_context(
    text: &str,
    offset: usize,
) -> Option<ModulePathCompletionContext> {
    if text.is_empty() || offset == 0 {
        return None;
    }
    let line_start = text[..offset].rfind('\n').map(|idx| idx + 1).unwrap_or(0);
    let line = &text[line_start..offset];
    let mut trimmed = line.trim_start();
    if trimmed.starts_with("module ") {
        let after = trimmed["module ".len()..].trim_start();
        return Some(ModulePathCompletionContext {
            kind: ModulePathCompletionKind::Declaration,
            prefix: sanitize_module_prefix(after),
        });
    }
    if trimmed.starts_with("pub ") {
        trimmed = trimmed["pub ".len()..].trim_start();
    }
    if trimmed.starts_with("import ") {
        let after = trimmed["import ".len()..].trim_start();
        return Some(ModulePathCompletionContext {
            kind: ModulePathCompletionKind::Import,
            prefix: sanitize_module_prefix(after),
        });
    }
    None
}

fn sanitize_module_prefix(input: &str) -> Option<String> {
    let mut end = input.len();
    for (idx, ch) in input.char_indices() {
        if ch == ';' {
            end = idx;
            break;
        }
        if ch.is_whitespace() {
            end = idx;
            break;
        }
    }
    let prefix = input[..end].trim().to_string();
    if prefix.is_empty() {
        None
    } else {
        Some(prefix)
    }
}

fn module_completion_items_from_manifest(
    manifest: &PackageManifest,
    prefix: Option<&str>,
    expected: Option<&str>,
) -> Vec<CompletionItem> {
    let entries = manifest.module_entries();
    let mut items = Vec::new();
    let mut seen = HashSet::new();
    if let Some(exp) = expected {
        if module_prefix_matches(exp, prefix) && seen.insert(exp.to_string()) {
            let detail = entries.iter().find(|entry| entry.name == exp).map(|entry| {
                format!(
                    "{} [{}]",
                    entry.path.display(),
                    module_visibility_label(entry.visibility)
                )
            });
            items.push(CompletionItem {
                label: exp.to_string(),
                kind: Some(CompletionItemKind::MODULE),
                sort_text: Some("0".into()),
                detail,
                ..CompletionItem::default()
            });
        }
    }
    for entry in &entries {
        if !module_prefix_matches(&entry.name, prefix) {
            continue;
        }
        if !seen.insert(entry.name.clone()) {
            continue;
        }
        let detail = format!(
            "{} [{}]",
            entry.path.display(),
            module_visibility_label(entry.visibility)
        );
        items.push(CompletionItem {
            label: entry.name.clone(),
            kind: Some(CompletionItemKind::MODULE),
            detail: Some(detail),
            ..CompletionItem::default()
        });
    }
    items
}

fn module_prefix_matches(name: &str, prefix: Option<&str>) -> bool {
    match prefix {
        Some(prefix) => name.starts_with(prefix),
        None => true,
    }
}

fn module_visibility_label(vis: ModuleVisibility) -> &'static str {
    match vis {
        ModuleVisibility::Public => "pub",
        ModuleVisibility::Package => "package",
        ModuleVisibility::Private => "private",
    }
}
fn flatten_struct_fields(
    name: &str,
    raw: &HashMap<String, RawStructInfo>,
    cache: &mut HashMap<String, Vec<StructFieldInfo>>,
    stack: &mut HashSet<String>,
) -> Vec<StructFieldInfo> {
    if let Some(cached) = cache.get(name) {
        return cached.clone();
    }
    if !stack.insert(name.to_string()) {
        return Vec::new();
    }
    let mut fields = Vec::new();
    let mut seen = HashSet::new();
    if let Some(info) = raw.get(name) {
        for field in &info.fields {
            if seen.insert(field.name.clone()) {
                fields.push(field.clone());
            }
        }
        for embedded in &info.embedded {
            let nested = flatten_struct_fields(embedded, raw, cache, stack);
            for field in nested {
                if seen.insert(field.name.clone()) {
                    fields.push(field);
                }
            }
        }
    }
    stack.remove(name);
    cache.insert(name.to_string(), fields.clone());
    fields
}

fn flatten_struct_methods(
    name: &str,
    raw: &HashMap<String, RawStructInfo>,
    cache: &mut HashMap<String, Vec<MethodInfo>>,
    stack: &mut HashSet<String>,
) -> Vec<MethodInfo> {
    if let Some(cached) = cache.get(name) {
        return cached.clone();
    }
    if !stack.insert(name.to_string()) {
        return Vec::new();
    }
    let mut methods = Vec::new();
    let mut seen = HashSet::new();
    if let Some(info) = raw.get(name) {
        for method in &info.methods {
            if seen.insert(method.name.clone()) {
                methods.push(method.clone());
            }
        }
        for embedded in &info.embedded {
            let nested = flatten_struct_methods(embedded, raw, cache, stack);
            for method in nested {
                if seen.insert(method.name.clone()) {
                    methods.push(method);
                }
            }
        }
    }
    stack.remove(name);
    cache.insert(name.to_string(), methods.clone());
    methods
}

fn general_completion_items(
    module: &Module,
    offset: Option<usize>,
    prefix: Option<&str>,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    if let Some(offset) = offset {
        for decl in visible_locals(module, offset) {
            items.push(CompletionItem {
                label: decl.name.clone(),
                kind: Some(CompletionItemKind::VARIABLE),
                detail: decl.ty.as_ref().map(|ty| format_type_expr(ty)),
                ..Default::default()
            });
        }
    }

    for item in &module.items {
        match item {
            Item::Function(func) => {
                if func.name == "main" {
                    continue;
                }
                items.push(CompletionItem {
                    label: func.name.clone(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some(format_function_signature(func)),
                    ..Default::default()
                })
            }
            Item::Struct(def) => items.push(CompletionItem {
                label: def.name.clone(),
                kind: Some(CompletionItemKind::STRUCT),
                detail: Some(format!(
                    "struct {}{}",
                    def.name,
                    format_type_params(&def.type_params)
                )),
                ..Default::default()
            }),
            Item::Enum(def) => items.push(CompletionItem {
                label: def.name.clone(),
                kind: Some(CompletionItemKind::ENUM),
                detail: Some(format!(
                    "enum {}{}",
                    def.name,
                    format_type_params(&def.type_params)
                )),
                ..Default::default()
            }),
            Item::Interface(def) => items.push(CompletionItem {
                label: def.name.clone(),
                kind: Some(CompletionItemKind::INTERFACE),
                detail: Some(format!(
                    "interface {}{}",
                    def.name,
                    format_type_params(&def.type_params)
                )),
                ..Default::default()
            }),
            Item::Const(def) => items.push(CompletionItem {
                label: def.name.clone(),
                kind: Some(CompletionItemKind::CONSTANT),
                detail: def
                    .ty
                    .as_ref()
                    .map(|ty| format_type_expr(&ty.ty))
                    .or(Some("const".into())),
                ..Default::default()
            }),
            Item::Impl(_) => {}
        }
    }

    items.extend(keyword_completion_items(prefix));

    items
        .into_iter()
        .filter(|item| prefix_matches(&item.label, prefix))
        .collect()
}

fn keyword_completion_items(prefix: Option<&str>) -> Vec<CompletionItem> {
    const KEYWORDS: &[&str] = &[
        "fn",
        "let",
        "mut",
        "struct",
        "enum",
        "interface",
        "impl",
        "const",
        "match",
        "if",
        "else",
        "for",
        "while",
        "return",
        "defer",
        "import",
        "break",
        "continue",
    ];
    KEYWORDS
        .iter()
        .filter(|kw| prefix_matches(kw, prefix))
        .map(|kw| CompletionItem {
            label: kw.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            ..Default::default()
        })
        .collect()
}

fn member_completion_items(
    text: &str,
    chain: &[String],
    struct_info: &HashMap<String, StructInfo>,
    interfaces: &HashMap<String, Vec<InterfaceInfo>>,
    prefix: Option<&str>,
    module: &Module,
    offset: usize,
) -> Option<Vec<CompletionItem>> {
    let (target_type, _) = resolve_chain_from_scope(chain, module, struct_info, offset)?;
    let qualifier = if chain.is_empty() {
        None
    } else {
        Some(chain.join("."))
    };
    let edit_range = member_completion_edit_range(text, offset, prefix, qualifier.as_deref());
    if let Some((name, args)) = named_type_with_args(&target_type) {
        if let Some(info) = struct_info.get(&name) {
            let mut items = Vec::new();
            for field in &info.fields {
                if prefix_matches(&field.name, prefix) {
                    let filter_text = qualifier
                        .as_ref()
                        .map(|qual| format!("{qual}.{}", field.name));
                    let new_text = qualifier
                        .as_ref()
                        .map(|qual| format!("{qual}.{}", field.name))
                        .unwrap_or_else(|| field.name.clone());
                    items.push(CompletionItem {
                        label: field.name.clone(),
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some(format!(
                            "{} (from {})",
                            format_type_expr(&field.ty),
                            field.declared_in
                        )),
                        filter_text,
                        text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                            range: edit_range.clone(),
                            new_text,
                        })),
                        ..Default::default()
                    });
                }
            }
            for method in &info.methods {
                if prefix_matches(&method.name, prefix) {
                    let filter_text = qualifier
                        .as_ref()
                        .map(|qual| format!("{qual}.{}", method.name));
                    let new_text = qualifier
                        .as_ref()
                        .map(|qual| format!("{qual}.{}", method.name))
                        .unwrap_or_else(|| method.name.clone());
                    items.push(CompletionItem {
                        label: method.name.clone(),
                        kind: Some(CompletionItemKind::METHOD),
                        detail: Some(format!(
                            "{} (from {})",
                            method.signature, method.declared_in
                        )),
                        filter_text,
                        text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                            range: edit_range.clone(),
                            new_text,
                        })),
                        ..Default::default()
                    });
                }
            }
            if items.is_empty() { None } else { Some(items) }
        } else if let Some(info) = select_interface_info(interfaces, &name, &module.name) {
            let subst = build_type_subst(&info.type_params, &args);
            let subst_ref = subst.as_ref();
            let mut items = Vec::new();
            for method in &info.methods {
                if prefix_matches(&method.name, prefix) {
                    let filter_text = qualifier
                        .as_ref()
                        .map(|qual| format!("{qual}.{}", method.name));
                    let new_text = qualifier
                        .as_ref()
                        .map(|qual| format!("{qual}.{}", method.name))
                        .unwrap_or_else(|| method.name.clone());
                    items.push(CompletionItem {
                        label: method.name.clone(),
                        kind: Some(CompletionItemKind::METHOD),
                        detail: Some(format!(
                            "{} (from {})",
                            format_interface_method_signature(method, subst_ref),
                            name
                        )),
                        filter_text,
                        text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                            range: edit_range.clone(),
                            new_text,
                        })),
                        ..Default::default()
                    });
                }
            }
            if items.is_empty() { None } else { Some(items) }
        } else {
            None
        }
    } else {
        None
    }
}

fn member_completion_edit_range(
    text: &str,
    offset: usize,
    prefix: Option<&str>,
    qualifier: Option<&str>,
) -> Range {
    let prefix_len = prefix.map(|p| p.len()).unwrap_or(0);
    let qualifier_len = qualifier.map(|q| q.len()).unwrap_or(0);
    let dot_len = if qualifier.is_some() { 1 } else { 0 };
    let replace_len = prefix_len + qualifier_len + dot_len;
    let start_offset = offset.saturating_sub(replace_len);
    Range {
        start: offset_to_position(text, start_offset),
        end: offset_to_position(text, offset),
    }
}

fn named_type_with_args(ty: &TypeExpr) -> Option<(String, Vec<TypeExpr>)> {
    match ty {
        TypeExpr::Named(name, args) => Some((name.clone(), args.clone())),
        TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => named_type_with_args(ty),
        _ => None,
    }
}

fn build_type_subst(params: &[String], args: &[TypeExpr]) -> Option<HashMap<String, TypeExpr>> {
    if params.is_empty() || params.len() != args.len() {
        return None;
    }
    let mut map = HashMap::new();
    for (param, arg) in params.iter().zip(args.iter()) {
        map.insert(param.clone(), arg.clone());
    }
    Some(map)
}
fn visible_locals(module: &Module, offset: usize) -> Vec<DeclInfo> {
    collect_decl_spans(module)
        .into_iter()
        .filter(|decl| scope_contains(decl.scope, offset))
        .filter(|decl| offset >= decl.available_from)
        .collect()
}

fn receiver_type_name(expr: &TypeExpr) -> Option<String> {
    match expr {
        TypeExpr::Named(name, _) => Some(name.clone()),
        TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => receiver_type_name(ty),
        _ => None,
    }
}

fn format_type_expr(expr: &TypeExpr) -> String {
    match expr {
        TypeExpr::Named(name, args) => {
            if args.is_empty() {
                name.clone()
            } else {
                let params = args
                    .iter()
                    .map(format_type_expr)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{name}[{params}]")
            }
        }
        TypeExpr::Slice(inner) => format!("[]{}", format_type_expr(inner)),
        TypeExpr::Array { size, ty } => format!("[{}]{}", size, format_type_expr(ty)),
        TypeExpr::Reference { mutable, ty } => {
            if *mutable {
                format!("&mut {}", format_type_expr(ty))
            } else {
                format!("&{}", format_type_expr(ty))
            }
        }
        TypeExpr::Pointer { mutable, ty } => {
            if *mutable {
                format!("*mut {}", format_type_expr(ty))
            } else {
                format!("*{}", format_type_expr(ty))
            }
        }
        TypeExpr::Tuple(types) => {
            let inner = types
                .iter()
                .map(format_type_expr)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", inner)
        }
        TypeExpr::Unit => "()".into(),
        TypeExpr::SelfType => "Self".into(),
    }
}

fn format_type_params(params: &[String]) -> String {
    if params.is_empty() {
        String::new()
    } else {
        format!("[{}]", params.join(", "))
    }
}

fn format_type_arguments(args: &[TypeExpr]) -> String {
    if args.is_empty() {
        String::new()
    } else {
        let rendered = args
            .iter()
            .map(format_type_expr)
            .collect::<Vec<_>>()
            .join(", ");
        format!("[{}]", rendered)
    }
}

fn expression_chain_before_dot(text: &str, offset: usize) -> Option<Vec<String>> {
    if offset == 0 {
        return None;
    }
    let bytes = text.as_bytes();
    let mut idx = skip_ws_back(text, offset);
    if idx > 0 && bytes[idx - 1].is_ascii_alphanumeric() {
        idx = skip_identifier(text, idx);
        idx = skip_ws_back(text, idx);
    }
    if idx == 0 || bytes[idx - 1] != b'.' {
        return None;
    }
    idx -= 1;
    collect_chain_segments(text, idx)
}

fn chain_for_field_token(text: &str, span: Span) -> Option<Vec<String>> {
    if span.start >= span.end || span.end > text.len() {
        return None;
    }
    let current = text.get(span.start..span.end)?.to_string();
    let bytes = text.as_bytes();
    let mut idx = skip_ws_back(text, span.start);
    if idx == 0 || bytes[idx - 1] != b'.' {
        return None;
    }
    idx -= 1;
    let mut segments = collect_chain_segments(text, idx)?;
    segments.push(current);
    if segments.len() < 2 {
        return None;
    }
    Some(segments)
}

fn skip_ws_back(text: &str, mut idx: usize) -> usize {
    let bytes = text.as_bytes();
    while idx > 0 {
        if bytes[idx - 1].is_ascii_whitespace() {
            idx -= 1;
        } else {
            break;
        }
    }
    idx
}

fn skip_identifier(text: &str, mut idx: usize) -> usize {
    let bytes = text.as_bytes();
    while idx > 0 {
        let ch = bytes[idx - 1];
        if is_ident_char(ch) {
            idx -= 1;
        } else {
            break;
        }
    }
    idx
}

fn collect_chain_segments(text: &str, mut idx: usize) -> Option<Vec<String>> {
    let bytes = text.as_bytes();
    let mut segments = Vec::new();
    loop {
        idx = skip_ws_back(text, idx);
        if idx == 0 {
            break;
        }
        let end = idx;
        let mut start = end;
        while start > 0 {
            let ch = bytes[start - 1];
            if ch.is_ascii_alphanumeric() || ch == b'_' {
                start -= 1;
            } else {
                break;
            }
        }
        if start == end {
            return None;
        }
        segments.push(text[start..end].to_string());
        idx = start;
        idx = skip_ws_back(text, idx);
        if idx == 0 || bytes[idx - 1] != b'.' {
            break;
        }
        idx -= 1;
    }
    if segments.is_empty() {
        None
    } else {
        segments.reverse();
        Some(segments)
    }
}

fn is_ident_char(ch: u8) -> bool {
    ch.is_ascii_alphanumeric() || ch == b'_'
}

fn resolve_chain_from_root<'a>(
    chain: &[String],
    root: TypeExpr,
    structs: &'a HashMap<String, StructInfo>,
) -> Option<(TypeExpr, Option<(String, &'a StructFieldInfo)>)> {
    if chain.is_empty() {
        return None;
    }
    let mut current = root;
    let mut last_field = None;
    for segment in chain.iter().skip(1) {
        let struct_name = struct_name_from_type(&current)?.to_string();
        let info = structs.get(&struct_name)?;
        let field = info.fields.iter().find(|f| f.name == *segment)?;
        current = field.ty.clone();
        last_field = Some((struct_name, field));
    }
    Some((current, last_field))
}

fn resolve_chain_from_scope<'a>(
    chain: &[String],
    module: &Module,
    structs: &'a HashMap<String, StructInfo>,
    offset: usize,
) -> Option<(TypeExpr, Option<(String, &'a StructFieldInfo)>)> {
    let root = chain.first()?;
    let ty = identifier_type_from_scope(module, structs, root, offset)?;
    resolve_chain_from_root(chain, ty, structs)
}

fn identifier_type_from_scope(
    module: &Module,
    structs: &HashMap<String, StructInfo>,
    name: &str,
    offset: usize,
) -> Option<TypeExpr> {
    if let Some(decl) = find_local_decl(module, name, offset) {
        if let Some(ty) = decl.ty {
            return Some(ty);
        }
    }
    if structs.contains_key(name) {
        return Some(TypeExpr::Named(name.to_string(), Vec::new()));
    }
    None
}

fn struct_name_from_type<'a>(ty: &'a TypeExpr) -> Option<&'a str> {
    match ty {
        TypeExpr::Named(name, _) => Some(name),
        TypeExpr::Reference { ty, .. } | TypeExpr::Pointer { ty, .. } => struct_name_from_type(ty),
        _ => None,
    }
}

fn expr_span(expr: &Expr) -> Span {
    match expr {
        Expr::Identifier(ident) => ident.span,
        Expr::Literal(Literal::Int(_, span))
        | Expr::Literal(Literal::Float(_, span))
        | Expr::Literal(Literal::Bool(_, span))
        | Expr::Literal(Literal::String(_, span))
        | Expr::Literal(Literal::Rune(_, span)) => *span,
        Expr::Binary { span, .. } => *span,
        Expr::Unary { span, .. } => *span,
        Expr::Call { span, .. } => *span,
        Expr::FieldAccess { span, .. } => *span,
        Expr::StructLiteral { span, .. } => *span,
        Expr::EnumLiteral { span, .. } => *span,
        Expr::Block(block) => block.span,
        Expr::If(if_expr) => if_expr.span,
        Expr::Match(match_expr) => match_expr.span,
        Expr::Tuple(_, span) => *span,
        Expr::ArrayLiteral(_, span) => *span,
        Expr::Range(range) => range.span,
        Expr::Reference { span, .. } => *span,
        Expr::Deref { span, .. } => *span,
        Expr::Move { span, .. } => *span,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn interface_member_completion_infers_methods() {
        let source = r#"
module demos::interface_generics;

interface Nameable[T] {
  fn label(self) -> string;
  fn pair(self, other: T) -> string;
}

fn announce[T](unit: Nameable[T], partner: T) {
  unit.label();
  unit.pair(partner);
}
"#;

        let module = parse_module(
            "demos::interface_generics",
            PathBuf::from("demo.prime"),
            source,
        )
        .unwrap();
        let structs = vec![module.clone()];
        let struct_info = collect_struct_info(&structs);
        let interface_info = collect_interface_info(&structs);
        let chain = vec!["unit".to_string()];
        let items = member_completion_items(
            source,
            &chain,
            &struct_info,
            &interface_info,
            None,
            &module,
            source.find("unit.label").unwrap_or(0),
        )
        .expect("completion items");
        let mut labels = Vec::new();
        let mut label_filter = None;
        for item in &items {
            labels.push(item.label.as_str());
            if item.label == "label" {
                label_filter = item.filter_text.clone();
            }
        }
        assert!(labels.contains(&"label"));
        assert!(labels.contains(&"pair"));
        assert_eq!(label_filter.as_deref(), Some("unit.label"));
    }

    #[test]
    fn struct_member_completion_exposes_embedded_fields() {
        let types_source = r#"
module core::types;

struct Vec2 {
  x: float32;
  y: float32;
}

struct Transform {
  position: Vec2;
  velocity: Vec2;
}

struct Stats {
  hp: int32;
  stamina: int32;
}

struct Player {
  Transform;
  Stats;
  name: string;
  level: int32;
}
"#;

        let main_source = r#"
module app::main;

import core::types;

fn heal(player: Player, boost: int32) -> Player {
  player.hp;
  player.position;
}
"#;

        let types_module =
            parse_module("core::types", PathBuf::from("types.prime"), types_source).unwrap();
        let main_module =
            parse_module("app::main", PathBuf::from("main.prime"), main_source).unwrap();
        let struct_modules = vec![types_module.clone(), main_module.clone()];
        let struct_info = collect_struct_info(&struct_modules);
        let interface_info = collect_interface_info(&struct_modules);
        let offset = main_source.find("player.hp").unwrap() + "player.".len();
        let chain = expression_chain_before_dot(main_source, offset).unwrap();
        let items = member_completion_items(
            main_source,
            &chain,
            &struct_info,
            &interface_info,
            None,
            &main_module,
            offset,
        )
        .expect("member completion items");
        let mut labels = Vec::new();
        let mut hp_filter = None;
        for item in &items {
            labels.push(item.label.as_str());
            if item.label == "hp" {
                hp_filter = item.filter_text.clone();
            }
        }
        assert!(labels.contains(&"hp"));
        assert!(labels.contains(&"stamina"));
        assert!(labels.contains(&"position"));
        assert!(labels.contains(&"velocity"));
        assert_eq!(hp_filter.as_deref(), Some("player.hp"));
    }

    #[test]
    fn member_completion_survives_incomplete_field_access() {
        let types_source = r#"
module core::types;

struct Vec2 {
  x: float32;
  y: float32;
}

struct Transform {
  position: Vec2;
  velocity: Vec2;
}

struct Stats {
  hp: int32;
  stamina: int32;
}

struct Player {
  Transform;
  Stats;
  name: string;
  level: int32;
}
"#;

        let ok_source = r#"
module app::main;

import core::types;

fn heal(player: Player, boost: int32) -> Player {
  player.hp + boost;
}
"#;

        let incomplete_source = r#"
module app::main;

import core::types;

fn heal(player: Player, boost: int32) -> Player {
  player.;
}
"#;

        let types_module =
            parse_module("core::types", PathBuf::from("types.prime"), types_source).unwrap();
        let ok_module = parse_module("app::main", PathBuf::from("main.prime"), ok_source).unwrap();
        let modules = vec![types_module.clone(), ok_module.clone()];
        let struct_info = collect_struct_info(&modules);
        let interface_info = collect_interface_info(&modules);
        let offset = incomplete_source.find("player.").unwrap() + "player.".len();
        let chain = expression_chain_before_dot(incomplete_source, offset).unwrap();
        let items = member_completion_items(
            incomplete_source,
            &chain,
            &struct_info,
            &interface_info,
            None,
            &ok_module,
            offset,
        )
        .expect("items");
        let mut labels = Vec::new();
        let mut hp_filter = None;
        for item in &items {
            labels.push(item.label.as_str());
            if item.label == "hp" {
                hp_filter = item.filter_text.clone();
            }
        }
        assert!(labels.contains(&"hp"));
        assert_eq!(hp_filter.as_deref(), Some("player.hp"));
    }

    #[test]
    fn nested_member_completion_lists_child_fields() {
        let types_source = r#"
module core::types;

struct Vec2 {
  x: float32;
  y: float32;
}

struct Transform {
  position: Vec2;
  velocity: Vec2;
}

struct Player {
  Transform;
}
"#;

        let ok_source = r#"
module app::main;

import core::types;

fn heal(player: Player) {
  player.position.x;
}
"#;

        let incomplete_source = r#"
module app::main;

import core::types;

fn heal(player: Player) {
  player.position.;
}
"#;

        let types_module =
            parse_module("core::types", PathBuf::from("types.prime"), types_source).unwrap();
        let ok_module = parse_module("app::main", PathBuf::from("main.prime"), ok_source).unwrap();
        let struct_modules = vec![types_module.clone(), ok_module.clone()];
        let struct_info = collect_struct_info(&struct_modules);
        let interface_info = collect_interface_info(&struct_modules);
        let offset = incomplete_source.find("player.position.").unwrap() + "player.position.".len();
        let chain = expression_chain_before_dot(incomplete_source, offset).unwrap();
        let items = member_completion_items(
            incomplete_source,
            &chain,
            &struct_info,
            &interface_info,
            None,
            &ok_module,
            offset,
        )
        .expect("nested items");
        let labels: Vec<_> = items.iter().map(|item| item.label.as_str()).collect();
        assert!(labels.contains(&"x"));
        assert!(labels.contains(&"y"));
    }

    #[test]
    fn player_panel_render_completion_has_vec2_fields() {
        use std::fs;
        let types_source = fs::read_to_string("types.prime").expect("types.prime");
        let main_source = fs::read_to_string("main.prime").expect("main.prime");
        let types_module =
            parse_module("core::types", PathBuf::from("types.prime"), &types_source).unwrap();
        let main_module =
            parse_module("app::main", PathBuf::from("main.prime"), &main_source).unwrap();
        let modules = vec![types_module.clone(), main_module.clone()];
        let struct_info = collect_struct_info(&modules);
        let interface_info = collect_interface_info(&modules);
        let offset = main_source.find("self.position.").unwrap() + "self.position.".len();
        let chain = expression_chain_before_dot(&main_source, offset).unwrap();
        let items = member_completion_items(
            &main_source,
            &chain,
            &struct_info,
            &interface_info,
            None,
            &main_module,
            offset,
        )
        .expect("player panel items");
        let labels: Vec<_> = items.iter().map(|item| item.label.as_str()).collect();
        assert!(labels.contains(&"x"));
        assert!(labels.contains(&"y"));
    }
}
