use super::{
    analysis::{
        find_local_decl, find_local_definition_span, find_module_item_span, identifier_at_offset,
        unused_variable_diagnostics,
    },
    completion::{
        ModulePathCompletionKind, collect_interface_info, collect_struct_info, completion_prefix,
        completion_trigger_characters, expression_chain_before_dot, format_function_param,
        format_function_signature, general_completion_items, keyword_completion_items,
        member_completion_items, module_completion_items_from_manifest,
        module_path_completion_context,
    },
    diagnostics::{collect_parse_and_manifest_diagnostics, diagnostic_code, manifest_entry_action},
    hover::{collect_var_infos, hover_for_token},
    parser::parse_module_from_uri,
    text::{
        collect_identifier_spans, collect_identifier_spans_in_scope, full_range, identifier_at,
        is_valid_identifier, manifest_context_for_uri, position_to_offset, prefix_identifier,
        span_to_range, token_at, url_to_path,
    },
};
use crate::project::{
    diagnostics::{CODE_MANIFEST_MISSING_MODULE, CODE_MISSING_MODULE_HEADER},
    find_manifest, load_package,
    manifest::PackageManifest,
};
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
use tower_lsp_server::lsp_types::request::{
    GotoDeclarationParams, GotoDeclarationResponse, GotoImplementationParams,
    GotoTypeDefinitionParams,
};
use tower_lsp_server::lsp_types::{
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
    TypeDefinitionProviderCapability, Uri, WorkspaceEdit, WorkspaceSymbol, WorkspaceSymbolParams,
};
use tower_lsp_server::{Client, LanguageServer, UriExt};

#[derive(Default)]
struct Documents {
    inner: RwLock<HashMap<Uri, String>>,
}

impl Documents {
    async fn insert(&self, uri: Uri, text: String) {
        self.inner.write().await.insert(uri, text);
    }

    #[allow(dead_code)]
    #[allow(dead_code)]
    async fn remove(&self, uri: &Uri) {
        self.inner.write().await.remove(uri);
    }

    async fn get(&self, uri: &Uri) -> Option<String> {
        self.inner.read().await.get(uri).cloned()
    }
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
    module_kind: ModuleKind,
    visibility: Visibility,
}

#[derive(Default)]
struct SymbolIndex {
    inner: RwLock<HashMap<String, Vec<SymbolLocation>>>,
}

impl SymbolIndex {
    async fn update_module(&self, uri: &Uri, module: &Module) {
        let mut map = self.inner.write().await;
        for locations in map.values_mut() {
            locations.retain(|loc| &loc.uri != uri);
        }
        map.retain(|_, locations| !locations.is_empty());
        for (name, span, kind, visibility, module_kind) in module_symbol_definitions(module) {
            map.entry(name).or_default().push(SymbolLocation {
                uri: uri.clone(),
                span,
                kind,
                module_name: module.name.clone(),
                module_kind,
                visibility,
            });
        }
    }

    #[allow(dead_code)]
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

    async fn duplicates_for_uri(&self, uri: &Uri) -> Vec<(String, Span, Vec<Uri>)> {
        let map = self.inner.read().await;
        let mut results = Vec::new();
        for (name, locs) in map.iter() {
            if locs.len() < 2 {
                continue;
            }
            let others: Vec<Uri> = locs
                .iter()
                .filter(|loc| loc.module_kind == ModuleKind::Library && &loc.uri != uri)
                .map(|loc| loc.uri.clone())
                .collect();
            if others.is_empty() {
                continue;
            }
            for loc in locs
                .iter()
                .filter(|loc| loc.module_kind == ModuleKind::Library && &loc.uri == uri)
            {
                results.push((name.clone(), loc.span, others.clone()));
            }
        }
        results
    }
}

fn module_symbol_definitions(
    module: &Module,
) -> Vec<(String, Span, SymbolKind, Visibility, ModuleKind)> {
    let mut defs = Vec::new();
    for item in &module.items {
        match item {
            Item::Function(func) => defs.push((
                func.name.clone(),
                func.span,
                SymbolKind::FUNCTION,
                func.visibility,
                module.kind,
            )),
            Item::Struct(def) => defs.push((
                def.name.clone(),
                def.span,
                SymbolKind::STRUCT,
                def.visibility,
                module.kind,
            )),
            Item::Enum(def) => {
                defs.push((
                    def.name.clone(),
                    def.span,
                    SymbolKind::ENUM,
                    def.visibility,
                    module.kind,
                ));
                for variant in &def.variants {
                    defs.push((
                        variant.name.clone(),
                        variant.span,
                        SymbolKind::ENUM_MEMBER,
                        def.visibility,
                        module.kind,
                    ));
                }
            }
            Item::Interface(def) => {
                defs.push((
                    def.name.clone(),
                    def.span,
                    SymbolKind::INTERFACE,
                    def.visibility,
                    module.kind,
                ));
            }
            Item::Const(def) => defs.push((
                def.name.clone(),
                def.span,
                SymbolKind::CONSTANT,
                def.visibility,
                module.kind,
            )),
            Item::Impl(_) => {}
        }
    }
    defs
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
                    while start > 0
                        && (bytes[start - 1].is_ascii_alphanumeric() || bytes[start - 1] == b'_')
                    {
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
    if let Some(loc) = candidates.iter().find(|loc| {
        matches!(loc.module_kind, ModuleKind::Module) && loc.visibility == Visibility::Public
    }) {
        return Some(loc);
    }
    if let Some(loc) = candidates
        .iter()
        .find(|loc| matches!(loc.visibility, Visibility::Public))
    {
        return Some(loc);
    }
    candidates.first()
}

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

impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> RpcResult<InitializeResult> {
        #[allow(deprecated)]
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
                declaration_provider: Some(DeclarationCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                implementation_provider: Some(ImplementationProviderCapability::Simple(true)),
                type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
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
            if let Some(hover) = hover_for_token(
                &text,
                token,
                &vars,
                module.as_ref(),
                struct_info.as_ref(),
                Some(&struct_modules),
            ) {
                return Ok(Some(hover));
            }
        }
        if let Some(module) = module.as_ref() {
            if let Some((name, span)) = identifier_at_offset(module, offset) {
                let synthetic = Token {
                    kind: TokenKind::Identifier(name),
                    span,
                };
                if let Some(hover) = hover_for_token(
                    &text,
                    &synthetic,
                    &vars,
                    Some(module),
                    struct_info.as_ref(),
                    Some(&struct_modules),
                ) {
                    return Ok(Some(hover));
                }
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
        if let Some(location) = self.resolve_symbol_location(&uri, position).await {
            return Ok(Some(GotoDefinitionResponse::Scalar(location)));
        }
        Ok(None)
    }

    async fn goto_declaration(
        &self,
        params: GotoDeclarationParams,
    ) -> RpcResult<Option<GotoDeclarationResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        if let Some(location) = self.resolve_symbol_location(&uri, position).await {
            return Ok(Some(GotoDeclarationResponse::Scalar(location)));
        }
        Ok(None)
    }

    async fn goto_type_definition(
        &self,
        params: GotoTypeDefinitionParams,
    ) -> RpcResult<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        if let Some(location) = self.resolve_type_location(&uri, position).await {
            return Ok(Some(GotoDefinitionResponse::Scalar(location)));
        }
        Ok(None)
    }

    async fn goto_implementation(
        &self,
        params: GotoImplementationParams,
    ) -> RpcResult<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        if let Some(location) = self.resolve_impl_location(&uri, position).await {
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
        let module = self.parse_cached_module(&uri, &text).await;
        let scoped_spans = module
            .as_ref()
            .and_then(|module| find_local_decl(module, &target_name, offset))
            .map(|decl| collect_identifier_spans_in_scope(&tokens, &target_name, decl.scope));
        let spans = if let Some(spans) = scoped_spans {
            if spans.is_empty() {
                collect_identifier_spans(&tokens, &target_name)
            } else {
                spans
            }
        } else {
            collect_identifier_spans(&tokens, &target_name)
        };
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

            let general_items =
                general_completion_items(&module, &struct_modules, Some(offset), prefix_ref);
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
            Item::Interface(def) => {
                symbols.push(make_symbol(
                    uri,
                    text,
                    &def.name,
                    SymbolKind::INTERFACE,
                    def.span,
                ));
                for method in &def.methods {
                    symbols.push(make_symbol(
                        uri,
                        text,
                        &format!("{}::{}", def.name, method.name),
                        SymbolKind::METHOD,
                        method.span,
                    ));
                }
            }
            Item::Const(def) => symbols.push(make_symbol(
                uri,
                text,
                &def.name,
                SymbolKind::CONSTANT,
                def.span,
            )),
            Item::Impl(block) => {
                if let Some(first) = block.methods.first() {
                    symbols.push(make_symbol(
                        uri,
                        text,
                        &format!("impl {} for {}", block.interface, block.target),
                        SymbolKind::INTERFACE,
                        first.span,
                    ));
                }
                for method in &block.methods {
                    symbols.push(make_symbol(
                        uri,
                        text,
                        &format!("{}::{}", block.target, method.name),
                        SymbolKind::METHOD,
                        method.span,
                    ));
                }
            }
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
