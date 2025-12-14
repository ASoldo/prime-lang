use super::*;

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
        let module = self.parse_cached_module(&uri, &text).await;
        let offset = position_to_offset(&text, position);
        let target = identifier_at(&tokens, offset).or_else(|| {
            module
                .as_ref()
                .and_then(|module| identifier_at_offset(module, offset))
        });
        let Some((name, span)) = target else {
            return Ok(None);
        };
        if !is_valid_identifier(&name) {
            return Ok(None);
        }
        let range = span_to_range(&text, span);
        Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
            range,
            placeholder: name,
        }))
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
        let module = self.parse_cached_module(&uri, &text).await;
        let target = identifier_at(&tokens, offset).or_else(|| {
            module
                .as_ref()
                .and_then(|module| identifier_at_offset(module, offset))
        });
        let Some((target_name, _)) = target else {
            return Ok(None);
        };
        let mut spans = Vec::new();
        if let Some(module) = module.as_ref() {
            if let Some(decl) = find_local_decl(module, &target_name, offset) {
                spans.extend(collect_identifier_spans_for_decl(module, &tokens, &decl));
            }
        }
        if spans.is_empty() {
            spans.extend(collect_identifier_spans(&tokens, &target_name));
            if let Some(module) = module.as_ref() {
                spans.extend(collect_identifier_spans_ast(module, &target_name));
            }
        }
        spans.sort_by(|a, b| a.start.cmp(&b.start).then_with(|| a.end.cmp(&b.end)));
        spans.dedup();
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
        let build_options = if let Some((manifest, _)) = manifest_context_for_uri(&uri) {
            BuildOptions::from_sources(None, None, Some(&manifest))
        } else {
            BuildOptions::default()
        };
        if let Some((manifest, _)) = manifest_context_for_uri(&uri) {
            self.ensure_manifest_modules(&manifest).await;
        } else if let Some(path) = url_to_path(&uri) {
            self.ensure_package_modules(&path).await;
        }
        let module_opt = self.parse_cached_module(&uri, &text).await;
        let offset = position_to_offset(&text, position);
        let struct_modules = self
            .modules
            .snapshot()
            .await
            .into_iter()
            .map(|(_, module)| module)
            .collect::<Vec<_>>();
        if let Some(ctx) = module_path_completion_context(&text, offset) {
            match ctx.kind {
                ModulePathCompletionKind::Declaration | ModulePathCompletionKind::Import => {
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
                ModulePathCompletionKind::ImportSelectors => {
                    if let Some(module_name) = ctx.prefix.as_deref() {
                        let items =
                            module_selector_items_from_modules(module_name, &struct_modules);
                        if !items.is_empty() {
                            return Ok(Some(CompletionResponse::Array(items)));
                        }
                    }
                }
            }
        }
        let prefix = completion_prefix(&text, offset, context.as_ref());
        let prefix_ref = prefix.as_deref();
        let struct_info = collect_struct_info(&struct_modules);
        let interface_info = collect_interface_info(&struct_modules);
        if let Some(module) = module_opt {
            if let Some(items) =
                enum_variant_completion_items(&text, offset, Some(&module), &struct_modules)
            {
                return Ok(Some(CompletionResponse::Array(items)));
            }
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

            let general_items = general_completion_items(
                &module,
                &struct_modules,
                Some(offset),
                prefix_ref,
                &build_options.target,
            );
            Ok(Some(CompletionResponse::Array(general_items)))
        } else {
            let keywords =
                keyword_completion_items(prefix_ref, false, false, Some(&build_options.target));
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
        let only_modules: Vec<Module> = modules.into_iter().map(|(_, m)| m).collect();
        Ok(signature_help_from_modules(&only_modules, &ctx))
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
    ) -> RpcResult<Option<WorkspaceSymbolResponse>> {
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
            Ok(Some(WorkspaceSymbolResponse::Flat(symbols)))
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
