use crate::{
    formatter::format_module,
    language::{
        ast::{Block, Expr, FunctionBody, Item, Module, RangeExpr, Statement, StructLiteralKind},
        errors::{SyntaxError, SyntaxErrors},
        lexer::{LexError, lex},
        parser::parse_module,
        span::Span,
        token::{Token, TokenKind},
    },
};
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    error::Error,
    path::{Path, PathBuf},
    sync::Arc,
};
use tokio::runtime::Runtime;
use tokio::sync::RwLock;
use tower_lsp_server::jsonrpc::Result as RpcResult;
use tower_lsp_server::lsp_types::{
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams,
    DocumentSymbolParams, DocumentSymbolResponse, Hover, HoverContents, HoverParams,
    HoverProviderCapability, InitializeParams, InitializeResult, InitializedParams, Location,
    MarkupContent, MarkupKind, MessageType, OneOf, Position, PrepareRenameResponse, Range,
    RenameParams, ServerCapabilities, SymbolInformation, SymbolKind, TextDocumentPositionParams,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Uri, WorkspaceEdit,
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

struct Backend {
    client: Client,
    docs: Arc<Documents>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            docs: Arc::new(Documents::default()),
        }
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
}

impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> RpcResult<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
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
        self.docs
            .insert(uri.clone(), params.text_document.text)
            .await;
        self.publish_diagnostics(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.last() {
            let uri = params.text_document.uri.clone();
            self.docs.insert(uri.clone(), change.text.clone()).await;
            self.publish_diagnostics(&uri).await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if let Some(text) = params.text {
            let uri = params.text_document.uri.clone();
            self.docs.insert(uri.clone(), text).await;
            self.publish_diagnostics(&uri).await;
        } else {
            self.publish_diagnostics(&params.text_document.uri).await;
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
        let tokens = lex(&text).ok();
        let Some(tokens) = tokens else {
            return Ok(None);
        };
        let vars = collect_var_infos(&text, &tokens);
        let offset = position_to_offset(&text, position);
        if let Some(token) = token_at(&tokens, offset) {
            if let Some(hover) = hover_for_token(&text, token, &vars) {
                return Ok(Some(hover));
            }
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
        let Some(module) = parse_module_from_uri(&uri, &text).ok() else {
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
            Item::Const(def) => symbols.push(make_symbol(
                uri,
                text,
                &def.name,
                SymbolKind::CONSTANT,
                def.span,
            )),
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
}

fn collect_decl_spans(module: &Module) -> Vec<DeclInfo> {
    let mut decls = Vec::new();
    for item in &module.items {
        if let Item::Function(func) = item {
            if let FunctionBody::Block(block) = &func.body {
                collect_decl_from_block(block, &mut decls);
            }
        }
    }
    decls
}

fn collect_decl_from_block(block: &Block, decls: &mut Vec<DeclInfo>) {
    for statement in &block.statements {
        match statement {
            Statement::Let(stmt) => decls.push(DeclInfo {
                name: stmt.name.clone(),
                span: stmt.span,
            }),
            Statement::Block(inner) => collect_decl_from_block(inner, decls),
            Statement::If(if_stmt) => {
                collect_decl_from_block(&if_stmt.then_branch, decls);
                if let Some(else_branch) = &if_stmt.else_branch {
                    collect_decl_from_block(else_branch, decls);
                }
            }
            Statement::While(while_stmt) => collect_decl_from_block(&while_stmt.body, decls),
            Statement::ForRange(for_stmt) => collect_decl_from_block(&for_stmt.body, decls),
            Statement::Match(match_stmt) => {
                for arm in &match_stmt.arms {
                    collect_decl_from_block(&arm.body, decls);
                }
            }
            _ => {}
        }
    }
    if let Some(tail) = &block.tail {
        collect_decl_from_expr(tail, decls);
    }
}

fn collect_decl_from_expr(expr: &Expr, decls: &mut Vec<DeclInfo>) {
    match expr {
        Expr::Block(block) => collect_decl_from_block(block, decls),
        Expr::If(if_expr) => {
            collect_decl_from_block(&if_expr.then_branch, decls);
            if let Some(else_branch) = &if_expr.else_branch {
                collect_decl_from_block(else_branch, decls);
            }
        }
        Expr::Match(match_expr) => {
            for arm in &match_expr.arms {
                collect_decl_from_expr(&arm.value, decls);
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
        Item::Function(func) => match &func.body {
            FunctionBody::Block(block) => collect_used_in_block(block, used),
            FunctionBody::Expr(expr) => collect_expr_idents(&expr.node, used),
        },
        Item::Const(def) => collect_expr_idents(&def.value, used),
        _ => {}
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
        Expr::Call { callee, args, .. } => {
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

fn hover_for_token(text: &str, token: &Token, vars: &[VarInfo]) -> Option<Hover> {
    let span = token.span;
    let hover = match &token.kind {
        TokenKind::Identifier(name) => {
            if name == "out" {
                Some(
                    "Built-in output function **out(expr)**\n\nPrints the evaluated expression."
                        .to_string(),
                )
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

fn extract_text(text: &str, start: usize, end: usize) -> String {
    let len = text.len();
    if start >= len || start >= end {
        return String::new();
    }
    let end = end.min(len);
    text[start..end].trim().to_string()
}

fn url_to_path(url: &Uri) -> Option<PathBuf> {
    url.to_file_path().map(|cow: Cow<'_, Path>| match cow {
        Cow::Owned(path) => path,
        Cow::Borrowed(path) => path.to_path_buf(),
    })
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
