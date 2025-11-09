use crate::formatter::format_program;
use crate::parser::{Expr, LexToken, Program, Statement, Token, parse, tokenize};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::sync::Arc;
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
use tower_lsp_server::{Client, LanguageServer, LspService, Server};

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

#[derive(Debug, Default)]
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

#[derive(Debug)]
struct Backend {
    client: Client,
    documents: Arc<Documents>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(Documents::default()),
        }
    }

    async fn publish_diagnostics(&self, uri: &Uri) {
        let Some(text) = self.documents.get(uri).await else {
            return;
        };
        let diagnostics = collect_diagnostics(&text);
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }

    async fn format_document(&self, uri: &Uri) -> Option<(String, String)> {
        let text = self.documents.get(uri).await?;
        let tokens = tokenize(&text);
        let program = parse(&tokens).ok()?;
        let formatted = format_program(&program);
        Some((text, formatted))
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
        rename_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        let _ = self
            .client
            .log_message(MessageType::INFO, "prime-lang LSP initialized")
            .await;
    }

    async fn shutdown(&self) -> RpcResult<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        self.documents
            .insert(uri.clone(), params.text_document.text)
            .await;
        self.publish_diagnostics(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.last() {
            let uri = params.text_document.uri.clone();
            self.documents
                .insert(uri.clone(), change.text.clone())
                .await;
            self.publish_diagnostics(&uri).await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if let Some(text) = params.text {
            let uri = params.text_document.uri.clone();
            self.documents.insert(uri.clone(), text).await;
            self.publish_diagnostics(&uri).await;
            return;
        }
        self.publish_diagnostics(&params.text_document.uri).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents.remove(&params.text_document.uri).await;
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn hover(&self, params: HoverParams) -> RpcResult<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some(text) = self.documents.get(&uri).await else {
            return Ok(None);
        };

        let tokens = tokenize(&text);
        let vars = collect_var_infos(&text, &tokens);
        let offset = position_to_offset(&text, position);

        if let Some(token) = token_at(&tokens, offset) {
            if let Some(hover) = hover_for_token(&text, token, offset, &vars) {
                return Ok(Some(hover));
            }
        }

        Ok(None)
    }

    async fn formatting(
        &self,
        params: DocumentFormattingParams,
    ) -> RpcResult<Option<Vec<TextEdit>>> {
        if let Some((original, formatted)) = self.format_document(&params.text_document.uri).await {
            let range = full_range(&original);
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
        let Some(text) = self.documents.get(&uri).await else {
            return Ok(None);
        };
        let tokens = tokenize(&text);
        let symbols = build_symbol_information(&uri, &text, &tokens);
        Ok(Some(DocumentSymbolResponse::Flat(symbols)))
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> RpcResult<Option<PrepareRenameResponse>> {
        let uri = params.text_document.uri;
        let position = params.position;
        let Some(text) = self.documents.get(&uri).await else {
            return Ok(None);
        };

        let tokens = tokenize(&text);
        let offset = position_to_offset(&text, position);
        if let Some((name, span)) = identifier_at(&tokens, offset) {
            let range = span_range(&text, span.start, span_len(&span));
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
        let Some(text) = self.documents.get(&uri).await else {
            return Ok(None);
        };

        if new_name.trim().is_empty() {
            return Ok(None);
        }

        let tokens = tokenize(&text);
        let program = parse(&tokens).ok();

        let offset = position_to_offset(&text, position);
        let Some((target_name, _span)) = identifier_at(&tokens, offset) else {
            return Ok(None);
        };

        if !is_valid_identifier(&new_name) {
            return Ok(None);
        }

        let occurrences = collect_identifier_spans(program.as_ref(), &tokens, &target_name);
        if occurrences.is_empty() {
            return Ok(None);
        }

        let edits: Vec<_> = occurrences
            .into_iter()
            .map(|span| TextEdit {
                range: span_range(&text, span.start, span_len(&span)),
                new_text: new_name.clone(),
            })
            .collect();

        Ok(Some(WorkspaceEdit {
            changes: Some([(uri, edits)].into_iter().collect()),
            ..Default::default()
        }))
    }
}

fn collect_diagnostics(text: &str) -> Vec<Diagnostic> {
    let tokens = tokenize(text);
    let mut diagnostics = Vec::new();

    diagnostics.extend(tokens.iter().filter_map(|token| {
        if matches!(token.token, Token::Unknown) {
            let start = token.span.start;
            let len = token.span.end.saturating_sub(token.span.start);
            Some(Diagnostic {
                range: span_range(text, start, len),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("prime-lang".into()),
                message: "Unknown token".into(),
                ..Default::default()
            })
        } else {
            None
        }
    }));

    let program = match parse(&tokens) {
        Ok(program) => Some(program),
        Err(errors) => {
            for err in errors {
                let span = err.span;
                let range = span_range(text, span.offset(), span.len());
                let mut message = err.message;
                if let Some(help) = err.help {
                    message.push_str("\n");
                    message.push_str(&help);
                }
                diagnostics.push(Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::ERROR),
                    source: Some("prime-lang".into()),
                    message,
                    ..Default::default()
                });
            }
            None
        }
    };

    if let Some(program) = program {
        diagnostics.extend(unused_variable_diagnostics(text, &tokens, &program));
    }

    diagnostics
}

fn span_range(text: &str, start: usize, len: usize) -> Range {
    let clamped_start = start.min(text.len());
    let clamped_end = (start.saturating_add(len)).min(text.len());
    Range {
        start: offset_to_position(text, clamped_start),
        end: offset_to_position(text, clamped_end),
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

fn full_range(text: &str) -> Range {
    Range {
        start: Position::new(0, 0),
        end: offset_to_position(text, text.len()),
    }
}

fn build_symbol_information(uri: &Uri, text: &str, tokens: &[LexToken]) -> Vec<SymbolInformation> {
    let mut symbols = Vec::new();

    if !tokens.is_empty() {
        #[allow(deprecated)]
        {
            symbols.push(SymbolInformation {
                name: "fn main".into(),
                kind: SymbolKind::FUNCTION,
                location: Location::new(uri.clone(), full_range(text)),
                container_name: None,
                deprecated: None,
                tags: None,
            });
        }
    }

    let mut i = 0;
    while i < tokens.len() {
        if let Token::LetInt = tokens[i].token {
            if let Some(Token::Identifier(name)) = tokens.get(i + 1).map(|t| &t.token) {
                let start = tokens[i].span.start;
                let end = find_statement_end(tokens, i);
                #[allow(deprecated)]
                symbols.push(SymbolInformation {
                    name: format!("let {}", name),
                    kind: SymbolKind::VARIABLE,
                    location: Location::new(uri.clone(), span_range(text, start, end - start)),
                    container_name: Some("fn main".into()),
                    deprecated: None,
                    tags: None,
                });
            }
        }
        i += 1;
    }

    symbols
}

fn find_statement_end(tokens: &[LexToken], start_idx: usize) -> usize {
    let mut end = tokens[start_idx].span.end;
    for token in tokens.iter().skip(start_idx + 1) {
        end = token.span.end;
        if matches!(token.token, Token::SemiColon | Token::RightCurlyBrace) {
            break;
        }
    }
    end
}

#[derive(Debug, Clone)]
struct DeclInfo {
    name: String,
    span: std::ops::Range<usize>,
}

fn unused_variable_diagnostics(
    text: &str,
    tokens: &[LexToken],
    program: &Program,
) -> Vec<Diagnostic> {
    let decls = collect_decl_spans(tokens);
    let mut used = HashSet::new();

    for statement in &program.statements {
        match statement {
            Statement::Let { value, .. } => collect_expr_idents(value, &mut used),
            Statement::Output(expr) => collect_expr_idents(expr, &mut used),
        }
    }

    decls
        .into_iter()
        .filter(|decl| !used.contains(&decl.name))
        .map(|decl| Diagnostic {
            range: span_range(
                text,
                decl.span.start,
                decl.span.end.saturating_sub(decl.span.start),
            ),
            severity: Some(DiagnosticSeverity::WARNING),
            source: Some("prime-lang".into()),
            message: format!("Variable `{}` is never used", decl.name),
            ..Default::default()
        })
        .collect()
}

fn collect_decl_spans(tokens: &[LexToken]) -> Vec<DeclInfo> {
    let mut decls = Vec::new();
    for i in 0..tokens.len() {
        if matches!(tokens[i].token, Token::LetInt) {
            if let Some(LexToken {
                token: Token::Identifier(name),
                span,
            }) = tokens.get(i + 1)
            {
                decls.push(DeclInfo {
                    name: name.clone(),
                    span: span.clone(),
                });
            }
        }
    }
    decls
}

fn collect_expr_idents(expr: &Expr, used: &mut HashSet<String>) {
    match expr {
        Expr::Identifier(name) => {
            used.insert(name.clone());
        }
        Expr::Binary { left, right, .. } => {
            collect_expr_idents(left, used);
            collect_expr_idents(right, used);
        }
        Expr::Integer(_) => {}
    }
}

fn collect_identifier_spans(
    program: Option<&Program>,
    tokens: &[LexToken],
    target: &str,
) -> Vec<std::ops::Range<usize>> {
    let mut spans: Vec<_> = tokens
        .iter()
        .filter_map(|token| match &token.token {
            Token::Identifier(name) if name == target => Some(token.span.clone()),
            _ => None,
        })
        .collect();

    if let Some(program) = program {
        for statement in &program.statements {
            match statement {
                Statement::Output(expr) => collect_expr_spans(expr, target, &mut spans),
                Statement::Let { value, .. } => collect_expr_spans(value, target, &mut spans),
            }
        }
    }

    spans.sort_unstable_by_key(|span| span.start);
    spans.dedup_by(|a, b| a.start == b.start && a.end == b.end);
    spans
}

fn collect_expr_spans(expr: &Expr, target: &str, spans: &mut Vec<std::ops::Range<usize>>) {
    match expr {
        Expr::Identifier(name) if name == target => {
            // spans already collected via tokens; nothing extra needed
        }
        Expr::Binary { left, right, .. } => {
            collect_expr_spans(left, target, spans);
            collect_expr_spans(right, target, spans);
        }
        Expr::Integer(_) | Expr::Identifier(_) => {}
    }
}

fn is_valid_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some(ch) if ch.is_ascii_alphabetic() || ch == '_' => (),
        _ => return false,
    }
    chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
}

#[derive(Debug, Clone)]
struct VarInfo {
    name: String,
    expr_text: String,
}

fn collect_var_infos(text: &str, tokens: &[LexToken]) -> Vec<VarInfo> {
    let mut vars = Vec::new();
    let mut idx = 0;
    while idx < tokens.len() {
        if matches!(tokens[idx].token, Token::LetInt) {
            if let Some(Token::Identifier(name)) = tokens.get(idx + 1).map(|t| &t.token) {
                if matches!(tokens.get(idx + 2).map(|t| &t.token), Some(Token::Equals)) {
                    let mut semi = idx + 3;
                    while semi < tokens.len() && !matches!(tokens[semi].token, Token::SemiColon) {
                        semi += 1;
                    }
                    if semi < tokens.len() && semi > idx + 3 {
                        let expr_start = tokens[idx + 3].span.start;
                        let expr_end = tokens[semi - 1].span.end;
                        let expr_text = text
                            .get(expr_start..expr_end)
                            .unwrap_or("")
                            .trim()
                            .to_string();
                        vars.push(VarInfo {
                            name: name.clone(),
                            expr_text,
                        });
                    }
                }
            }
        }
        idx += 1;
    }
    vars
}

fn token_at<'a>(tokens: &'a [LexToken], offset: usize) -> Option<&'a LexToken> {
    tokens
        .iter()
        .find(|token| offset >= token.span.start && offset < token.span.end)
}

fn identifier_at(
    tokens: &[LexToken],
    offset: usize,
) -> Option<(String, std::ops::Range<usize>)> {
    token_at(tokens, offset).and_then(|tok| match &tok.token {
        Token::Identifier(name) => Some((name.clone(), tok.span.clone())),
        _ => None,
    })
}

fn span_len(span: &std::ops::Range<usize>) -> usize {
    span.end.saturating_sub(span.start)
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

fn hover_for_token(
    text: &str,
    token: &LexToken,
    cursor_offset: usize,
    vars: &[VarInfo],
) -> Option<Hover> {
    match &token.token {
        Token::Identifier(name) => {
            if name == "out" {
                Some(markdown_hover(
                    text,
                    token,
                    "Built-in output function **out(expr)**\n\nEvaluates the expression and prints the result."
                        .into(),
                ))
            } else {
                let info = vars.iter().rev().find(|var| var.name == *name)?;
                let contents = format!(
                    "```prime\nlet int {} = {};\n```\nType: `int`",
                    info.name, info.expr_text
                );
                Some(markdown_hover(text, token, contents))
            }
        }
        Token::LetInt => {
            let word = word_under_cursor(text, cursor_offset);
            let message = match word.as_deref() {
                Some("let") => {
                    "Built-in keyword **let**\n\nStarts an integer binding: `let int name = value;`"
                        .into()
                }
                Some("int") => {
                    "Built-in type **int**\n\nSigned 32-bit integer used in all bindings.".into()
                }
                _ => "Let statement `let int <name> = <expr>;`.".into(),
            };
            Some(markdown_hover(text, token, message))
        }
        Token::FnMain => {
            let word = word_under_cursor(text, cursor_offset);
            let message = match word.as_deref() {
                Some("fn") => {
                    "Built-in keyword **fn**\n\nIntroduces the entry function `fn main() { ... }`."
                        .into()
                }
                Some("main") => "Built-in function **main**\n\nProgram entry point.".into(),
                _ => "Function definition `fn main() { ... }`.".into(),
            };
            Some(markdown_hover(text, token, message))
        }
        Token::Integer(value) => Some(markdown_hover(
            text,
            token,
            format!("Integer literal `{}`", value),
        )),
        _ => None,
    }
}

fn markdown_hover(text: &str, token: &LexToken, value: String) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: Some(span_range(
            text,
            token.span.start,
            token.span.end.saturating_sub(token.span.start),
        )),
    }
}

fn word_under_cursor<'a>(text: &'a str, offset: usize) -> Option<&'a str> {
    if offset >= text.len() {
        return None;
    }
    let bytes = text.as_bytes();
    if !bytes[offset].is_ascii_alphabetic() {
        return None;
    }
    let mut start = offset;
    while start > 0 && text.as_bytes()[start - 1].is_ascii_alphabetic() {
        start -= 1;
    }
    let mut end = offset;
    while end < text.len() && text.as_bytes()[end].is_ascii_alphabetic() {
        end += 1;
    }
    Some(&text[start..end])
}
