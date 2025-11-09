use crate::formatter::format_program;
use crate::parser::{Token, parse, tokenize};
use std::collections::HashMap;
use std::error::Error;
use std::sync::Arc;
use tokio::runtime::Runtime;
use tokio::sync::RwLock;
use tower_lsp_server::jsonrpc::Result as RpcResult;
use tower_lsp_server::lsp_types::{
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams, Hover,
    HoverContents, HoverParams, HoverProviderCapability, InitializeParams, InitializeResult,
    InitializedParams, MarkedString, MessageType, OneOf, Position, Range, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Uri,
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

    async fn hover(&self, _: HoverParams) -> RpcResult<Option<Hover>> {
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "Prime language file".to_string(),
            )),
            range: None,
        }))
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

    if let Err(errors) = parse(&tokens) {
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
