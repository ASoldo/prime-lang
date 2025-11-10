use crate::{
    formatter::format_module,
    language::{ast::Item, lexer::lex, parser::parse_module},
};
use std::{borrow::Cow, collections::HashMap, error::Error, path::PathBuf, sync::Arc};
use tokio::runtime::Runtime;
use tokio::sync::RwLock;
use tower_lsp_server::jsonrpc::Result as RpcResult;
use tower_lsp_server::lsp_types::{
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams,
    DocumentSymbolParams, DocumentSymbolResponse, Hover, HoverParams, InitializeParams,
    InitializeResult, InitializedParams, MessageType, OneOf, Position, Range, ServerCapabilities,
    SymbolInformation, SymbolKind, TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Uri,
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
}

impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> RpcResult<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                document_formatting_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
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

    async fn formatting(
        &self,
        params: DocumentFormattingParams,
    ) -> RpcResult<Option<Vec<TextEdit>>> {
        let Some(text) = self.docs.get(&params.text_document.uri).await else {
            return Ok(None);
        };
        if let Some(formatted) = format_document(&params.text_document.uri, &text) {
            let range = Range {
                start: Position::new(0, 0),
                end: offset_to_position(&text, text.len()),
            };
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
        let Some(text) = self.docs.get(&params.text_document.uri).await else {
            return Ok(None);
        };
        let symbols = collect_symbols(&params.text_document.uri, &text);
        Ok(Some(DocumentSymbolResponse::Flat(symbols)))
    }

    async fn hover(&self, _: HoverParams) -> RpcResult<Option<Hover>> {
        Ok(None)
    }
}

fn format_document(uri: &Uri, text: &str) -> Option<String> {
    let path = url_to_path(uri)?;
    let module_name = path.file_stem()?.to_str()?.to_string();
    let module = parse_module(&module_name, path, text).ok()?;
    Some(format_module(&module))
}

fn collect_diagnostics(uri: &Uri, text: &str) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    match lex(text) {
        Ok(_) => {
            if let Some(path) = url_to_path(uri) {
                let name = path
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or("main")
                    .to_string();
                if let Err(errors) = parse_module(&name, path, text) {
                    for err in errors.errors {
                        diags.push(syntax_error_to_lsp(text, err));
                    }
                }
            }
        }
        Err(errors) => {
            for err in errors {
                diags.push(Diagnostic {
                    range: byte_range_to_lsp(text, err.span.start, err.span.len()),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: err.message,
                    ..Default::default()
                });
            }
        }
    }
    diags
}

fn syntax_error_to_lsp(text: &str, err: crate::language::errors::SyntaxError) -> Diagnostic {
    let mut message = err.message;
    if let Some(help) = err.help {
        message.push('\n');
        message.push_str(&help);
    }
    Diagnostic {
        range: byte_range_to_lsp(text, err.span.start, err.span.len()),
        severity: Some(DiagnosticSeverity::ERROR),
        message,
        ..Default::default()
    }
}

fn collect_symbols(uri: &Uri, text: &str) -> Vec<SymbolInformation> {
    let mut symbols = Vec::new();
    let Some(path) = url_to_path(uri) else {
        return symbols;
    };
    let name = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main")
        .to_string();
    let module = match parse_module(&name, path, text) {
        Ok(module) => module,
        Err(_) => return symbols,
    };

    for item in module.items {
        match item {
            Item::Function(func) => symbols.push(make_symbol(
                uri,
                func.name,
                SymbolKind::FUNCTION,
                func.span.start,
                func.span.len(),
                text,
            )),
            Item::Struct(def) => symbols.push(make_symbol(
                uri,
                def.name,
                SymbolKind::STRUCT,
                def.span.start,
                def.span.len(),
                text,
            )),
            Item::Enum(def) => symbols.push(make_symbol(
                uri,
                def.name,
                SymbolKind::ENUM,
                def.span.start,
                def.span.len(),
                text,
            )),
            Item::Const(def) => symbols.push(make_symbol(
                uri,
                def.name,
                SymbolKind::CONSTANT,
                def.span.start,
                def.span.len(),
                text,
            )),
        }
    }

    symbols
}

fn make_symbol(
    uri: &Uri,
    name: String,
    kind: SymbolKind,
    start: usize,
    len: usize,
    text: &str,
) -> SymbolInformation {
    #[allow(deprecated)]
    {
        SymbolInformation {
            name,
            kind,
            location: tower_lsp_server::lsp_types::Location::new(
                uri.clone(),
                byte_range_to_lsp(text, start, len),
            ),
            container_name: None,
            deprecated: None,
            tags: None,
        }
    }
}

fn url_to_path(url: &Uri) -> Option<PathBuf> {
    url.to_file_path().map(|cow| match cow {
        Cow::Owned(path) => path,
        Cow::Borrowed(path) => path.to_path_buf(),
    })
}

fn byte_range_to_lsp(text: &str, start: usize, len: usize) -> Range {
    Range {
        start: offset_to_position(text, start),
        end: offset_to_position(text, start + len),
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
