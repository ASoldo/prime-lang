use super::backend::Backend;
use std::error::Error;
use tokio::runtime::Runtime;
use tower_lsp_server::{LspService, Server};

pub fn serve_stdio() -> Result<(), Box<dyn Error + Send + Sync>> {
    let runtime = Runtime::new()?;
    runtime.block_on(async {
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();
        let (service, socket) = LspService::new(Backend::new);
        Server::new(stdin, stdout, socket).serve(service).await;
        Ok(())
    })
}
