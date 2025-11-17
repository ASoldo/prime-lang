use crate::language::{ast::Module, errors::SyntaxErrors, parser::parse_module};
use tower_lsp_server::lsp_types::Uri;

use super::text::url_to_path;

pub fn parse_module_from_uri(uri: &Uri, text: &str) -> Result<Module, SyntaxErrors> {
    let path = url_to_path(uri).ok_or_else(|| SyntaxErrors::new(vec![]))?;
    let module_name = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main")
        .to_string();
    parse_module(&module_name, path, text)
}
