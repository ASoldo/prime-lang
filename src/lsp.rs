// lsp.rs
use crate::parser::{Token, tokenize};
use miette::{Diagnostic, NamedSource, Result, SourceSpan};
use std::error::Error;
use std::fs;
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
#[error("Unknown Token spoted:")]
#[diagnostic(
    code(main::prime::error),
    url(docsrs),
    help("Contact developer to implement it")
)]
struct MyBad {
    // The Source that we're gonna be printing snippets out of.
    // This can be a String if you don't have or care about file names.
    #[source_code]
    src: NamedSource<String>,
    // Snippets and highlights can be included in the diagnostic!
    #[label("& token is not yet implemented")]
    bad_bit: SourceSpan,
}

fn report_unknown(file_path: &str, content: &str, span: SourceSpan) -> Result<()> {
    Err(MyBad {
        src: NamedSource::new(file_path.to_string(), content.to_string()),
        bad_bit: span,
    }
    .into())
}

pub fn start_lsp_server(file_path: &str) -> Result<(), Box<dyn Error + Sync + Send>> {
    let content = fs::read_to_string(file_path).expect("Could not read the file");
    let tokens = tokenize(&content);
    println!("{:?}", tokens);

    println!("{}", "LSP Started!");
    for (index, token) in tokens.iter().enumerate() {
        if matches!(token.token, Token::Unknown) {
            println!("Element 'Unknown' at position {}", index);
            let span: SourceSpan = (
                token.span.start,
                token.span.end.saturating_sub(token.span.start),
            )
                .into();
            report_unknown(file_path, &content, span)?;
        }
    }
    println!("{}", "LSP Done! No errors!");

    Ok(())
}
