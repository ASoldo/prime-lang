// lsp.rs
use crate::parser::{tokenize, Token};
use miette::{Diagnostic, SourceSpan};
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
    src: NamedSource,
    // Snippets and highlights can be included in the diagnostic!
    #[label("& token is not yet implemented")]
    bad_bit: SourceSpan,
}

use miette::{NamedSource, Result};
fn this_fails() -> Result<()> {
    // You can use plain strings as a `Source`, or anything that implements
    // the one-method `Source` trait.
    let src = "&".to_string();

    Err(MyBad {
        src: NamedSource::new("main.prime", src),
        bad_bit: (0, 1).into(),
    })?;

    Ok(())
}

pub fn start_lsp_server(file_path: &str) -> Result<(), Box<dyn Error + Sync + Send>> {
    let content = fs::read_to_string(file_path).expect("Could not read the file");
    let tokens = tokenize(&content);
    println!("{:?}", tokens);

    println!("{}", "LSP Started!");
    for (index, token) in tokens.iter().enumerate() {
        match token {
            Token::Unknown => {
                println!("Element 'Unknown' at position {}", index);
                this_fails()?;
            }
            _ => (),
        }
    }
    println!("{}", "LSP Done! No errors!");

    Ok(())
}
