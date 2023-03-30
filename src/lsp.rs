// lsp.rs
use crate::parser::{tokenize, Token};
use std::error::Error;
use std::fs;

pub fn start_lsp_server(file_path: &str) -> Result<(), Box<dyn Error + Sync + Send>> {
    let content = fs::read_to_string(file_path).expect("Could not read the file");
    let tokens = tokenize(&content);
    println!("{:?}", tokens);

    for (index, token) in tokens.iter().enumerate() {
        match token {
            Token::Unknown => {
                println!("Element 'Unknown' at position {}", index);
            }
            _ => (),
        }
    }

    Ok(())
}
