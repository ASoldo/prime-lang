mod compiler;
mod interpreter;
mod lsp;
mod parser;

use compiler::Compiler;
use interpreter::interpret;
use miette::{Diagnostic, NamedSource, Report};
use parser::{LexToken, ParseError, Program, parse, tokenize};
use std::env;
use std::fs;
use std::process::Command;
use thiserror::Error;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 3 {
        eprintln!("Usage: ./prime-lang [run|build|lsp] <filename.prime>");
        std::process::exit(1);
    }

    let command = &args[1];
    let filename = &args[2];

    if !filename.ends_with(".prime") {
        eprintln!("Invalid file extension. Only .prime files are allowed.");
        std::process::exit(1);
    }

    let content = fs::read_to_string(filename).expect("Could not read the file");
    let tokens = tokenize(&content);

    match command.as_str() {
        "run" => {
            println!(
                "Tokens: {:?}",
                tokens.iter().map(|lex| &lex.token).collect::<Vec<_>>()
            );
            let program = parse_or_report(filename, &content, &tokens);
            interpret(&program);
        }
        "build" => {
            println!(
                "Tokens: {:?}",
                tokens.iter().map(|lex| &lex.token).collect::<Vec<_>>()
            );
            let program = parse_or_report(filename, &content, &tokens);

            // Initialize the compiler instance backed by llvm-sys
            let mut compiler = Compiler::new();

            // Compile the code
            compiler.compile(&program);
            compiler.print_ir();
            compiler
                .write_ir_to_file("output.ll")
                .expect("Failed to write LLVM IR to file");

            // Call llc
            let output = Command::new("llc")
                .arg("-relocation-model=pic")
                .arg("-filetype=obj")
                .arg("output.ll")
                .arg("-o")
                .arg("output.o")
                .output()
                .expect("Failed to execute llc");
            println!("llc stdout: {}", String::from_utf8_lossy(&output.stdout));
            println!("llc stderr: {}", String::from_utf8_lossy(&output.stderr));

            // Call gcc
            let output = Command::new("gcc")
                .arg("output.o")
                .arg("-o")
                .arg("output")
                .output()
                .expect("Failed to execute gcc");
            println!("gcc stdout: {}", String::from_utf8_lossy(&output.stdout));
            println!("gcc stderr: {}", String::from_utf8_lossy(&output.stderr));
        }
        "lsp" => {
            // Start the LSP server
            lsp::start_lsp_server(filename).expect("Failed to start LSP server");
        }
        _ => {
            eprintln!("Invalid command. Usage: ./prime-lang [run|build|lsp] <filename.prime>");
            std::process::exit(1);
        }
    }
}

fn parse_or_report(filename: &str, source: &str, tokens: &[LexToken]) -> Program {
    match parse(tokens) {
        Ok(program) => program,
        Err(errors) => {
            let named = NamedSource::new(filename.to_string(), source.to_string());
            for err in errors {
                let diagnostic = ParserDiagnostic::from_error(named.clone(), err);
                let report = Report::new(diagnostic);
                eprintln!("{:?}", report);
            }
            std::process::exit(1);
        }
    }
}

#[derive(Debug, Error, Diagnostic)]
#[error("{message}")]
struct ParserDiagnostic {
    message: String,
    label: String,
    #[source_code]
    src: NamedSource<String>,
    #[label("{label}")]
    span: miette::SourceSpan,
    #[help("{help_msg}")]
    help_msg: Option<String>,
}

impl ParserDiagnostic {
    fn from_error(src: NamedSource<String>, err: ParseError) -> Self {
        Self {
            message: err.message,
            label: err.label,
            span: err.span,
            src,
            help_msg: err.help,
        }
    }
}
