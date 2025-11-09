mod compiler;
mod interpreter;
mod lsp;
mod parser;

use clap::{Parser, Subcommand};
use compiler::Compiler;
use interpreter::interpret;
use miette::{Diagnostic, NamedSource, Report};
use parser::{LexToken, ParseError, Program, parse, tokenize};
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use thiserror::Error;

#[derive(Debug, Parser)]
#[command(
    name = "prime-lang",
    version,
    about = "Prime language CLI",
    arg_required_else_help = true
)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    /// Interpret a Prime source file directly
    Run {
        /// Path to the .prime file
        file: PathBuf,
    },
    /// Build a Prime source file down to a native executable
    Build {
        /// Path to the .prime file
        file: PathBuf,
    },
    /// Start the diagnostic helper for editor integrations
    Lsp {
        /// Optional file to lint immediately before serving
        #[arg(short, long)]
        file: Option<PathBuf>,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run { file } => {
            ensure_prime_file(&file);
            let (source, tokens) = read_source(&file);
            log_tokens(&tokens);
            let program = parse_or_report(&file, &source, &tokens);
            interpret(&program);
        }
        Commands::Build { file } => {
            ensure_prime_file(&file);
            let (source, tokens) = read_source(&file);
            log_tokens(&tokens);
            let program = parse_or_report(&file, &source, &tokens);
            build_program(&program);
        }
        Commands::Lsp { file } => {
            if let Some(ref path) = file {
                ensure_prime_file(path);
            }
            lsp::start_lsp_server(file.as_deref()).expect("Failed to start the diagnostic helper");
        }
    }
}

fn ensure_prime_file(path: &Path) {
    if path.extension().and_then(OsStr::to_str) != Some("prime") {
        eprintln!("{} is not a .prime file", path.display());
        std::process::exit(1);
    }
}

fn read_source(path: &Path) -> (String, Vec<LexToken>) {
    let content = fs::read_to_string(path).unwrap_or_else(|err| {
        eprintln!("Failed to read {}: {}", path.display(), err);
        std::process::exit(1);
    });
    let tokens = tokenize(&content);
    (content, tokens)
}

fn log_tokens(tokens: &[LexToken]) {
    let display: Vec<_> = tokens.iter().map(|lex| &lex.token).collect();
    println!("Tokens: {:?}", display);
}

fn build_program(program: &Program) {
    let mut compiler = Compiler::new();
    compiler.compile(program);
    compiler.print_ir();
    compiler
        .write_ir_to_file("output.ll")
        .expect("Failed to write LLVM IR to file");

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

    let output = Command::new("gcc")
        .arg("output.o")
        .arg("-o")
        .arg("output")
        .output()
        .expect("Failed to execute gcc");
    println!("gcc stdout: {}", String::from_utf8_lossy(&output.stdout));
    println!("gcc stderr: {}", String::from_utf8_lossy(&output.stderr));
}

fn parse_or_report(path: &Path, source: &str, tokens: &[LexToken]) -> Program {
    match parse(tokens) {
        Ok(program) => program,
        Err(errors) => {
            let named = NamedSource::new(path.display().to_string(), source.to_string());
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
