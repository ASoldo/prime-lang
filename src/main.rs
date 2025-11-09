mod compiler;
mod diagnostics;
mod formatter;
mod interpreter;
mod lint;
mod lsp;
mod parser;

use clap::{Parser, Subcommand};
use compiler::Compiler;
use diagnostics::emit_parse_errors;
use formatter::format_program;
use interpreter::interpret;
use lint::run_lint;
use parser::{LexToken, Program, parse, tokenize};
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

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
    /// Lint a file once or continuously
    Lint {
        #[arg(short, long)]
        file: PathBuf,
        #[arg(long, default_value_t = false)]
        watch: bool,
    },
    /// Format a source file and emit or write the result
    Fmt {
        #[arg(short, long)]
        file: PathBuf,
        /// Overwrite the source file with the formatted output
        #[arg(long, default_value_t = false)]
        write: bool,
    },
    /// Start the LSP server over stdio (used by editors)
    Lsp,
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
        Commands::Lint { file, watch } => {
            ensure_prime_file(&file);
            run_lint(&file, watch).expect("Failed to run lint");
        }
        Commands::Fmt { file, write } => {
            ensure_prime_file(&file);
            format_file(&file, write);
        }
        Commands::Lsp => {
            lsp::serve_stdio().expect("Failed to start LSP server");
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
            emit_parse_errors(path, source, &errors);
            std::process::exit(1);
        }
    }
}

fn format_file(path: &Path, write: bool) {
    let (source, tokens) = read_source(path);
    let program = parse_or_report(path, &source, &tokens);
    let formatted = format_program(&program);
    if write {
        if let Err(err) = fs::write(path, formatted) {
            eprintln!("Failed to write {}: {}", path.display(), err);
            std::process::exit(1);
        }
    } else {
        print!("{}", formatted);
    }
}
