mod compiler;
mod diagnostics;
mod formatter;
mod language;
mod lint;
mod lsp;
mod project;
mod runtime;

use clap::{Parser, Subcommand};
use compiler::Compiler;
use diagnostics::{emit_syntax_errors, report_io_error, report_runtime_error};
use formatter::format_module;
use language::parser::parse_module;
use lint::run_lint;
use project::{FileErrors, PackageError, load_package};
use runtime::Interpreter;
use std::{
    fs,
    path::{Path, PathBuf},
    process::Command,
};

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
    Run { file: PathBuf },
    /// Build a Prime source file down to LLVM IR and native binary
    Build {
        file: PathBuf,
        /// Base name for generated artifacts
        #[arg(long, default_value = "output")]
        name: String,
    },
    /// Lint a file once or continuously
    Lint {
        file: PathBuf,
        #[arg(long, default_value_t = false)]
        watch: bool,
    },
    /// Format a Prime source file
    Fmt {
        file: PathBuf,
        #[arg(long, default_value_t = false)]
        write: bool,
    },
    /// Start the LSP server over stdio
    Lsp,
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Commands::Run { file } => run_entry(&file),
        Commands::Build { file, name } => build_entry(&file, &name),
        Commands::Lint { file, watch } => {
            ensure_prime_file(&file);
            if let Err(err) = run_lint(&file, watch) {
                eprintln!("lint failed: {err}");
                std::process::exit(1);
            }
        }
        Commands::Fmt { file, write } => {
            ensure_prime_file(&file);
            if let Err(err) = format_file(&file, write) {
                eprintln!("format failed: {err}");
                std::process::exit(1);
            }
        }
        Commands::Lsp => {
            if let Err(err) = lsp::serve_stdio() {
                eprintln!("LSP server failed: {err}");
                std::process::exit(1);
            }
        }
    }
}
fn run_entry(path: &Path) {
    ensure_prime_file(path);
    match load_package(path) {
        Ok(package) => {
            let mut interpreter = Interpreter::new(package.clone());
            if let Err(err) = interpreter.run() {
                report_runtime_error(&err);
                std::process::exit(1);
            }
        }
        Err(PackageError::Io { path, error }) => {
            report_io_error(&path, &error);
            std::process::exit(1);
        }
        Err(PackageError::Syntax(errors)) => {
            emit_syntax_errors(&errors);
            std::process::exit(1);
        }
    }
}

fn build_entry(path: &Path, name: &str) {
    ensure_prime_file(path);
    match load_package(path) {
        Ok(package) => {
            let mut compiler = Compiler::new();
            if let Err(err) = compiler.compile_program(&package.program) {
                eprintln!("Build failed: {err}");
                std::process::exit(1);
            }
            let build_root = Path::new(".build.prime");
            if let Err(err) = fs::create_dir_all(build_root) {
                eprintln!("Failed to create build directory: {err}");
                std::process::exit(1);
            }
            let artifact_dir = build_root.join(name);
            if let Err(err) = fs::create_dir_all(&artifact_dir) {
                eprintln!("Failed to create artifact directory: {err}");
                std::process::exit(1);
            }
            let ir_path = artifact_dir.join(format!("{name}.ll"));
            if let Err(err) = compiler.write_ir_to(&ir_path) {
                eprintln!("Failed to write IR: {err}");
                std::process::exit(1);
            }
            let obj_path = artifact_dir.join(format!("{name}.o"));
            if let Err(err) = run_llc(&ir_path, &obj_path) {
                eprintln!("{err}");
                std::process::exit(1);
            }
            let bin_path = artifact_dir.join(name);
            if let Err(err) = run_gcc(&obj_path, &bin_path) {
                eprintln!("{err}");
                std::process::exit(1);
            }
            println!("Artifacts written to {}", artifact_dir.display());
        }
        Err(PackageError::Io { path, error }) => {
            report_io_error(&path, &error);
            std::process::exit(1);
        }
        Err(PackageError::Syntax(errors)) => {
            emit_syntax_errors(&errors);
            std::process::exit(1);
        }
    }
}

fn format_file(path: &Path, write: bool) -> Result<(), Box<dyn std::error::Error>> {
    let source = fs::read_to_string(path)?;
    let module_name = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main")
        .to_string();
    let module = match parse_module(&module_name, path.to_path_buf(), &source) {
        Ok(module) => module,
        Err(errs) => {
            emit_syntax_errors(&[FileErrors {
                path: path.to_path_buf(),
                source: source.clone(),
                errors: errs.errors,
            }]);
            return Err("failed to parse module".into());
        }
    };
    let formatted = format_module(&module);
    if write {
        fs::write(path, formatted)?;
    } else {
        print!("{formatted}");
    }
    Ok(())
}

fn run_llc(ir_path: &Path, obj_path: &Path) -> Result<(), String> {
    let output = Command::new("llc")
        .arg("-relocation-model=pic")
        .arg("-filetype=obj")
        .arg(ir_path)
        .arg("-o")
        .arg(obj_path)
        .output()
        .map_err(|err| format!("Failed to execute llc: {err}"))?;
    if !output.status.success() {
        return Err(format!(
            "llc failed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    Ok(())
}

fn run_gcc(obj_path: &Path, bin_path: &Path) -> Result<(), String> {
    let output = Command::new("gcc")
        .arg(obj_path)
        .arg("-o")
        .arg(bin_path)
        .output()
        .map_err(|err| format!("Failed to execute gcc: {err}"))?;
    if !output.status.success() {
        return Err(format!(
            "gcc failed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    Ok(())
}

fn ensure_prime_file(path: &Path) {
    if path
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext != "prime")
        .unwrap_or(true)
    {
        eprintln!("{} is not a .prime file", path.display());
        std::process::exit(1);
    }
}
