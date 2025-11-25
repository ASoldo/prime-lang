mod docs;
mod language;
mod lsp;
mod project;
mod runtime;
mod tools;

use clap::{Parser, Subcommand, ValueEnum};
use language::{ast::{Item, ModuleKind}, span::Span};
use language::{compiler::Compiler, macro_expander, parser::parse_module, typecheck};
use miette::NamedSource;
use project::diagnostics::analyze_manifest_issues;
use project::{
    canonicalize, FileErrors, PackageError, apply_manifest_header_with_manifest, find_manifest,
    load_package, manifest::PackageManifest, warn_manifest_drift,
};
use runtime::Interpreter;
use std::{
    env, fs, io,
    path::{Component, Path, PathBuf},
    process::Command,
};
use toml::{Value, value::Table as TomlTable};
use tools::{
    diagnostics::{
        emit_manifest_issues, emit_syntax_errors, emit_type_errors, report_io_error,
        report_runtime_error,
    },
    formatter::format_module,
    lint::run_lint,
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
        #[arg(value_name = "FILE", required_unless_present = "file_flag")]
        path: Option<PathBuf>,
        #[arg(
            short = 'f',
            long = "file",
            value_name = "FILE",
            conflicts_with = "path"
        )]
        file_flag: Option<PathBuf>,
        #[arg(long, default_value_t = false)]
        write: bool,
    },
    /// Start the LSP server over stdio
    Lsp,
    /// Initialize a new Prime workspace
    Init {
        #[arg(value_name = "DIR", default_value = ".")]
        path: PathBuf,
    },
    /// Run Prime tests discovered in the workspace
    Test {
        #[arg(
            value_name = "TARGET",
            value_delimiter = ',',
            help = "Optional comma-separated list of test files or module names (defaults to discovering test headers)"
        )]
        targets: Vec<String>,
    },
    /// Add a module entry and stub file
    Add {
        #[arg(value_name = "MODULE")]
        name: String,
        #[arg(long, value_name = "PATH")]
        path: Option<PathBuf>,
        #[arg(long, value_enum, default_value = "pub")]
        visibility: ModuleVisibilityArg,
        #[arg(
            long,
            default_value_t = false,
            help = "Add a test entry instead of a module",
            conflicts_with = "library"
        )]
        test: bool,
        #[arg(
            long,
            default_value_t = false,
            help = "Add a library entry (importable, no `main`)",
            conflicts_with = "test"
        )]
        library: bool,
    },
    /// Print reference snippets for Prime language features
    Docs {
        #[arg(
            long,
            value_name = "TOPIC",
            value_delimiter = ',',
            help = "Comma-separated list of topics to show"
        )]
        query: Vec<String>,
        #[arg(long, default_value_t = false, help = "List available topics")]
        list: bool,
    },
    /// Expand macros for a file (optionally at a cursor offset)
    Expand {
        #[arg(value_name = "FILE")]
        file: PathBuf,
        #[arg(
            long,
            value_name = "BYTE_OFFSET",
            help = "Byte offset in the file for which to show macro expansion trace"
        )]
        offset: Option<usize>,
        #[arg(
            long,
            value_name = "LINE",
            requires = "column",
            help = "1-based line number (use with --column) to derive byte offset"
        )]
        line: Option<usize>,
        #[arg(
            long,
            value_name = "COLUMN",
            requires = "line",
            help = "1-based column number (use with --line) to derive byte offset"
        )]
        column: Option<usize>,
        #[arg(
            long,
            default_value_t = false,
            help = "Also print the fully expanded, formatted module"
        )]
        print_expanded: bool,
    },
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Commands::Run { file } => run_entry(&file),
        Commands::Build { file, name } => build_entry(&file, &name),
        Commands::Lint { file, watch } => {
            ensure_prime_file(&file);
            warn_manifest_drift(&file);
            if let Err(err) = run_lint(&file, watch) {
                eprintln!("lint failed: {err}");
                std::process::exit(1);
            }
        }
        Commands::Fmt {
            path,
            file_flag,
            write,
        } => {
            let file = path.or(file_flag).expect("clap enforces an input path");
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
        Commands::Init { path } => {
            if let Err(err) = init_project(&path) {
                eprintln!("init failed: {err}");
                std::process::exit(1);
            }
        }
        Commands::Test { targets } => {
            if let Err(errors) = tools::tester::run_tests(&targets) {
                eprintln!("{errors}");
                std::process::exit(1);
            }
        }
        Commands::Add {
            name,
            path,
            visibility,
            test,
            library,
        } => {
            if let Err(err) = add_module(&name, path.as_deref(), visibility, test, library) {
                eprintln!("add failed: {err}");
                std::process::exit(1);
            }
        }
        Commands::Docs { query, list } => {
            if list {
                docs::print_topic_list();
            } else if let Err(err) = docs::print_topics(&query) {
                eprintln!("{err}");
                eprintln!("Run `prime-lang docs --list` to see available topics.");
                std::process::exit(1);
            }
        }
        Commands::Expand {
            file,
            offset,
            line,
            column,
            print_expanded,
        } => expand_entry(&file, offset, line, column, print_expanded),
    }
}
fn run_entry(path: &Path) {
    ensure_prime_file(path);
    if is_test_file(path) {
        eprintln!("`prime-lang run` cannot execute test targets; use `prime-lang test`");
        std::process::exit(1);
    }
    if is_library_file(path) {
        eprintln!("`prime-lang run` cannot execute library targets; use a module with `main`");
        std::process::exit(1);
    }
    reject_library_entry(path);
    warn_manifest_drift(path);
    match load_package(path) {
        Ok(package) => {
            let expanded_program = expand_or_report(&package.program);
            let expanded_modules = expanded_program
                .program
                .modules
                .iter()
                .cloned()
                .map(|module| project::ModuleUnit { module })
                .collect();
            let expanded_package = project::Package {
                program: expanded_program.program.clone(),
                modules: expanded_modules,
            };
            if let Err(errors) = typecheck::check_program(&expanded_program) {
                emit_type_errors(&errors);
                std::process::exit(1);
            }
            let mut interpreter = Interpreter::new(expanded_package);
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
        Err(PackageError::Manifest { path, message }) => {
            eprintln!("manifest error at {}: {}", path.display(), message);
            std::process::exit(1);
        }
    }
}

fn compile_runtime_abi() -> Result<PathBuf, String> {
    let runtime_dir = PathBuf::from(".build.prime/runtime");
    if let Err(err) = fs::create_dir_all(&runtime_dir) {
        return Err(format!("failed to create runtime build dir: {err}"));
    }
    let output_lib = runtime_dir.join("libruntime_abi.a");
    let status = Command::new("rustc")
        .arg("--crate-type")
        .arg("staticlib")
        .arg("--edition")
        .arg("2021")
        .arg("src/runtime/abi.rs")
        .arg("-o")
        .arg(&output_lib)
        .status()
        .map_err(|err| format!("failed to spawn rustc for runtime ABI: {err}"))?;
    if !status.success() {
        return Err("rustc failed compiling runtime ABI".into());
    }
    Ok(output_lib)
}

fn build_entry(path: &Path, name: &str) {
    ensure_prime_file(path);
    if is_test_file(path) {
        eprintln!("`prime-lang build` cannot compile test targets; use `prime-lang test`");
        std::process::exit(1);
    }
    if is_library_file(path) {
        eprintln!("`prime-lang build` cannot compile library targets; use a module with `main`");
        std::process::exit(1);
    }
    reject_library_entry(path);
    warn_manifest_drift(path);
    match load_package(path) {
        Ok(package) => {
            let expanded_program = expand_or_report(&package.program);
            if let Err(errors) = typecheck::check_program(&expanded_program) {
                emit_type_errors(&errors);
                std::process::exit(1);
            }
            let mut compiler = Compiler::new();
            if let Err(err) = compiler.compile_program(&expanded_program.program) {
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
            let runtime_lib = match compile_runtime_abi() {
                Ok(path) => Some(path),
                Err(err) => {
                    eprintln!("Failed to compile runtime ABI: {err}");
                    std::process::exit(1);
                }
            };
            let bin_path = artifact_dir.join(name);
            if let Err(err) = run_gcc_with_runtime(&obj_path, runtime_lib.as_deref(), &bin_path) {
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
        Err(PackageError::Manifest { path, message }) => {
            eprintln!("manifest error at {}: {}", path.display(), message);
            std::process::exit(1);
        }
    }
}

fn format_file(path: &Path, write: bool) -> Result<(), Box<dyn std::error::Error>> {
    warn_manifest_drift(path);
    let source = fs::read_to_string(path)?;
    let module_name = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main")
        .to_string();
    let mut module = match parse_module(&module_name, path.to_path_buf(), &source) {
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
    let manifest =
        find_manifest(path).and_then(|manifest_path| PackageManifest::load(&manifest_path).ok());
    if let Some(manifest) = manifest.as_ref() {
        let issues = analyze_manifest_issues(&module, path, Some(manifest));
        if !issues.is_empty() {
            let named = NamedSource::new(path.display().to_string(), source.clone());
            emit_manifest_issues(&named, &issues);
        }
    }
    apply_manifest_header_with_manifest(manifest.as_ref(), path, &mut module);
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

fn run_gcc_with_runtime(
    obj_path: &Path,
    runtime_lib: Option<&Path>,
    bin_path: &Path,
) -> Result<(), String> {
    let mut cmd = Command::new("gcc");
    cmd.arg(obj_path);
    if let Some(lib) = runtime_lib {
        cmd.arg(lib);
    }
    cmd.arg("-o")
        .arg(bin_path)
        .arg("-lpthread")
        .arg("-ldl")
        .arg("-lm");
    let output = cmd
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

fn expand_entry(
    path: &Path,
    offset: Option<usize>,
    line: Option<usize>,
    column: Option<usize>,
    print_expanded: bool,
) {
    ensure_prime_file(path);
    if offset.is_some() && (line.is_some() || column.is_some()) {
        eprintln!("Use either --offset or --line/--column to select a position, not both.");
        std::process::exit(1);
    }
    let source = match fs::read_to_string(path) {
        Ok(text) => text,
        Err(err) => {
            report_io_error(path, &err);
            std::process::exit(1);
        }
    };
    let resolved_offset = match offset {
        Some(off) => off,
        None => {
            let line = line.unwrap_or(1);
            let column = column.unwrap_or(1);
            match offset_from_line_col(&source, line, column) {
                Some(off) => off,
                None => {
                    eprintln!(
                        "Could not compute offset for line {}, column {} in {}",
                        line,
                        column,
                        path.display()
                    );
                    std::process::exit(1);
                }
            }
        }
    };
    warn_manifest_drift(path);
    let has_position = offset.is_some() || line.is_some() || column.is_some();
    match load_package(path) {
        Ok(package) => {
            let expanded = expand_or_report(&package.program);
            let canonical = canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
            let span = Span::new(resolved_offset, resolved_offset.saturating_add(1));
            let macro_names = expanded.traces.macro_names_for(&canonical, span);
            if let Some(trace) = expanded.traces.help_for(&canonical, span) {
                println!("Macro expansion trace:\n{trace}");
            } else {
                println!(
                    "No macro expansion trace found at offset {} (line {}, column {})",
                    resolved_offset,
                    line.unwrap_or_else(|| line_number(&source, resolved_offset)),
                    column.unwrap_or_else(|| column_number(&source, resolved_offset))
                );
            }
            if let Some(module) = expanded
                .program
                .modules
                .iter()
                .find(|m| m.path == canonical)
            {
                let formatted = if print_expanded || !has_position {
                    format_module(module)
                } else {
                    let mut focused_items: Vec<Item> = module
                        .items
                        .iter()
                        .filter(|item| {
                            let span = item_span(item);
                            span.start <= resolved_offset && span.end >= resolved_offset
                        })
                        .cloned()
                        .collect();
                    if focused_items.is_empty() {
                        if let Some(names) = macro_names.as_ref() {
                            if let Some(origins) = expanded.item_origins.get(&canonical) {
                                let by_origin: Vec<Item> = module
                                    .items
                                    .iter()
                                    .zip(origins.iter())
                                    .filter(|(_, origin)| {
                                        origin
                                            .as_ref()
                                            .map(|o| names.iter().any(|n| n == o))
                                            .unwrap_or(false)
                                    })
                                    .map(|(item, _)| item.clone())
                                    .collect();
                                focused_items.extend(by_origin);
                            }
                        }
                    }
                    if focused_items.is_empty() {
                        format_module(module)
                    } else {
                        let mut clone = module.clone();
                        clone.items = focused_items;
                        format_module(&clone)
                    }
                };
                let header = if print_expanded || !has_position {
                    format!("Expanded module ({})", module.name)
                } else {
                    format!(
                        "Expanded items at offset {} ({}:{})",
                        resolved_offset,
                        line.unwrap_or_else(|| line_number(&source, resolved_offset)),
                        column.unwrap_or_else(|| column_number(&source, resolved_offset))
                    )
                };
                println!("\n=== {header} ===\n");
                println!("{formatted}");
            } else {
                eprintln!(
                    "Expanded module for {} not found in expanded program",
                    canonical.display()
                );
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
        Err(PackageError::Manifest { path, message }) => {
            eprintln!("manifest error at {}: {}", path.display(), message);
            std::process::exit(1);
        }
    }
}

fn offset_from_line_col(source: &str, line: usize, column: usize) -> Option<usize> {
    if line == 0 || column == 0 {
        return None;
    }
    let mut offset = 0usize;
    for (idx, mut l) in source.split_inclusive('\n').enumerate() {
        let current_line = idx + 1;
        if current_line == line {
            if !l.ends_with('\n') {
                l = l.trim_end_matches('\r');
            }
            if column - 1 > l.len() {
                return None;
            }
            return Some(offset + column - 1);
        }
        offset += l.len();
    }
    None
}

fn line_number(text: &str, offset: usize) -> usize {
    text[..offset.min(text.len())].bytes().filter(|b| *b == b'\n').count() + 1
}

fn column_number(text: &str, offset: usize) -> usize {
    let slice = &text[..offset.min(text.len())];
    let line_start = slice.rfind('\n').map(|idx| idx + 1).unwrap_or(0);
    offset.saturating_sub(line_start) + 1
}

fn item_span(item: &Item) -> Span {
    match item {
        Item::Struct(def) => def.span,
        Item::Enum(def) => def.span,
        Item::Interface(def) => def.span,
        Item::Impl(def) => def
            .methods
            .first()
            .map(|m| m.span)
            .unwrap_or_else(|| Span::new(0, 0)),
        Item::Function(def) => def.span,
        Item::Macro(def) => def.span,
        Item::Const(def) => def.span,
        Item::MacroInvocation(inv) => inv.span,
    }
}

#[cfg(test)]
mod expand_tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn computes_offset_from_line_and_column() {
        let src = "abc\ndef\n";
        assert_eq!(offset_from_line_col(src, 1, 1), Some(0));
        assert_eq!(offset_from_line_col(src, 2, 2), Some(5));
        assert_eq!(offset_from_line_col(src, 3, 1), None);
    }

    #[test]
    fn expansion_trace_reports_macro_chain() {
        let dir = tempdir().expect("tempdir");
        let file = dir.path().join("demo.prime");
        let src = r#"
module demo::main;

macro add_one(value: expr) -> int32 { value + 1 }

fn main() {
  let int32 x = ~add_one(5);
  out(x);
}
"#;
        fs::write(&file, src).expect("write");
        let package = load_package(&file).expect("load package");
        let expanded = expand_or_report(&package.program);
        let canonical = canonicalize(&file).unwrap();
        let offset = src.find("~add_one").expect("find macro usage");
        let trace = expanded
            .traces
            .help_for(&canonical, Span::new(offset, offset + 1));
        assert!(
            trace
                .as_deref()
                .map(|t| t.contains("add_one"))
                .unwrap_or(false),
            "expected macro name in trace, got {trace:?}"
        );
    }
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

fn is_test_file(path: &Path) -> bool {
    fs::read_to_string(path)
        .ok()
        .map(|content| {
            let trimmed = content.trim_start();
            trimmed.starts_with("test ")
                || trimmed.starts_with("test\t")
                || trimmed.starts_with("test::")
                || trimmed.starts_with("test.")
        })
        .unwrap_or(false)
}

fn is_library_file(path: &Path) -> bool {
    fs::read_to_string(path)
        .ok()
        .map(|content| {
            let trimmed = content.trim_start();
            trimmed.starts_with("library ")
                || trimmed.starts_with("library\t")
                || trimmed.starts_with("library::")
                || trimmed.starts_with("library.")
        })
        .unwrap_or(false)
}

fn reject_library_entry(path: &Path) {
    if let Some(manifest_path) = find_manifest(path) {
        if let Ok(manifest) = PackageManifest::load(&manifest_path) {
            let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
            if let Some(name) = manifest.module_name_for_path(&canonical) {
                if let Some(kind) = manifest.module_kind(&name) {
                    if kind != ModuleKind::Module {
                        eprintln!(
                            "`{}` is listed as {:?} in prime.toml and cannot be used as an entrypoint",
                            name, kind
                        );
                        std::process::exit(1);
                    }
                }
            }
        }
    }
}

fn expand_or_report(program: &language::ast::Program) -> macro_expander::ExpandedProgram {
    match macro_expander::expand_program(program) {
        Ok(expanded) => expanded,
        Err(errors) => {
            let file_errors: Vec<FileErrors> = errors
                .into_iter()
                .map(|err| FileErrors {
                    source: fs::read_to_string(&err.path).unwrap_or_default(),
                    path: err.path,
                    errors: err.errors,
                })
                .collect();
            emit_syntax_errors(&file_errors);
            std::process::exit(1);
        }
    }
}

#[derive(Clone, Copy, Debug, ValueEnum)]
enum ModuleVisibilityArg {
    Pub,
    Package,
    Private,
}

impl ModuleVisibilityArg {
    fn as_manifest_str(&self) -> &'static str {
        match self {
            ModuleVisibilityArg::Pub => "pub",
            ModuleVisibilityArg::Package => "package",
            ModuleVisibilityArg::Private => "private",
        }
    }
}

fn init_project(dir: &Path) -> Result<(), Box<dyn std::error::Error>> {
    if !dir.exists() {
        fs::create_dir_all(dir)?;
    }
    let manifest_path = dir.join("prime.toml");
    if manifest_path.exists() {
        return Err(io::Error::new(
            io::ErrorKind::AlreadyExists,
            format!("{} already exists", manifest_path.display()),
        )
        .into());
    }
    let package_name = package_name_from_dir(dir);
    let manifest = format!(
        r#"manifest_version = "2"

[package]
name = "{package_name}"
version = "0.1.0"
kind = "binary"
entry = "app::main"

[[modules]]
name = "app::main"
path = "main.prime"
visibility = "pub"
"#
    );
    fs::write(&manifest_path, manifest)?;
    let main_path = dir.join("main.prime");
    write_module_file(&main_path, "app::main", true, false, false)?;
    Ok(())
}

fn add_module(
    module_name: &str,
    explicit_path: Option<&Path>,
    visibility: ModuleVisibilityArg,
    is_test: bool,
    is_library: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let segments = parse_module_segments(module_name)?;
    let cwd = env::current_dir()?;
    let manifest_path = find_manifest(&cwd).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::NotFound,
            "could not locate prime.toml in current or parent directories",
        )
    })?;
    let manifest_dir = manifest_path
        .parent()
        .map(Path::to_path_buf)
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "invalid manifest path"))?;
    let manifest_text = fs::read_to_string(&manifest_path)?;
    let mut doc: Value = toml::from_str(&manifest_text)?;
    let table = doc
        .as_table_mut()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "malformed manifest"))?;
    match table
        .get("manifest_version")
        .and_then(|value| value.as_str())
    {
        Some("2") => {}
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "`prime add` requires manifest_version = \"2\"",
            )
            .into());
        }
    }
    let rel_path = match explicit_path {
        Some(path) => normalize_relative_path(path)?,
        None => default_module_path(&segments),
    };
    let manifest_path_string = manifest_path_string(&rel_path);
    let key = if is_test { "tests" } else { "modules" };
    let modules_value = table.entry(key).or_insert_with(|| Value::Array(Vec::new()));
    let modules = modules_value
        .as_array_mut()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "`modules` must be an array"))?;
    let manifest_current = PackageManifest::load(&manifest_path).map_err(|err| {
        io::Error::new(
            io::ErrorKind::Other,
            format!("failed to load manifest: {err:?}"),
        )
    })?;
    if manifest_current.module_path(module_name).is_some() {
        return Err(
            io::Error::new(io::ErrorKind::Other, "module already exists in manifest").into(),
        );
    }
    let mut entry = TomlTable::new();
    entry.insert("name".into(), Value::String(module_name.to_string()));
    entry.insert("path".into(), Value::String(manifest_path_string.clone()));
    entry.insert(
        "visibility".into(),
        Value::String(visibility.as_manifest_str().to_string()),
    );
    if is_library {
        entry.insert("kind".into(), Value::String("library".into()));
    }
    modules.push(Value::Table(entry));
    let manifest_pretty = render_manifest(&doc)?;
    fs::write(&manifest_path, manifest_pretty)?;
    let module_abs_path = manifest_dir.join(&rel_path);
    write_module_file(&module_abs_path, module_name, false, is_test, is_library)?;
    Ok(())
}

fn package_name_from_dir(dir: &Path) -> String {
    if let Some(name) = dir.file_name().and_then(|os| os.to_str()) {
        return sanitize_package_name(name);
    }
    if let Ok(cwd) = env::current_dir() {
        if let Some(name) = cwd.file_name().and_then(|os| os.to_str()) {
            return sanitize_package_name(name);
        }
    }
    "prime-app".into()
}

fn sanitize_package_name(name: &str) -> String {
    let mut out = name
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || ch == '-' {
                ch
            } else {
                '-'
            }
        })
        .collect::<String>();
    if out.chars().all(|ch| ch == '-') {
        out = "prime-app".into();
    }
    out
}

fn parse_module_segments(name: &str) -> Result<Vec<String>, Box<dyn std::error::Error>> {
    if name.trim().is_empty() {
        return Err(
            io::Error::new(io::ErrorKind::InvalidInput, "module name cannot be empty").into(),
        );
    }
    let parts: Vec<_> = name
        .split(|ch| ch == ':' || ch == '.')
        .filter(|s| !s.is_empty() && *s != ":")
        .collect();
    if parts.iter().any(|segment| segment.trim().is_empty()) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "module segments cannot be empty",
        )
        .into());
    }
    let mut segments = Vec::new();
    for part in parts {
        if !part
            .chars()
            .all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
        {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("invalid module segment `{part}`"),
            )
            .into());
        }
        segments.push(part.to_string());
    }
    Ok(segments)
}

fn default_module_path(segments: &[String]) -> PathBuf {
    let mut path = PathBuf::new();
    for segment in segments {
        path.push(segment);
    }
    path.set_extension("prime");
    path
}

fn render_manifest(doc: &Value) -> Result<String, Box<dyn std::error::Error>> {
    let table = doc
        .as_table()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "manifest not a table"))?;
    let mut out = String::new();
    if let Some(Value::String(version)) = table.get("manifest_version") {
        out.push_str(&format!("manifest_version = \"{}\"\n", version));
    }
    if let Some(Value::Table(pkg)) = table.get("package") {
        out.push_str("\n[package]\n");
        for (k, v) in pkg {
            out.push_str(&format!("{} = {}\n", k, toml::to_string(v)?));
        }
    }
    if let Some(Value::Array(mods)) = table.get("modules") {
        if !mods.is_empty() {
            out.push('\n');
        }
        for entry in mods {
            if let Value::Table(t) = entry {
                out.push_str("[[modules]]\n");
                for (k, v) in t {
                    out.push_str(&format!("{} = {}\n", k, toml::to_string(v)?));
                }
                out.push('\n');
            }
        }
    }
    if let Some(Value::Array(tests)) = table.get("tests") {
        if !tests.is_empty() {
            out.push('\n');
        }
        for entry in tests {
            if let Value::Table(t) = entry {
                out.push_str("[[tests]]\n");
                for (k, v) in t {
                    out.push_str(&format!("{} = {}\n", k, toml::to_string(v)?));
                }
                out.push('\n');
            }
        }
    }
    // append any other keys in stable order
    for (k, v) in table {
        if k == "manifest_version" || k == "package" || k == "modules" || k == "tests" {
            continue;
        }
        out.push_str(&format!("\n[{k}]\n"));
        out.push_str(&toml::to_string(v)?);
        out.push('\n');
    }
    Ok(out)
}

fn normalize_relative_path(path: &Path) -> Result<PathBuf, Box<dyn std::error::Error>> {
    if path.is_absolute() {
        return Err(
            io::Error::new(io::ErrorKind::InvalidInput, "module path must be relative").into(),
        );
    }
    if path
        .components()
        .any(|component| matches!(component, Component::ParentDir))
    {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "module path cannot contain `..`",
        )
        .into());
    }
    let mut buf = path.to_path_buf();
    if buf.extension().and_then(|ext| ext.to_str()) != Some("prime") {
        buf.set_extension("prime");
    }
    Ok(buf)
}

fn manifest_path_string(path: &Path) -> String {
    path.components()
        .map(|component| component.as_os_str().to_string_lossy())
        .collect::<Vec<_>>()
        .join("/")
}

fn write_module_file(
    path: &Path,
    module_name: &str,
    include_example: bool,
    is_test: bool,
    is_library: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    if path.exists() {
        ensure_module_header(path, module_name)?;
        return Ok(());
    }
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    let header = if is_test {
        "test"
    } else if is_library {
        "library"
    } else {
        "module"
    };
    let body = if include_example {
        format!("{header} {module_name};\n\nfn main() {{\n  out(\"Hello from Prime!\");\n}}\n")
    } else {
        format!("{header} {module_name};\n\n")
    };
    fs::write(path, body)?;
    Ok(())
}

fn ensure_module_header(path: &Path, module_name: &str) -> Result<(), Box<dyn std::error::Error>> {
    let contents = fs::read_to_string(path)?;
    let trimmed = contents.trim_start();
    if trimmed.starts_with("module ")
        || trimmed.starts_with("test ")
        || trimmed.starts_with("library ")
    {
        return Ok(());
    }
    let mut updated = format!("module {module_name};\n\n");
    updated.push_str(&contents);
    fs::write(path, updated)?;
    Ok(())
}
