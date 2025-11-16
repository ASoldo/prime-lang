mod compiler;
mod diagnostics;
mod formatter;
mod language;
mod lint;
mod lsp;
mod project;
mod runtime;

use clap::{Parser, Subcommand, ValueEnum};
use compiler::Compiler;
use diagnostics::{emit_syntax_errors, report_io_error, report_runtime_error};
use formatter::format_module;
use language::parser::parse_module;
use lint::run_lint;
use project::{FileErrors, PackageError, find_manifest, load_package, manifest::PackageManifest};
use runtime::Interpreter;
use std::{
    env, fs, io,
    path::{Component, Path, PathBuf},
    process::Command,
};
use toml::{Value, value::Table as TomlTable};

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
    /// Add a module entry and stub file
    Add {
        #[arg(value_name = "MODULE")]
        name: String,
        #[arg(long, value_name = "PATH")]
        path: Option<PathBuf>,
        #[arg(long, value_enum, default_value = "pub")]
        visibility: ModuleVisibilityArg,
    },
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
        Commands::Add {
            name,
            path,
            visibility,
        } => {
            if let Err(err) = add_module(&name, path.as_deref(), visibility) {
                eprintln!("add failed: {err}");
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
        Err(PackageError::Manifest { path, message }) => {
            eprintln!("manifest error at {}: {}", path.display(), message);
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
        Err(PackageError::Manifest { path, message }) => {
            eprintln!("manifest error at {}: {}", path.display(), message);
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
    write_module_file(&main_path, "app::main", true)?;
    Ok(())
}

fn add_module(
    module_name: &str,
    explicit_path: Option<&Path>,
    visibility: ModuleVisibilityArg,
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
    let modules_value = table
        .entry("modules")
        .or_insert_with(|| Value::Array(Vec::new()));
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
    modules.push(Value::Table(entry));
    let manifest_pretty = toml::to_string_pretty(&doc)?;
    fs::write(&manifest_path, manifest_pretty)?;
    let module_abs_path = manifest_dir.join(&rel_path);
    write_module_file(&module_abs_path, module_name, false)?;
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
    let parts: Vec<_> = name.split("::").collect();
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
) -> Result<(), Box<dyn std::error::Error>> {
    if path.exists() {
        ensure_module_header(path, module_name)?;
        return Ok(());
    }
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    let body = if include_example {
        format!("module {module_name};\n\nfn main() {{\n  out(\"Hello from Prime!\");\n}}\n")
    } else {
        format!("module {module_name};\n\n")
    };
    fs::write(path, body)?;
    Ok(())
}

fn ensure_module_header(path: &Path, module_name: &str) -> Result<(), Box<dyn std::error::Error>> {
    let contents = fs::read_to_string(path)?;
    let trimmed = contents.trim_start();
    if trimmed.starts_with("module ") {
        return Ok(());
    }
    let mut updated = format!("module {module_name};\n\n");
    updated.push_str(&contents);
    fs::write(path, updated)?;
    Ok(())
}
