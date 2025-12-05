mod docs;
mod language;
mod lsp;
mod project;
mod runtime;
mod target;
#[cfg(test)]
mod tests;
mod tools;

use clap::{Parser, Subcommand, ValueEnum};
use language::{
    ast::{Item, ModuleKind},
    span::Span,
};
use language::{
    compiler::Compiler, macro_expander, parser::parse_module, typecheck,
    typecheck::TypecheckOptions,
};
use miette::NamedSource;
use project::diagnostics::analyze_manifest_issues;
use project::{
    FileErrors, PackageError, apply_manifest_header_with_manifest, canonicalize, find_manifest,
    load_package, load_package_with_manifest, manifest::PackageManifest,
    manifest::canonical_module_name, manifest::manifest_key_for, warn_manifest_drift,
};
use runtime::{Interpreter, platform};
use std::{
    env, fs, io,
    path::{Component, Path, PathBuf},
    process::Command,
    sync::{Mutex, OnceLock},
};
use target::{BuildOptions, BuildTarget};
use toml::Value;
use toml_edit::{
    self, Array, DocumentMut, InlineTable, Item as TomlItem, Table as TomlEditTable,
    Value as TomlValue,
};
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
    /// Interpret a Prime module by manifest name or file path
    Run {
        #[arg(value_name = "TARGET")]
        entry: String,
        #[arg(short = 'p', long = "project", value_name = "PACKAGE")]
        project: Option<String>,
        #[arg(
            long,
            default_value_t = false,
            help = "Require prime.lock to satisfy dependencies"
        )]
        frozen: bool,
    },
    /// Build a Prime module down to LLVM IR and a native binary
    Build {
        #[arg(value_name = "TARGET")]
        entry: String,
        #[arg(short = 'p', long = "project", value_name = "PACKAGE")]
        project: Option<String>,
        #[arg(
            long,
            default_value_t = false,
            help = "Require prime.lock to satisfy dependencies"
        )]
        frozen: bool,
        /// Target triple (e.g. riscv32imc-unknown-none-elf); defaults to host
        #[arg(long = "target", value_name = "TRIPLE")]
        target_triple: Option<String>,
        /// Optional platform hint (esp32, host); falls back to PRIME_PLATFORM
        #[arg(long = "platform", value_name = "PLATFORM")]
        platform: Option<String>,
        /// Skip flashing even if enabled in manifest build.flash
        #[arg(long, default_value_t = false)]
        no_flash: bool,
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
    /// Initialize a new Prime project
    New {
        #[arg(value_name = "DIR", default_value = ".")]
        path: PathBuf,
        #[arg(long, default_value_t = false, help = "Create a library project")]
        lib: bool,
        #[arg(
            long,
            default_value_t = false,
            help = "Create a binary project (default)"
        )]
        bin: bool,
        #[arg(
            short = 'w',
            long = "wrk",
            alias = "workspace",
            default_value_t = false,
            help = "Create a workspace with a single member project scaffold"
        )]
        workspace: bool,
    },
    /// Deprecated: use `prime-lang new` instead
    #[command(hide = true)]
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
        #[arg(short = 'p', long = "project", value_name = "PACKAGE")]
        project: Option<String>,
        #[arg(long, value_name = "PATH", conflicts_with_all = ["git", "dep_path"])]
        path: Option<PathBuf>,
        #[arg(long, value_enum, default_value = "pub")]
        visibility: ModuleVisibilityArg,
        #[arg(
            long,
            default_value_t = false,
            help = "Add a test entry instead of a module",
            conflicts_with_all = ["library", "git", "dep_path"]
        )]
        test: bool,
        #[arg(
            long,
            default_value_t = false,
            help = "Add a library entry (importable, no `main`)",
            conflicts_with_all = ["test", "git", "dep_path"]
        )]
        library: bool,
        #[arg(
            long,
            value_name = "GIT_URL",
            help = "Add a dependency from a git repository",
            conflicts_with = "dep_path"
        )]
        git: Option<String>,
        #[arg(
            long = "dep-path",
            value_name = "DIR",
            help = "Add a dependency from a local path",
            conflicts_with = "git"
        )]
        dep_path: Option<PathBuf>,
        #[arg(
            long,
            value_name = "FEATURES",
            value_delimiter = ',',
            help = "Comma-separated dependency features"
        )]
        features: Vec<String>,
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
    /// Install a tool from git
    Install {
        #[arg(long, value_name = "GIT_URL")]
        git: String,
        #[arg(long, value_name = "NAME")]
        name: Option<String>,
    },
    /// Update installed tools
    Update {
        #[arg(long, value_name = "NAME")]
        name: Option<String>,
    },
    /// Uninstall installed tools
    Uninstall {
        #[arg(value_name = "NAME")]
        name: String,
    },
    /// Sync dependencies and write prime.lock
    Sync {
        #[arg(value_name = "PATH", default_value = ".")]
        path: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Commands::Run {
            entry,
            project,
            frozen,
        } => run_entry(&entry, project.as_deref(), frozen),
        Commands::Build {
            entry,
            project,
            frozen,
            target_triple,
            platform,
            no_flash,
            name,
        } => build_entry(
            &entry,
            project.as_deref(),
            frozen,
            &name,
            target_triple,
            platform,
            no_flash,
        ),
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
        Commands::New {
            path,
            lib,
            bin,
            workspace,
        } => {
            if lib && bin {
                eprintln!("cannot use --lib and --bin together; pick one");
                std::process::exit(1);
            }
            let kind = if workspace {
                NewKind::Workspace {
                    library: lib,
                    binary: bin,
                }
            } else if lib {
                NewKind::Library
            } else {
                NewKind::Binary
            };
            if let Err(err) = init_project(&path, kind, false) {
                eprintln!("new failed: {err}");
                std::process::exit(1);
            }
        }
        Commands::Init { path } => {
            eprintln!("`prime-lang init` is deprecated; use `prime-lang new` instead.");
            if let Err(err) = init_project(&path, NewKind::Binary, true) {
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
            project,
            visibility,
            test,
            library,
            git,
            dep_path,
            features,
        } => {
            if let Err(err) = add_module(
                &name,
                path.as_deref(),
                project.as_deref(),
                visibility,
                test,
                library,
                git,
                dep_path,
                features,
            ) {
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
        Commands::Install { git, name } => {
            if let Err(err) = install_tool(&git, name.as_deref()) {
                eprintln!("install failed: {err}");
                std::process::exit(1);
            }
        }
        Commands::Update { name } => {
            if let Err(err) = update_tools(name.as_deref()) {
                eprintln!("update failed: {err}");
                std::process::exit(1);
            }
        }
        Commands::Uninstall { name } => {
            if let Err(err) = uninstall_tool(&name) {
                eprintln!("uninstall failed: {err}");
                std::process::exit(1);
            }
        }
        Commands::Sync { path } => {
            if let Err(err) = sync_deps(&path) {
                eprintln!("sync failed: {err}");
                std::process::exit(1);
            }
        }
    }
}
fn run_entry(entry: &str, project: Option<&str>, frozen: bool) {
    let (entry, entry_path, manifest) = match resolve_entry_for_cli(entry, project) {
        Ok(values) => values,
        Err(err) => exit_on_package_error(err),
    };
    ensure_prime_file(&entry_path);
    reject_non_module_entry(&entry_path, manifest.as_ref(), "run");
    if frozen {
        if let Err(err) = require_frozen(manifest.as_ref(), &entry_path) {
            eprintln!("{err}");
            std::process::exit(1);
        }
    }
    warn_manifest_drift(&entry_path);
    let run_options = BuildOptions::from_sources(None, None, manifest.as_ref());
    platform::configure_platform(&run_options);
    match load_package_with_manifest(entry.as_entry_point(), manifest) {
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
            let typecheck_options = TypecheckOptions {
                target: run_options.target.clone(),
            };
            if let Err(errors) =
                typecheck::check_program_with_options(&expanded_program, &typecheck_options)
            {
                emit_type_errors(&errors);
                std::process::exit(1);
            }
            let mut interpreter =
                Interpreter::with_target(expanded_package, run_options.target.clone());
            if let Err(err) = interpreter.run() {
                report_runtime_error(&err);
                std::process::exit(1);
            }
        }
        Err(err) => exit_on_package_error(err),
    }
}

fn compile_runtime_abi(options: &BuildOptions) -> Result<PathBuf, String> {
    static RUNTIME_ABI_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    let _runtime_guard = RUNTIME_ABI_LOCK
        .get_or_init(|| Mutex::new(()))
        .lock()
        .map_err(|_| "runtime ABI build lock poisoned".to_string())?;

    let target = &options.target;
    let runtime_dir = match target.triple() {
        Some(triple) => PathBuf::from(".build.prime/runtime").join(triple),
        None => PathBuf::from(".build.prime/runtime").join("host"),
    };
    if let Err(err) = fs::create_dir_all(&runtime_dir) {
        return Err(format!("failed to create runtime build dir: {err}"));
    }
    compile_runtime_abi_with_cargo(target, &runtime_dir, options)
}

fn compile_runtime_abi_with_cargo(
    target: &BuildTarget,
    runtime_dir: &Path,
    options: &BuildOptions,
) -> Result<PathBuf, String> {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let triple = target.triple().unwrap_or("x86_64-unknown-linux-gnu");
    let cargo_dir = runtime_dir.join("cargo");
    fs::create_dir_all(&cargo_dir)
        .map_err(|err| format!("failed to create runtime cargo dir: {err}"))?;
    let abi_path = fs::canonicalize(manifest_dir.join("src/runtime/abi.rs"))
        .map_err(|err| format!("failed to canonicalize src/runtime/abi.rs: {err}"))?;
    if env::var_os("PRIME_DEBUG_RT_ABI").is_some() {
        eprintln!(
            "[prime-debug] runtime ABI cargo build: manifest_dir={:?} abi_path={:?}",
            manifest_dir, abi_path
        );
    }
    let manifest = format!(
        r#"[package]
name = "runtime-abi"
version = "0.1.0"
edition = "2021"

[lib]
name = "runtime_abi"
path = "{}"
crate-type = ["staticlib"]
"#,
        abi_path
            .to_str()
            .ok_or_else(|| "non-utf8 path to abi.rs".to_string())?
    );
    fs::write(cargo_dir.join("Cargo.toml"), manifest)
        .map_err(|err| format!("failed to write runtime Cargo.toml: {err}"))?;
    let mut cmd = Command::new("cargo");
    // The xtensa target requires a nightly-like toolchain (esp) for build-std; default to it
    // when the user hasn't set a toolchain.
    if target.is_embedded() && env::var("RUSTUP_TOOLCHAIN").is_err() {
        cmd.env("RUSTUP_TOOLCHAIN", "esp");
    }
    if target.is_esp32_xtensa() || target.is_esp32_xtensa_espidf() {
        let flags = env::var("RUSTFLAGS").unwrap_or_default();
        let mut new_flags = flags;
        if !new_flags.is_empty() {
            new_flags.push(' ');
        }
        // Suppress the expected warning about the unstable `windowed` target feature; we need it
        // for Xtensa and it is benign for our frozen toolchain.
        new_flags.push_str(
            "-Zunstable-options -Aunstable-features -Awarnings -C target-feature=+windowed",
        );
        cmd.env("RUSTFLAGS", new_flags);
    }
    cmd.arg("build")
        .arg("--release")
        .arg("--target")
        .arg(triple)
        .arg("--manifest-path")
        .arg(cargo_dir.join("Cargo.toml"));
    if target.is_embedded() {
        cmd.arg("-Zbuild-std=core,alloc");
    }
    let linker_var = format!(
        "CARGO_TARGET_{}_LINKER",
        triple
            .chars()
            .map(|c| if c.is_ascii_alphanumeric() {
                c.to_ascii_uppercase()
            } else {
                '_'
            })
            .collect::<String>()
    );
    let default_linker = if triple.contains("xtensa") {
        "xtensa-esp32-elf-gcc"
    } else if triple.contains("riscv32") {
        "riscv32-esp-elf-gcc"
    } else {
        "gcc"
    };
    cmd.env(
        linker_var,
        env::var("PRIME_RUNTIME_LINKER").unwrap_or_else(|_| default_linker.into()),
    );
    apply_runtime_env(&mut cmd, options);
    let status = cmd
        .status()
        .map_err(|err| format!("failed to spawn cargo for runtime ABI: {err}"))?;
    if !status.success() {
        return Err("cargo failed compiling runtime ABI (build-std)".into());
    }
    let target_root = env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| cargo_dir.join("target"));
    let output_lib = target_root
        .join(triple)
        .join("release")
        .join("libruntime_abi.a");
    if output_lib.exists() {
        Ok(output_lib)
    } else {
        Err(format!(
            "expected runtime staticlib at {}, but it was not produced",
            output_lib.display()
        ))
    }
}

fn runtime_env_pairs(options: &BuildOptions) -> Vec<(&'static str, String)> {
    let mut vars = Vec::new();
    if let Some(slots) = options.runtime.task_slots {
        vars.push(("PRIME_RT_TASK_SLOTS", slots.to_string()));
    }
    if let Some(slots) = options.runtime.channel_slots {
        vars.push(("PRIME_RT_CHANNEL_SLOTS", slots.to_string()));
    }
    if let Some(cap) = options.runtime.channel_capacity {
        vars.push(("PRIME_RT_CHANNEL_CAP", cap.to_string()));
    }
    if let Some(poll_ms) = options.runtime.recv_poll_ms {
        vars.push(("PRIME_RT_RECV_POLL_MS", poll_ms.to_string()));
    }
    vars
}

fn apply_runtime_env(cmd: &mut Command, options: &BuildOptions) {
    for (key, value) in runtime_env_pairs(options) {
        cmd.env(key, value);
    }
}

fn configure_embedded_env(options: &BuildOptions) {
    if !options.target.is_embedded() {
        return;
    }
    let home = match env::var("HOME") {
        Ok(val) => PathBuf::from(val),
        Err(_) => return,
    };
    // Default to esp toolchain for embedded if caller hasn't set one.
    if env::var("RUSTUP_TOOLCHAIN").is_err() {
        unsafe { env::set_var("RUSTUP_TOOLCHAIN", "esp") };
    }
    // Keep embedded builds in a cache dir by default.
    if env::var_os("CARGO_TARGET_DIR").is_none() {
        unsafe { env::set_var("CARGO_TARGET_DIR", home.join(".cache/prime-xtensa")) };
    }
    // Xtensa-specific defaults (ESP32 classic).
    if options.target.is_esp32_xtensa() || options.target.is_esp32_xtensa_espidf() {
        let clang_root = home.join(".espressif/tools/esp-clang/esp-clang");
        let xtensa_root = home.join(".espressif/tools/xtensa-esp-elf");
        if env::var("LLVM_SYS_201_PREFIX").is_err() && clang_root.exists() {
            unsafe { env::set_var("LLVM_SYS_201_PREFIX", &clang_root) };
        }
        if env::var("LD_LIBRARY_PATH").is_err() && clang_root.join("lib").exists() {
            unsafe {
                env::set_var(
                    "LD_LIBRARY_PATH",
                    format!(
                        "/usr/lib64:/usr/lib:/lib:{}",
                        clang_root.join("lib").display()
                    ),
                )
            };
        }
        // Add esp-clang and xtensa-esp-elf bins if present.
        let mut extra_bins: Vec<PathBuf> = Vec::new();
        if clang_root.join("bin").exists() {
            extra_bins.push(clang_root.join("bin"));
        }
        if let Ok(entries) = fs::read_dir(&xtensa_root) {
            for entry in entries.flatten() {
                let candidate = entry.path().join("xtensa-esp-elf/bin");
                if candidate.exists() {
                    extra_bins.push(candidate);
                    break;
                }
            }
        }
        if !extra_bins.is_empty() {
            let current_path = env::var("PATH").unwrap_or_default();
            let mut parts: Vec<String> = extra_bins
                .into_iter()
                .map(|p| p.display().to_string())
                .collect();
            parts.push(current_path);
            unsafe { env::set_var("PATH", parts.join(":")) };
        }
        if env::var("CARGO_TARGET_XTENSA_ESP32_ESPIDF_LINKER").is_err() {
            unsafe {
                env::set_var(
                    "CARGO_TARGET_XTENSA_ESP32_ESPIDF_LINKER",
                    "xtensa-esp32-elf-gcc",
                )
            };
        }
    }

    // Apply user-provided env overrides from the manifest toolchain block.
    if let Some(env_map) = &options.toolchain.env {
        for (key, val) in env_map {
            let expanded = expand_env_vars(val);
            unsafe { env::set_var(key, expanded) };
        }
    }
}

fn build_entry(
    entry: &str,
    project: Option<&str>,
    frozen: bool,
    name: &str,
    target_flag: Option<String>,
    platform_flag: Option<String>,
    no_flash: bool,
) {
    let (entry, entry_path, manifest) = match resolve_entry_for_cli(entry, project) {
        Ok(values) => values,
        Err(err) => exit_on_package_error(err),
    };
    let mut build_options =
        BuildOptions::from_sources(target_flag, platform_flag, manifest.as_ref());
    if no_flash {
        build_options.flash.enabled = false;
    }
    ensure_prime_file(&entry_path);
    reject_non_module_entry(&entry_path, manifest.as_ref(), "build");
    if frozen {
        if let Err(err) = require_frozen(manifest.as_ref(), &entry_path) {
            eprintln!("{err}");
            std::process::exit(1);
        }
    }
    warn_manifest_drift(&entry_path);
    platform::configure_platform(&build_options);
    match load_package_with_manifest(entry.as_entry_point(), manifest) {
        Ok(package) => {
            configure_embedded_env(&build_options);
            let expanded_program = expand_or_report(&package.program);
            println!(
                "[prime-debug] starting build for {} target={:?}",
                entry_path.display(),
                build_options.target
            );
            let typecheck_options = TypecheckOptions {
                target: build_options.target.clone(),
            };
            if let Err(errors) =
                typecheck::check_program_with_options(&expanded_program, &typecheck_options)
            {
                emit_type_errors(&errors);
                std::process::exit(1);
            }
            println!("[prime-debug] typecheck ok, compiling program...");
            let mut compiler = Compiler::with_target(build_options.target.clone());
            println!("[prime-debug] invoking compiler.compile_program()");
            if let Err(err) = compiler.compile_program(&expanded_program.program) {
                eprintln!("Build failed: {err}");
                std::process::exit(1);
            }
            println!("[prime-debug] compiler finished");
            let build_root = Path::new(".build.prime");
            if let Err(err) = fs::create_dir_all(build_root) {
                eprintln!("Failed to create build directory: {err}");
                std::process::exit(1);
            }
            let artifact_dir = match build_options.target.triple() {
                Some(triple) => build_root.join(format!("{name}-{triple}")),
                None => build_root.join(name),
            };
            if let Err(err) = fs::create_dir_all(&artifact_dir) {
                eprintln!("Failed to create artifact directory: {err}");
                std::process::exit(1);
            }
            let ir_path = artifact_dir.join(format!("{name}.ll"));
            if let Err(err) = compiler.write_ir_to(&ir_path) {
                eprintln!("Failed to write IR: {err}");
                std::process::exit(1);
            }
            println!("[prime-debug] wrote IR {}", ir_path.display());
            let obj_path = artifact_dir.join(format!("{name}.o"));
            if let Err(err) = run_llc(&ir_path, &obj_path, &build_options.target) {
                eprintln!("{err}");
                std::process::exit(1);
            }
            println!("[prime-debug] llc done -> {}", obj_path.display());
            let runtime_lib = match compile_runtime_abi(&build_options) {
                Ok(path) => Some(path),
                Err(err) => {
                    eprintln!("Failed to compile runtime ABI: {err}");
                    std::process::exit(1);
                }
            };
            println!("[prime-debug] runtime lib {:?}", runtime_lib);
            let bin_path = artifact_dir.join(name);
            if let Err(err) =
                run_gcc_with_runtime(&obj_path, runtime_lib.as_deref(), &bin_path, &build_options)
            {
                eprintln!("{err}");
                std::process::exit(1);
            }
            println!("[prime-debug] link done -> {}", bin_path.display());
            if let Err(err) = maybe_objcopy_and_flash(&bin_path, &build_options) {
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

#[derive(Clone, Copy, PartialEq, Eq)]
enum NewKind {
    Binary,
    Library,
    Workspace { library: bool, binary: bool },
}

#[derive(Clone)]
enum EntryTarget {
    Path(PathBuf),
    Module(String),
}

impl EntryTarget {
    fn as_entry_point(&self) -> project::EntryPoint<'_> {
        match self {
            EntryTarget::Path(path) => project::EntryPoint::Path(path),
            EntryTarget::Module(name) => project::EntryPoint::Module(name),
        }
    }
}

fn resolve_entry_for_cli(
    target: &str,
    project: Option<&str>,
) -> Result<(EntryTarget, PathBuf, Option<PackageManifest>), PackageError> {
    let entry = parse_entry_target(target);
    let manifest = project::load_manifest_for_entry(entry.as_entry_point(), project)?;
    if project.is_some() && manifest.is_none() {
        let manifest_stub = env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("."))
            .join("prime.toml");
        return Err(PackageError::Manifest {
            path: manifest_stub,
            message: "workspace manifest required when using --project".into(),
        });
    }
    let entry_path =
        project::resolve_entry_path(entry.as_entry_point(), manifest.as_ref(), project)?;
    Ok((entry, entry_path, manifest))
}

fn parse_entry_target(target: &str) -> EntryTarget {
    let trimmed = target.trim();
    let path_candidate = PathBuf::from(trimmed);
    let looks_like_path = trimmed.contains(std::path::MAIN_SEPARATOR)
        || trimmed.contains('/')
        || trimmed.contains('\\')
        || path_candidate
            .extension()
            .and_then(|ext| ext.to_str())
            .map(|ext| ext == "prime")
            .unwrap_or(false)
        || path_candidate.exists();
    if looks_like_path {
        EntryTarget::Path(path_candidate)
    } else {
        EntryTarget::Module(trimmed.to_string())
    }
}

fn reject_non_module_entry(path: &Path, manifest: Option<&PackageManifest>, command: &str) {
    if let Some(manifest) = manifest {
        let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        if let Some(name) = manifest.module_name_for_path(&canonical) {
            if let Some(kind) = manifest.module_kind(&name) {
                if kind != ModuleKind::Module {
                    eprintln!(
                        "`{}` is listed as {:?} in {} and cannot be used with `prime-lang {}`",
                        name,
                        kind,
                        manifest.path.display(),
                        command
                    );
                    std::process::exit(1);
                }
            }
        }
    }
    let (test_msg, lib_msg) = entry_error_messages(command);
    if is_test_file(path) {
        eprintln!("{test_msg}");
        std::process::exit(1);
    }
    if is_library_file(path) {
        eprintln!("{lib_msg}");
        std::process::exit(1);
    }
}

fn entry_error_messages(command: &str) -> (&'static str, &'static str) {
    match command {
        "run" => (
            "`prime-lang run` cannot execute test targets; use `prime-lang test`",
            "`prime-lang run` cannot execute library targets; use a module with `main`",
        ),
        "build" => (
            "`prime-lang build` cannot compile test targets; use `prime-lang test`",
            "`prime-lang build` cannot compile library targets; use a module with `main`",
        ),
        _ => (
            "`prime-lang` cannot operate on test targets for this command",
            "`prime-lang` cannot operate on library targets for this command",
        ),
    }
}

fn require_frozen(manifest: Option<&PackageManifest>, entry_path: &Path) -> Result<(), String> {
    let Some(manifest) = manifest else {
        return Err("`--frozen` requires a manifest to resolve dependencies".into());
    };
    let Some(lock) = manifest.lock_entries() else {
        return Err(format!(
            "`--frozen` requires prime.lock (missing near {})",
            entry_path.display()
        ));
    };
    for dep in manifest.dependencies() {
        if let project::manifest::DependencySource::Git { .. } = dep.source {
            let entry = lock.iter().find(|locked| locked.name == dep.name);
            if entry.is_none() {
                return Err(format!(
                    "`--frozen` missing lock entry for dependency `{}`",
                    dep.name
                ));
            }
            if entry.and_then(|e| e.rev.as_ref()).is_none() {
                return Err(format!(
                    "`--frozen` requires locked revision for dependency `{}`",
                    dep.name
                ));
            }
        }
    }
    Ok(())
}

fn exit_on_package_error(err: PackageError) -> ! {
    match err {
        PackageError::Io { path, error } => {
            report_io_error(&path, &error);
        }
        PackageError::Syntax(errors) => {
            emit_syntax_errors(&errors);
        }
        PackageError::Manifest { path, message } => {
            eprintln!("manifest error at {}: {}", path.display(), message);
        }
    }
    std::process::exit(1);
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

fn run_llc(ir_path: &Path, obj_path: &Path, target: &BuildTarget) -> Result<(), String> {
    let mut cmd = if target.is_esp32_xtensa() || target.is_esp32_xtensa_espidf() {
        let llc_path = env::var("PRIME_LLC").ok().or_else(|| {
            env::var("LLVM_SYS_201_PREFIX")
                .or_else(|_| env::var("LLVM_SYS_211_PREFIX"))
                .ok()
                .map(|p| format!("{p}/bin/llc"))
                .or_else(|| {
                    env::var("HOME")
                        .ok()
                        .map(|home| format!("{home}/.espressif/tools/esp-clang/esp-clang/bin/llc"))
                })
        });
        Command::new(llc_path.unwrap_or_else(|| "llc".to_string()))
    } else {
        let llc_path = env::var("PRIME_LLC")
            .ok()
            .or_else(|| {
                env::var("LLVM_SYS_211_PREFIX")
                    .ok()
                    .map(|p| format!("{p}/bin/llc"))
            })
            .or_else(|| {
                env::var("LLVM_SYS_201_PREFIX")
                    .ok()
                    .map(|p| format!("{p}/bin/llc"))
            });
        Command::new(llc_path.unwrap_or_else(|| "llc".to_string()))
    };
    // Xtensa toolchains typically build static images; PIC relocations are not supported.
    let relocation_model = if target.is_esp32_xtensa() || target.is_esp32_xtensa_espidf() {
        "static"
    } else {
        "pic"
    };
    cmd.arg(format!("-relocation-model={relocation_model}"))
        .arg("-filetype=obj")
        .arg(ir_path)
        .arg("-o")
        .arg(obj_path);
    // Normalize triples for toolchain expectations.
    if target.is_esp32_xtensa() || target.is_esp32_xtensa_espidf() {
        // Prefer esp32 little-endian triple for Xtensa; keep windowed ABI to match esp toolchain.
        cmd.arg("-mtriple=xtensa-esp32-elf")
            .arg("-mcpu=esp32")
            .arg("-mattr=+windowed");
    } else if let Some(triple) = target.triple() {
        cmd.arg(format!("-mtriple={triple}"));
    } else {
        // Ask llvm-config for the default triple; fall back to a standard host triple.
        let llvm_triple = Command::new("llvm-config")
            .arg("--host-target")
            .output()
            .ok()
            .and_then(|o| {
                if o.status.success() {
                    String::from_utf8(o.stdout)
                        .ok()
                        .map(|s| s.trim().to_string())
                } else {
                    None
                }
            });
        let triple = llvm_triple.unwrap_or_else(|| "x86_64-pc-linux-gnu".to_string());
        cmd.arg(format!("-mtriple={triple}"));
    }
    if target.is_esp32c3() {
        cmd.arg("-march=rv32imc");
    }
    let output = cmd
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
    build_options: &BuildOptions,
) -> Result<(), String> {
    match &build_options.target {
        BuildTarget::Host => link_host(obj_path, runtime_lib, bin_path),
        BuildTarget::Triple(_) if build_options.target.is_embedded() => {
            link_esp32(obj_path, runtime_lib, bin_path, build_options)
        }
        BuildTarget::Triple(triple) => Err(format!(
            "unsupported target triple `{triple}`; supported embedded targets: {}",
            crate::target::embedded_target_hint()
        )),
    }
}

fn maybe_objcopy_and_flash(bin_path: &Path, build_options: &BuildOptions) -> Result<(), String> {
    if !build_options.target.is_embedded() {
        return Ok(());
    }
    let esptool = resolve_esptool(build_options);
    let bin_image = bin_path.with_extension("bin");
    // Prefer generating a proper ESP image header with elf2image; fall back to objcopy if unavailable.
    let elf2image_output = Command::new(&esptool)
        .arg("--chip")
        .arg(if build_options.target.is_esp32c3() {
            "esp32c3"
        } else {
            "esp32"
        })
        .arg("elf2image")
        .arg("--flash_mode")
        .arg("dio")
        .arg("--flash_freq")
        .arg("40m")
        .arg("--flash_size")
        .arg("4MB")
        .arg("-o")
        .arg(&bin_image)
        .arg(bin_path)
        .output();
    match elf2image_output {
        Ok(output) if output.status.success() => {}
        _ => {
            let objcopy = build_options
                .toolchain
                .objcopy
                .clone()
                .or_else(|| env::var("PRIME_RISCV_OBJCOPY").ok());
            if let Some(objcopy) = objcopy {
                let output = Command::new(&objcopy)
                    .arg("-O")
                    .arg("binary")
                    .arg(bin_path)
                    .arg(&bin_image)
                    .output()
                    .map_err(|err| format!("Failed to run objcopy: {err}"))?;
                if !output.status.success() {
                    return Err(format!(
                        "objcopy failed:\n{}",
                        String::from_utf8_lossy(&output.stderr)
                    ));
                }
            }
        }
    }
    if build_options.flash.enabled {
        flash_esp32(&bin_image, build_options)?;
    }
    Ok(())
}

fn resolve_esptool(build_options: &BuildOptions) -> String {
    build_options
        .toolchain
        .esptool
        .clone()
        .or_else(|| env::var("PRIME_ESPTOOL").ok())
        .unwrap_or_else(|| "esptool".to_string())
}

fn flash_esp32(bin_image: &Path, build_options: &BuildOptions) -> Result<(), String> {
    let esptool = resolve_esptool(build_options);
    let chip = if build_options.target.is_esp32_xtensa()
        || build_options.target.is_esp32_xtensa_espidf()
    {
        "esp32"
    } else {
        "esp32c3"
    };
    let port = build_options
        .flash
        .port
        .clone()
        .or_else(|| env::var("PRIME_ESP_PORT").ok())
        .unwrap_or_else(|| "/dev/ttyUSB0".to_string());
    let baud = build_options
        .flash
        .baud
        .or_else(|| env::var("PRIME_ESP_BAUD").ok().and_then(|s| s.parse().ok()))
        .unwrap_or(460800);
    let address = build_options
        .flash
        .address
        .clone()
        .unwrap_or_else(|| "0x10000".to_string());
    let mut cmd = Command::new(esptool);
    cmd.arg("--chip")
        .arg(chip)
        .arg("--port")
        .arg(&port)
        .arg("--baud")
        .arg(format!("{baud}"))
        .arg("write-flash")
        .arg("-z");

    // Include bootloader and partition table if present to avoid invalid header resets.
    let default_bootloader = PathBuf::from(env::var("HOME").unwrap_or_else(|_| ".".into()))
        .join("esp/esp-idf/examples/get-started/blink/build/bootloader/bootloader.bin");
    let default_partitions = PathBuf::from(env::var("HOME").unwrap_or_else(|_| ".".into()))
        .join("esp/esp-idf/examples/get-started/blink/build/partition_table/partition-table.bin");
    let bootloader = env::var("PRIME_ESP_BOOTLOADER")
        .map(PathBuf::from)
        .unwrap_or(default_bootloader);
    let partitions = env::var("PRIME_ESP_PARTITIONS")
        .map(PathBuf::from)
        .unwrap_or(default_partitions);
    if bootloader.exists() {
        cmd.arg("0x1000").arg(&bootloader);
    }
    if partitions.exists() {
        cmd.arg("0x8000").arg(&partitions);
    }
    cmd.arg(&address).arg(bin_image);

    let output = cmd
        .output()
        .map_err(|err| format!("Failed to execute esptool.py: {err}"))?;
    if !output.status.success() {
        return Err(format!(
            "esptool.py failed:\n{}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    println!(
        "Flashed {} to {} at {} (baud {})",
        bin_image.display(),
        port,
        address,
        baud
    );
    Ok(())
}

fn link_host(obj_path: &Path, runtime_lib: Option<&Path>, bin_path: &Path) -> Result<(), String> {
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

fn expand_env_vars(raw: &str) -> String {
    let mut out = String::new();
    let mut chars = raw.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '$' {
            if matches!(chars.peek(), Some('{')) {
                let _ = chars.next();
                let mut name = String::new();
                while let Some(next) = chars.next() {
                    if next == '}' {
                        break;
                    }
                    name.push(next);
                }
                if !name.is_empty() {
                    if let Ok(val) = env::var(&name) {
                        out.push_str(&val);
                    } else {
                        out.push_str("${");
                        out.push_str(&name);
                        out.push('}');
                    }
                    continue;
                }
            } else {
                let mut name = String::new();
                while let Some(peek) = chars.peek() {
                    if peek.is_alphanumeric() || *peek == '_' {
                        name.push(*peek);
                        let _ = chars.next();
                    } else {
                        break;
                    }
                }
                if !name.is_empty() {
                    if let Ok(val) = env::var(&name) {
                        out.push_str(&val);
                    } else {
                        out.push('$');
                        out.push_str(&name);
                    }
                    continue;
                }
            }
        }
        out.push(ch);
    }
    out
}

fn link_esp32(
    obj_path: &Path,
    runtime_lib: Option<&Path>,
    bin_path: &Path,
    build_options: &BuildOptions,
) -> Result<(), String> {
    if build_options.target.is_esp32_xtensa_espidf() && env::var("IDF_PATH").is_err() {
        return Err("ESP-IDF builds require IDF_PATH to point at your ESP-IDF checkout (see docs/esp32_toolchain_template.toml)".to_string());
    }
    let cc_default = if build_options.target.is_esp32_xtensa()
        || build_options.target.is_esp32_xtensa_espidf()
    {
        "xtensa-esp32-elf-gcc".to_string()
    } else {
        "riscv32-esp-elf-gcc".to_string()
    };
    let cc = build_options
        .toolchain
        .cc
        .clone()
        .unwrap_or_else(|| cc_default.clone());
    let ld_script_raw = build_options
        .toolchain
        .ld_script
        .clone()
        .or_else(|| env::var("PRIME_RISCV_LD_SCRIPT").ok())
        .ok_or_else(|| {
            "No linker script set; configure build.toolchain.ld_script or PRIME_RISCV_LD_SCRIPT"
                .to_string()
        })?;
    let ld_script = expand_env_vars(&ld_script_raw);
    if ld_script.contains("${") || ld_script.contains("$IDF_PATH") {
        return Err(format!(
            "Linker script still contains unresolved env vars: {} (set IDF_PATH or update build.toolchain.ld_script)",
            ld_script_raw
        ));
    }
    let mut cmd = Command::new(cc);
    if build_options.target.is_esp32c3() {
        cmd.arg("-march=rv32imc").arg("-mabi=ilp32");
    }
    if build_options.target.is_esp32_xtensa() || build_options.target.is_esp32_xtensa_espidf() {
        // Ensure endianness matches xtensa config.
        cmd.arg("-mlongcalls").arg("-Wl,-EL");
    }
    // Keep the Xtensa entry symbol so the image has a valid entry point.
    cmd.arg("-Wl,-u,call_user_start_cpu0")
        .arg("-Wl,-e,call_user_start_cpu0");
    cmd.arg("-nostdlib");
    // Allow passing a sections.ld from ESP-IDF; include memory.ld before it.
    // If memory.ld is already in ld_flags, pull it out and order it before sections.ld.
    let mut ld_scripts: Vec<PathBuf> = Vec::new();
    let mut ld_flags_val = build_options
        .toolchain
        .ld_flags
        .clone()
        .or_else(|| env::var("PRIME_RISCV_LD_FLAGS").ok());
    let mut extra_args: Vec<String> = Vec::new();
    if let Some(flags) = ld_flags_val.take() {
        let expanded_flags = expand_env_vars(&flags);
        if expanded_flags.contains("${") || expanded_flags.contains("$IDF_PATH") {
            return Err(format!(
                "Linker flags still contain unresolved env vars: {} (set IDF_PATH or update build.toolchain.ld_flags)",
                flags
            ));
        }
        for tok in expanded_flags.split_whitespace() {
            if tok.starts_with("-T") && tok.contains("memory.ld") {
                let path_str = tok.trim_start_matches("-T");
                ld_scripts.push(PathBuf::from(path_str));
            } else {
                extra_args.push(tok.to_string());
            }
        }
    }
    if ld_script.ends_with("sections.ld") {
        let sibling_memory = PathBuf::from(&ld_script)
            .parent()
            .map(|p| p.join("memory.ld"));
        let already_have_mem = ld_scripts
            .iter()
            .any(|p| p.file_name().map(|n| n == "memory.ld").unwrap_or(false));
        if let Some(mem) = sibling_memory {
            if mem.exists() && !already_have_mem {
                ld_scripts.push(mem);
            }
        }
    }
    // Finally add the primary script (sections or otherwise).
    ld_scripts.push(PathBuf::from(&ld_script));
    for script in ld_scripts {
        cmd.arg(format!("-T{}", script.display()));
    }
    cmd.arg(obj_path);
    if let Some(startup) = build_options
        .toolchain
        .startup_obj
        .clone()
        .or_else(|| env::var("PRIME_RISCV_STARTUP_OBJ").ok())
    {
        cmd.arg(expand_env_vars(&startup));
    }
    if let Some(lib) = runtime_lib {
        cmd.arg(lib);
    }
    // Minimal runtime: include libc for memset/memcpy and libgcc for compiler builtins.
    cmd.arg("-lc");
    cmd.arg("-lgcc");
    if !extra_args.is_empty() {
        cmd.args(&extra_args);
    }
    cmd.arg("-Wl,--gc-sections").arg("-o").arg(bin_path);

    let output = cmd
        .output()
        .map_err(|err| format!("Failed to execute ESP32 linker: {err}"))?;
    if !output.status.success() {
        return Err(format!(
            "ESP32 link failed:\n{}\n{}",
            String::from_utf8_lossy(&output.stdout),
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
    text[..offset.min(text.len())]
        .bytes()
        .filter(|b| *b == b'\n')
        .count()
        + 1
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

fn init_project(
    dir: &Path,
    kind: NewKind,
    warn_deprecated: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    if !dir.exists() {
        fs::create_dir_all(dir)?;
    }
    match kind {
        NewKind::Workspace { library, binary } => {
            let package_name = package_name_from_dir(dir);
            let member_dir = dir.join(&package_name);
            if dir.join("prime.toml").exists() {
                return Err(io::Error::new(
                    io::ErrorKind::AlreadyExists,
                    format!("{} already exists", dir.join("prime.toml").display()),
                )
                .into());
            }
            init_project(
                &member_dir,
                if library {
                    NewKind::Library
                } else if binary {
                    NewKind::Binary
                } else {
                    NewKind::Binary
                },
                warn_deprecated,
            )?;
            let member_rel = member_dir
                .strip_prefix(dir)
                .unwrap_or(&member_dir)
                .to_path_buf();
            let workspace_manifest = format!(
                r#"manifest_version = "3"

[workspace]
members = [
  "{member}"
]
"#,
                member = manifest_path_string(&member_rel)
            );
            fs::write(dir.join("prime.toml"), workspace_manifest)?;
        }
        NewKind::Binary | NewKind::Library => {
            let manifest_path = dir.join("prime.toml");
            if manifest_path.exists() {
                return Err(io::Error::new(
                    io::ErrorKind::AlreadyExists,
                    format!("{} already exists", manifest_path.display()),
                )
                .into());
            }
            let package_name = package_name_from_dir(dir);
            let module_name = match kind {
                NewKind::Binary => format!("{}::main", package_name.replace('-', "_")),
                NewKind::Library => format!("{}::lib", package_name.replace('-', "_")),
                _ => unreachable!(),
            };
            let entry_path = if kind == NewKind::Binary {
                "main.prime"
            } else {
                "lib.prime"
            };
            let manifest = if kind == NewKind::Binary {
                format!(
                    r#"manifest_version = "3"

[package]
name = "{package_name}"
version = "0.1.0"

[module]
name = "{module_name}"
path = "{entry_path}"
visibility = "pub"
"#
                )
            } else {
                let module_key = manifest_key_for(&module_name);
                format!(
                    r#"manifest_version = "3"

[package]
name = "{package_name}"
version = "0.1.0"

[libraries]
{module_key} = {{ name = "{module_name}", path = "{entry_path}", visibility = "pub" }}
"#
                )
            };
            fs::write(&manifest_path, manifest)?;
            let main_path = manifest_path.parent().unwrap_or(dir).join(entry_path);
            write_module_file(
                &main_path,
                &module_name,
                true,
                false,
                matches!(kind, NewKind::Library),
            )?;
        }
    }
    if warn_deprecated {
        eprintln!("`prime-lang init` will be removed in a future release; use `prime-lang new`.");
    }
    Ok(())
}

fn add_module(
    module_name: &str,
    explicit_path: Option<&Path>,
    project: Option<&str>,
    visibility: ModuleVisibilityArg,
    is_test: bool,
    is_library: bool,
    dep_git: Option<String>,
    dep_path: Option<PathBuf>,
    dep_features: Vec<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    if dep_git.is_some() || dep_path.is_some() {
        return add_dependency_entry(module_name, project, dep_git, dep_path, dep_features);
    }
    let segments = parse_module_segments(module_name)?;
    let cwd = env::current_dir()?;
    let manifest_path = resolve_edit_manifest(&cwd, project)?;
    let manifest_dir = manifest_path
        .parent()
        .map(Path::to_path_buf)
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "invalid manifest path"))?;
    let manifest_text = fs::read_to_string(&manifest_path)?;
    let mut doc: DocumentMut = manifest_text.parse().map_err(|err| {
        io::Error::new(
            io::ErrorKind::Other,
            format!("failed to parse manifest: {err:?}"),
        )
    })?;
    match doc["manifest_version"].as_str() {
        Some("3") => {}
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "`prime add` requires manifest_version = \"3\"",
            )
            .into());
        }
    }
    let rel_path = match explicit_path {
        Some(path) => normalize_relative_path(path)?,
        None => default_module_path(&segments),
    };
    let manifest_path_string = manifest_path_string(&rel_path);
    let manifest_current = PackageManifest::load(&manifest_path).map_err(|err| {
        io::Error::new(
            io::ErrorKind::Other,
            format!("failed to load manifest: {err:?}"),
        )
    })?;
    let canonical = canonical_module_name(module_name);
    if manifest_current.module_path(&canonical).is_some() {
        return Err(
            io::Error::new(io::ErrorKind::Other, "module already exists in manifest").into(),
        );
    }
    let key = manifest_key_for(module_name);
    let mut entry = TomlEditTable::new();
    entry["name"] = toml_edit::value(&canonical);
    entry["path"] = toml_edit::value(manifest_path_string.clone());
    entry["visibility"] = toml_edit::value(visibility.as_manifest_str());
    if is_library {
        entry["kind"] = toml_edit::value("library");
    }
    let mut inline = InlineTable::new();
    inline.insert("name", TomlValue::from(&canonical));
    inline.insert("path", TomlValue::from(manifest_path_string));
    inline.insert("visibility", TomlValue::from(visibility.as_manifest_str()));
    if is_library {
        inline.insert("kind", TomlValue::from("library"));
    }
    if is_test {
        set_inline_table_entry(doc.entry("tests"), key, inline);
    } else if is_library {
        set_inline_table_entry(doc.entry("libraries"), key, inline);
    } else {
        set_inline_table_entry(doc.entry("modules"), key, inline);
        if !doc["module"].is_table() {
            doc["module"] = TomlItem::Table(entry);
        }
    }
    reorder_manifest(&mut doc);
    fs::write(&manifest_path, doc.to_string())?;
    let module_abs_path = manifest_dir.join(&rel_path);
    write_module_file(&module_abs_path, module_name, false, is_test, is_library)?;
    Ok(())
}

fn add_dependency_entry(
    dep_name: &str,
    project: Option<&str>,
    dep_git: Option<String>,
    dep_path: Option<PathBuf>,
    dep_features: Vec<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    let debug = std::env::var_os("PRIME_DEBUG_ADD").is_some();
    if dep_git.is_none() && dep_path.is_none() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "dependency requires --git or --dep-path",
        )
        .into());
    }
    let cwd = env::current_dir()?;
    let manifest_path = resolve_edit_manifest(&cwd, project)?;
    let manifest_dir = manifest_path
        .parent()
        .map(Path::to_path_buf)
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "invalid manifest path"))?;
    let manifest_text = fs::read_to_string(&manifest_path)?;
    if debug {
        eprintln!(
            "debug add-dep: editing manifest {}",
            manifest_path.display()
        );
    }
    let mut doc: DocumentMut = manifest_text.parse().map_err(|err| {
        io::Error::new(
            io::ErrorKind::Other,
            format!("failed to parse manifest: {err:?}"),
        )
    })?;
    if debug {
        eprintln!("debug add-dep: parsed manifest");
    }
    match doc["manifest_version"].as_str() {
        Some("3") => {}
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "`prime add --git/--dep-path` requires manifest_version = \"3\"",
            )
            .into());
        }
    }
    if !doc["package"].is_table() {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "dependencies must be added inside a package manifest (not workspace root)",
        )
        .into());
    }
    let deps_item = doc
        .entry("dependencies")
        .or_insert(TomlItem::Table(TomlEditTable::new()));
    let deps_table = deps_item
        .as_table_like_mut()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "`dependencies` must be a table"))?;
    if debug {
        eprintln!("debug add-dep: adding entry for {dep_name}");
    }
    let mut entry = InlineTable::new();
    entry.insert("name", TomlValue::from(dep_name));
    if let Some(url) = dep_git.as_ref() {
        entry.insert("git", TomlValue::from(url));
    }
    if let Some(path) = dep_path.as_ref() {
        let rel = path
            .strip_prefix(&manifest_dir)
            .unwrap_or(path)
            .to_path_buf();
        entry.insert("path", TomlValue::from(manifest_path_string(&rel)));
    }
    if !dep_features.is_empty() {
        let mut arr = Array::new();
        for feature in &dep_features {
            arr.push(feature);
        }
        entry.insert("features", TomlValue::Array(arr));
    }
    let key = manifest_key_for(dep_name);
    deps_table.insert(&key, TomlItem::Value(toml_edit::Value::InlineTable(entry)));
    if debug {
        eprintln!("debug add-dep: writing manifest");
    }
    reorder_manifest(&mut doc);
    reorder_manifest(&mut doc);
    fs::write(&manifest_path, doc.to_string())?;

    let lock_path = manifest_dir.join("prime.lock");
    let mut lock = crate::project::lock::load_lockfile(&lock_path).unwrap_or_else(|| {
        PackageManifest::load(&manifest_path)
            .map(|m| m.lock_dependencies())
            .unwrap_or_default()
    });

    let mut locked = crate::project::lock::LockedDependency {
        name: dep_name.to_string(),
        package: None,
        git: dep_git.clone(),
        path: dep_path.as_ref().map(|p| {
            p.strip_prefix(&manifest_dir)
                .unwrap_or(p)
                .to_string_lossy()
                .to_string()
        }),
        rev: None,
        features: if dep_features.is_empty() {
            None
        } else {
            Some(dep_features.clone())
        },
    };

    if let Some(url) = dep_git {
        let cache_root = manifest_dir.join(".prime/deps");
        match project::deps::ensure_git_checkout(&url, dep_name, None, &cache_root) {
            Ok(path) => {
                if let Some(rev) = project::deps::repo_head_rev(&path) {
                    locked.rev = Some(rev);
                }
                println!("fetched dependency `{dep_name}` into {}", path.display());
            }
            Err(message) => {
                eprintln!("warning: failed to fetch dependency `{dep_name}`: {message}");
            }
        }
    }

    if let Some(existing) = lock.dependencies.iter_mut().find(|d| d.name == locked.name) {
        *existing = locked;
    } else {
        lock.dependencies.push(locked);
    }
    if let Err(err) = crate::project::lock::write_lockfile(&lock_path, &lock) {
        eprintln!("warning: failed to write lockfile: {err}");
    }
    Ok(())
}

fn install_tool(git: &str, name: Option<&str>) -> Result<(), String> {
    let dest = project::deps::install_tool(git, name)?;
    println!("Installed tool at {}", dest.display());
    Ok(())
}

fn sync_deps(path: &Path) -> Result<(), String> {
    let manifest_path = find_manifest(path).ok_or_else(|| {
        "could not locate prime.toml in current or parent directories".to_string()
    })?;
    let manifest = PackageManifest::load(&manifest_path).map_err(|err| {
        format!(
            "failed to load manifest {}: {err:?}",
            manifest_path.display()
        )
    })?;
    let lock = manifest.lock_dependencies();
    let lock_path = manifest_path
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .join("prime.lock");
    project::lock::write_lockfile(&lock_path, &lock)?;
    println!("Wrote {}", lock_path.display());
    Ok(())
}

fn resolve_edit_manifest(
    cwd: &Path,
    project: Option<&str>,
) -> Result<PathBuf, Box<dyn std::error::Error>> {
    let found = find_manifest(cwd).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::NotFound,
            "could not locate prime.toml in current or parent directories",
        )
    })?;
    // if a workspace and project was requested, target the member manifest
    if let Some(project) = project {
        if let Ok(workspace) = toml::from_str::<Value>(&fs::read_to_string(&found)?) {
            if let Some(Value::Table(ws)) = workspace.get("workspace") {
                let members = ws
                    .get("members")
                    .and_then(|v| v.as_array())
                    .cloned()
                    .unwrap_or_default();
                let root = found
                    .parent()
                    .map(|p| p.to_path_buf())
                    .unwrap_or_else(|| PathBuf::from("."));
                for member in members {
                    if let Some(path_str) = member.as_str() {
                        let member_path = root.join(path_str).join("prime.toml");
                        if !member_path.exists() {
                            continue;
                        }
                        if path_str == project {
                            return Ok(member_path);
                        }
                        if let Ok(manifest_text) = fs::read_to_string(&member_path) {
                            if let Ok(manifest) = toml::from_str::<Value>(&manifest_text) {
                                if manifest
                                    .get("package")
                                    .and_then(|pkg| pkg.get("name"))
                                    .and_then(|v| v.as_str())
                                    == Some(project)
                                {
                                    return Ok(member_path);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    let candidate = cwd.join("prime.toml");
    if candidate.exists() {
        if let Ok(text) = fs::read_to_string(&candidate) {
            if let Ok(value) = toml::from_str::<Value>(&text) {
                if !value.get("workspace").is_some() {
                    return Ok(candidate);
                }
            }
        }
    }
    Ok(found)
}

fn update_tools(name: Option<&str>) -> Result<(), String> {
    let updated = project::deps::update_tools(name)?;
    if updated.is_empty() {
        println!("No tools updated");
    } else {
        for path in updated {
            println!("Updated {}", path.display());
        }
    }
    Ok(())
}

fn uninstall_tool(name: &str) -> Result<(), String> {
    project::deps::uninstall_tool(name)?;
    println!("Uninstalled `{name}`");
    Ok(())
}

fn set_inline_table_entry(entry: toml_edit::Entry<'_>, key: String, table: InlineTable) {
    let item = entry.or_insert(TomlItem::Table(TomlEditTable::new()));
    if let Some(obj) = item.as_table_like_mut() {
        obj.insert(&key, TomlItem::Value(toml_edit::Value::InlineTable(table)));
    }
}

fn reorder_manifest(doc: &mut DocumentMut) {
    const ORDER: &[&str] = &[
        "manifest_version",
        "package",
        "dependencies",
        "modules",
        "libraries",
        "tests",
        "workspace",
    ];
    let mut map: std::collections::HashMap<String, TomlItem> = doc
        .iter()
        .map(|(k, _)| (k.to_string(), doc[k].clone()))
        .collect();
    let unknown_order: Vec<String> = doc
        .iter()
        .map(|(k, _)| k.to_string())
        .filter(|k| !ORDER.contains(&k.as_str()))
        .collect();
    let mut new_doc: DocumentMut = "".parse().unwrap();
    for key in ORDER {
        if let Some(item) = map.remove(*key) {
            new_doc.insert(key, item);
        }
    }
    for key in unknown_order {
        if let Some(item) = map.remove(&key) {
            new_doc.insert(&key, item);
        }
    }
    *doc = new_doc;
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
    if name.contains('.') {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "module name cannot contain '.'; use '::' or '/' separators",
        )
        .into());
    }
    let parts: Vec<_> = name
        .split(|ch| ch == ':' || ch == '/')
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
            .all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '-')
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

#[allow(dead_code)]
fn render_manifest(doc: &Value) -> Result<String, Box<dyn std::error::Error>> {
    Ok(toml::to_string_pretty(doc)?)
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
        match header {
            "module" => {
                format!(
                    "{header} {module_name};\n\nfn main() {{\n  out(\"Hello from Prime!\");\n}}\n"
                )
            }
            "library" => {
                format!(
                    "{header} {module_name};\n\nexport prelude {{ example }};\n\npub fn example() -> int32 {{\n  0\n}}\n"
                )
            }
            "test" => {
                format!("{header} {module_name};\n\nfn example_test() -> bool {{\n  true\n}}\n")
            }
            _ => format!("{header} {module_name};\n\n"),
        }
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
