use crate::{
    language::{macro_expander, typecheck},
    project,
    project::manifest::{PackageManifest, canonical_module_name},
    runtime::{Interpreter, value::Value},
    tools::diagnostics::{emit_syntax_errors, emit_type_errors, report_runtime_error},
};
use std::{
    collections::HashSet,
    fmt::Write,
    fs,
    path::{Path, PathBuf},
};

pub fn run_tests(targets: &[String]) -> Result<(), String> {
    let workspace_root = manifest_or_current(None)?;

    if !targets.is_empty() {
        let mut failures = 0;
        for target in targets {
            match resolve_hint_to_path(&workspace_root, target)? {
                Some(path) => {
                    if let Err(err) = run_explicit_file(&workspace_root, &path) {
                        eprintln!("{err}");
                        failures += 1;
                    }
                }
                None => {
                    eprintln!("could not resolve target `{target}`");
                    failures += 1;
                }
            }
        }
        if failures > 0 {
            return Err(format!("{failures} test target(s) failed"));
        }
        return Ok(());
    }

    let test_files = discover_tests(&workspace_root)?;
    if test_files.is_empty() {
        return Err("No tests found (looked for *.prime under tests/)".into());
    }

    let mut failures = 0;
    for test in &test_files {
        if let Err(message) = run_test_file(workspace_root.as_path(), test) {
            failures += 1;
            eprintln!(
                "test name:{} path:{} status:failed\n{}",
                summary_name(test, workspace_root.as_path()),
                display_path(workspace_root.as_path(), test),
                message
            );
        }
    }

    if failures > 0 {
        Err(format!("test result: FAILED. {failures} test(s) failed"))
    } else {
        println!("test result: ok. {} passed", test_files.len());
        Ok(())
    }
}

fn manifest_or_current(path_hint: Option<&Path>) -> Result<PathBuf, String> {
    if let Some(path) = path_hint {
        if path.is_dir() {
            return path.canonicalize().map_err(|e| e.to_string());
        }
    }
    std::env::current_dir().map_err(|e| e.to_string())
}

fn discover_tests(root: &Path) -> Result<Vec<PathBuf>, String> {
    let mut files = Vec::new();
    let mut seen = HashSet::new();
    let search_roots = [root.join("tests"), root.to_path_buf()];
    for dir in search_roots {
        if !dir.exists() {
            continue;
        }
        for entry in fs::read_dir(&dir).map_err(|e| e.to_string())? {
            let entry = entry.map_err(|e| e.to_string())?;
            let path = entry.path();
            if path.extension().map(|ext| ext == "prime").unwrap_or(false) && is_test_header(&path)
            {
                if seen.insert(path.clone()) {
                    files.push(path);
                }
            }
        }
    }
    files.sort();
    Ok(files)
}

fn run_explicit_file(root: &Path, path: &Path) -> Result<(), String> {
    ensure_prime_file(path);
    match run_test_file(root, path) {
        Ok(_) => Ok(()),
        Err(err) => Err(err),
    }
}

fn run_test_file(root: &Path, path: &Path) -> Result<(), String> {
    ensure_prime_file(path);
    let canonical = std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
    let display = display_path(root, &canonical);
    let package = match project::load_package(&canonical) {
        Ok(package) => package,
        Err(project::PackageError::Io { path, error }) => {
            return Err(format!("io error at {}: {}", path.display(), error));
        }
        Err(project::PackageError::Syntax(errors)) => {
            emit_syntax_errors(&errors);
            return Err("syntax errors".into());
        }
        Err(project::PackageError::Manifest { path, message }) => {
            return Err(format!("manifest error at {}: {}", path.display(), message));
        }
    };

    let expanded_program = match macro_expander::expand_program(&package.program) {
        Ok(program) => program,
        Err(errors) => {
            let file_errors: Vec<project::FileErrors> = errors
                .into_iter()
                .map(|err| project::FileErrors {
                    source: fs::read_to_string(&err.path).unwrap_or_default(),
                    path: err.path,
                    errors: err.errors,
                })
                .collect();
            emit_syntax_errors(&file_errors);
            return Err("macro expansion failed".into());
        }
    };

    if let Err(errors) = typecheck::check_program(&expanded_program) {
        emit_type_errors(&errors);
        return Err("typechecking failed".into());
    }

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

    let mut interpreter = Interpreter::new(expanded_package.clone());
    if let Err(err) = interpreter.bootstrap() {
        report_runtime_error(&err);
        return Err("bootstrap failed".into());
    }

    let mut failures = Vec::new();
    let test_module = expanded_package
        .modules
        .iter()
        .find(|unit| unit.module.path == canonical)
        .map(|unit| unit.module.clone());
    let Some(module) = test_module else {
        return Err("could not locate test module".into());
    };
    let mut last_values: Option<Vec<Value>> = None;
    for item in &module.items {
        if let crate::language::ast::Item::Function(func) = item {
            if func.name == "main" {
                continue;
            }
            let args = if func.params.is_empty() {
                Vec::new()
            } else if let Some(values) = &last_values {
                if func.params.len() == values.len() {
                    values.clone()
                } else if func.params.len() == 1 {
                    vec![values.first().cloned().unwrap_or(Value::Unit)]
                } else {
                    println!(
                        "test name:{} path:{} status:skipped (needs {} args, have {})",
                        format!("{}::{}", module.name, func.name),
                        display,
                        func.params.len(),
                        values.len()
                    );
                    continue;
                }
            } else {
                println!(
                    "test name:{} path:{} status:skipped (no chained value for {} args)",
                    format!("{}::{}", module.name, func.name),
                    display,
                    func.params.len()
                );
                continue;
            };
            let qualified = format!("{}::{}", module.name, func.name);
            let arg_display = format_args_display(&args);
            match interpreter.run_function_with_args(&qualified, args.clone()) {
                Ok(values) => {
                    if !values.is_empty() {
                        last_values = Some(values.clone());
                    }
                    if let Some(Value::Bool(false)) = values.first() {
                        failures.push(format!("{qualified}({arg_display}) returned false"));
                    } else {
                        println!(
                            "test name:{} path:{} status:ok",
                            format_test_call(&qualified, &args),
                            display
                        );
                    }
                }
                Err(err) => {
                    report_runtime_error(&err);
                    failures.push(format!("{qualified}({arg_display}) failed"));
                }
            }
        }
    }

    if failures.is_empty() {
        Ok(())
    } else {
        Err(failures.join("\n"))
    }
}

fn display_path(root: &Path, path: &Path) -> String {
    let mut buf = String::new();
    if let Ok(rel) = path.strip_prefix(root) {
        write!(&mut buf, "{}", rel.display()).ok();
    } else {
        write!(&mut buf, "{}", path.display()).ok();
    }
    buf
}

fn summary_name(path: &Path, root: &Path) -> String {
    if let Some(stem) = path.file_stem().and_then(|p| p.to_str()) {
        return stem.to_string();
    }
    display_path(root, path)
}

fn ensure_prime_file(path: &Path) {
    if path.extension().map(|ext| ext == "prime").unwrap_or(false) {
        return;
    }
    let mut msg = String::from("expected a .prime file");
    if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
        msg.push_str(", found .");
        msg.push_str(ext);
    }
    panic!("{msg}");
}

fn resolve_hint_to_path(root: &Path, hint: &str) -> Result<Option<PathBuf>, String> {
    let path = PathBuf::from(hint);
    if path.exists() {
        if path.is_file() {
            return path.canonicalize().map(Some).map_err(|e| e.to_string());
        }
    }
    let looks_like_module = hint.contains("::") || hint.contains('.');
    if looks_like_module {
        let manifest_path = root.join("prime.toml");
        if manifest_path.exists() {
            let manifest = PackageManifest::load(&manifest_path)
                .map_err(|e| format!("manifest load failed: {e:?}"))?;
            let name = canonical_module_name(hint);
            if let Some(path) = manifest.module_path(&name) {
                return Ok(Some(path));
            }
        }
    }
    Ok(None)
}

fn is_test_header(path: &Path) -> bool {
    if let Ok(contents) = fs::read_to_string(path) {
        let trimmed = contents.trim_start();
        return trimmed.starts_with("test ")
            || trimmed.starts_with("test\t")
            || trimmed.starts_with("test::")
            || trimmed.starts_with("test.");
    }
    false
}

fn format_test_call(name: &str, args: &[Value]) -> String {
    if args.is_empty() {
        return format!("{name}()");
    }
    format!("{name}({})", format_args_display(args))
}

fn format_args_display(args: &[Value]) -> String {
    args.iter()
        .map(|v| format!("{v}"))
        .collect::<Vec<_>>()
        .join(", ")
}
