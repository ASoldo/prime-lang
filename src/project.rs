use crate::language::{
    ast::{Module, Program},
    errors::SyntaxError,
    parser::parse_module,
};
use std::{
    collections::HashSet,
    fs,
    path::{Path, PathBuf},
};

#[derive(Debug, Clone)]
pub struct ModuleUnit {
    pub module: Module,
    pub source: String,
}

#[derive(Debug, Clone)]
pub struct Package {
    pub program: Program,
    pub modules: Vec<ModuleUnit>,
}

#[derive(Debug)]
pub struct FileErrors {
    pub path: PathBuf,
    pub source: String,
    pub errors: Vec<SyntaxError>,
}

#[derive(Debug)]
pub enum PackageError {
    Io {
        path: PathBuf,
        error: std::io::Error,
    },
    Syntax(Vec<FileErrors>),
}

pub fn load_package(entry_path: &Path) -> Result<Package, PackageError> {
    let canonical_entry = canonicalize(entry_path).map_err(|error| PackageError::Io {
        path: entry_path.to_path_buf(),
        error,
    })?;
    let mut loader = ModuleLoader::new();
    loader.load(&canonical_entry)?;

    if !loader.file_errors.is_empty() {
        return Err(PackageError::Syntax(loader.file_errors));
    }

    let modules = loader.modules;
    let program = Program {
        entry: module_name(&canonical_entry),
        modules: modules.iter().map(|unit| unit.module.clone()).collect(),
    };

    Ok(Package { program, modules })
}

struct ModuleLoader {
    visited: HashSet<PathBuf>,
    modules: Vec<ModuleUnit>,
    file_errors: Vec<FileErrors>,
}

impl ModuleLoader {
    fn new() -> Self {
        Self {
            visited: HashSet::new(),
            modules: Vec::new(),
            file_errors: Vec::new(),
        }
    }

    fn load(&mut self, path: &Path) -> Result<(), PackageError> {
        self.load_module(path)?;
        Ok(())
    }

    fn load_module(&mut self, path: &Path) -> Result<(), PackageError> {
        let canonical = canonicalize(path).map_err(|error| PackageError::Io {
            path: path.to_path_buf(),
            error,
        })?;
        if !self.visited.insert(canonical.clone()) {
            return Ok(());
        }

        let source = fs::read_to_string(&canonical).map_err(|error| PackageError::Io {
            path: canonical.clone(),
            error,
        })?;
        let module_name = module_name(&canonical);

        match parse_module(&module_name, canonical.clone(), &source) {
            Ok(module) => {
                let imports = module.imports.clone();
                self.modules.push(ModuleUnit { module, source });

                for import in imports {
                    let resolved = resolve_import(&canonical, &import.path);
                    self.load_module(&resolved)?;
                }
            }
            Err(errs) => {
                self.file_errors.push(FileErrors {
                    path: canonical.clone(),
                    source,
                    errors: errs.errors,
                });
            }
        }

        Ok(())
    }
}

fn module_name(path: &Path) -> String {
    path.file_stem()
        .and_then(|os| os.to_str())
        .unwrap_or("module")
        .to_string()
}

fn resolve_import(base: &Path, import_path: &str) -> PathBuf {
    let mut path = PathBuf::from(import_path);
    if path.extension().is_none() {
        path.set_extension("prime");
    }
    if path.is_absolute() {
        return path;
    }
    let base_dir = base
        .parent()
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| PathBuf::from("."));
    base_dir.join(path)
}

fn canonicalize(path: &Path) -> Result<PathBuf, std::io::Error> {
    if path.exists() {
        path.canonicalize()
    } else {
        let mut buf = path.to_path_buf();
        if buf.extension().is_none() {
            buf.set_extension("prime");
        }
        buf.canonicalize()
    }
}
