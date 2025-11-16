mod manifest;

use crate::language::{
    ast::{Module, Program},
    errors::SyntaxError,
    parser::parse_module,
};
use manifest::{ManifestError, PackageManifest};
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
    Manifest {
        path: PathBuf,
        message: String,
    },
}

pub fn load_package(entry_path: &Path) -> Result<Package, PackageError> {
    let canonical_entry = canonicalize(entry_path).map_err(|error| PackageError::Io {
        path: entry_path.to_path_buf(),
        error,
    })?;
    let manifest = match find_manifest(&canonical_entry) {
        Some(path) => Some(
            PackageManifest::load(&path).map_err(|err| PackageError::Manifest {
                path: path.clone(),
                message: manifest_error_message(err),
            })?,
        ),
        None => None,
    };
    let entry_name = manifest
        .as_ref()
        .and_then(|m| m.module_name_for_path(&canonical_entry))
        .unwrap_or_else(|| module_name_from_path(&canonical_entry));
    let mut loader = ModuleLoader::new(manifest);
    loader.load(&canonical_entry)?;

    if !loader.file_errors.is_empty() {
        return Err(PackageError::Syntax(loader.file_errors));
    }

    let modules = loader.modules;
    let program = Program {
        entry: entry_name,
        modules: modules.iter().map(|unit| unit.module.clone()).collect(),
    };

    Ok(Package { program, modules })
}

struct ModuleLoader {
    visited: HashSet<PathBuf>,
    modules: Vec<ModuleUnit>,
    file_errors: Vec<FileErrors>,
    manifest: Option<PackageManifest>,
}

impl ModuleLoader {
    fn new(manifest: Option<PackageManifest>) -> Self {
        Self {
            visited: HashSet::new(),
            modules: Vec::new(),
            file_errors: Vec::new(),
            manifest,
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
        let module_name = self
            .manifest
            .as_ref()
            .and_then(|m| m.module_name_for_path(&canonical))
            .unwrap_or_else(|| module_name_from_path(&canonical));

        match parse_module(&module_name, canonical.clone(), &source) {
            Ok(module) => {
                let imports = module.imports.clone();
                self.modules.push(ModuleUnit { module, source });

                for import in imports {
                    let resolved = self.resolve_import_path(&canonical, &import.path)?;
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

    fn resolve_import_path(&self, base: &Path, import_path: &str) -> Result<PathBuf, PackageError> {
        if let Some(manifest) = &self.manifest {
            if let Some(path) = manifest.module_path(import_path) {
                return Ok(path);
            }
        }
        Ok(resolve_import_relative(base, import_path))
    }
}

fn module_name_from_path(path: &Path) -> String {
    path.file_stem()
        .and_then(|os| os.to_str())
        .unwrap_or("module")
        .to_string()
}

fn resolve_import_relative(base: &Path, import_path: &str) -> PathBuf {
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

fn find_manifest(start: &Path) -> Option<PathBuf> {
    let mut current = if start.is_dir() {
        start.to_path_buf()
    } else {
        start
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| PathBuf::from("."))
    };
    loop {
        let candidate = current.join("prime.toml");
        if candidate.exists() {
            return Some(candidate);
        }
        if !current.pop() {
            break;
        }
    }
    None
}

fn manifest_error_message(err: ManifestError) -> String {
    match err {
        ManifestError::Io { error, .. } => error.to_string(),
        ManifestError::Parse { message, .. } => message,
        ManifestError::ModulePath { module, error, .. } => {
            format!("module `{}` path error: {}", module, error)
        }
    }
}
