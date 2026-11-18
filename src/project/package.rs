use crate::language::{
    ast::{ImportPath, Module, ModuleKind, Program, Visibility},
    errors::SyntaxError,
    parser::parse_module,
};
use std::{
    collections::HashSet,
    fs,
    path::{Path, PathBuf},
};

use super::manifest::{ManifestError, ModuleInfo, ModuleVisibility, PackageManifest};

#[derive(Debug, Clone)]
pub struct ModuleUnit {
    pub module: Module,
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

impl std::fmt::Display for PackageError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PackageError::Io { path, error } => write!(f, "{}: {}", path.display(), error),
            PackageError::Syntax(errors) => write!(f, "syntax errors in {} file(s)", errors.len()),
            PackageError::Manifest { path, message } => {
                write!(f, "manifest error at {}: {}", path.display(), message)
            }
        }
    }
}

impl std::error::Error for PackageError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            PackageError::Io { error, .. } => Some(error),
            _ => None,
        }
    }
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
    load_resolved_package(&canonical_entry, manifest)
}

pub fn load_resolved_package(
    entry: &Path,
    manifest: Option<PackageManifest>,
) -> Result<Package, PackageError> {
    let mut loader = ModuleLoader::new(manifest);
    loader.load(entry)?;

    if !loader.file_errors.is_empty() {
        return Err(PackageError::Syntax(loader.file_errors));
    }

    let modules = loader.modules;
    let program = Program {
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
        self.load_module(path, None)?;
        Ok(())
    }

    fn load_module(
        &mut self,
        path: &Path,
        source_library: Option<&ModuleInfo>,
    ) -> Result<(), PackageError> {
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
            Ok(mut module) => {
                if let Some(manifest) = &self.manifest {
                    module.no_std = source_library
                        .map_or_else(|| manifest.module_no_std(&module.name), |lib| lib.no_std);
                }
                let imports = module.imports.clone();
                if let Some(err) = library_main_error(&module) {
                    self.file_errors.push(FileErrors {
                        path: canonical.clone(),
                        source,
                        errors: vec![err],
                    });
                    return Ok(());
                }
                let owner = self.public_import_owner(&module)?;
                self.modules.push(ModuleUnit { module });

                for import in imports {
                    let path = import.canonical_path();
                    let name = path.strip_suffix("::prelude").unwrap_or(&path);
                    if import.visibility == Visibility::Public
                        && self
                            .manifest
                            .as_ref()
                            .is_some_and(|m| m.module_path(name).is_none())
                        && let Some(lib) = owner.as_ref().and_then(|m| m.local_library(name))
                        && lib.visibility == ModuleVisibility::Public
                    {
                        self.load_module(&lib.path, Some(lib))?;
                    } else {
                        let resolved = self.resolve_import_path(&canonical, &import.path)?;
                        self.load_module(&resolved, None)?;
                    }
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

    // Direct file mappings expose a library without adding its manifest to the
    // consumer's dependency graph. Consult its own library table for a named
    // public import missing from the consumer. Keep dependency resolution in
    // the original manifest, including its lockfile and lookup precedence.
    fn public_import_owner(
        &self,
        module: &Module,
    ) -> Result<Option<PackageManifest>, PackageError> {
        let Some(entry) = &self.manifest else {
            return Ok(None);
        };
        if entry.module_kind(&module.name) != Some(ModuleKind::Library)
            || !module.imports.iter().any(|i| {
                let path = i.canonical_path();
                i.visibility == Visibility::Public
                    && entry
                        .module_path(path.strip_suffix("::prelude").unwrap_or(&path))
                        .is_none()
            })
        {
            return Ok(None);
        }
        let Some(path) = find_manifest(&module.path).filter(|p| *p != entry.path) else {
            return Ok(None);
        };
        PackageManifest::load(&path)
            .map(Some)
            .map_err(|err| PackageError::Manifest {
                path,
                message: manifest_error_message(err),
            })
    }

    fn resolve_import_path(
        &self,
        base: &Path,
        import_path: &ImportPath,
    ) -> Result<PathBuf, PackageError> {
        let mut lookup_path = import_path.clone();
        if lookup_path
            .segments
            .last()
            .map(|s| s == "prelude")
            .unwrap_or(false)
            && lookup_path.segments.len() > 1
        {
            lookup_path.segments.pop();
        }
        if let Some(manifest) = &self.manifest {
            if let Some(path) = resolve_namespaced_import(manifest, import_path) {
                return Ok(path);
            }
            if let Some(path) = manifest.module_path(&lookup_path.to_string()) {
                return Ok(path);
            }
            for dep in manifest.dependencies() {
                let dep_name = crate::project::manifest::canonical_module_name(&dep.name);
                let dep_segments: Vec<_> = dep_name.split("::").collect();
                let import_segments: Vec<_> =
                    import_path.segments.iter().map(|s| s.as_str()).collect();
                if import_segments.starts_with(&dep_segments[..]) {
                    let rel_segments = &import_segments[dep_segments.len()..];
                    let mut candidate_root = match &dep.source {
                        crate::project::manifest::DependencySource::Path { path } => path.clone(),
                        crate::project::manifest::DependencySource::Git { cache, .. } => {
                            cache.clone()
                        }
                    };
                    for seg in rel_segments {
                        candidate_root.push(seg);
                    }
                    if candidate_root.extension().is_none() {
                        candidate_root.set_extension("prime");
                    }
                    if candidate_root.exists() {
                        return Ok(candidate_root);
                    }
                }
            }
        }
        Ok(resolve_import_relative(base, import_path))
    }
}

fn library_main_error(module: &Module) -> Option<SyntaxError> {
    if module.kind != crate::language::ast::ModuleKind::Library {
        return None;
    }
    for item in &module.items {
        if let crate::language::ast::Item::Function(func) = item {
            if func.name == "main" {
                return Some(SyntaxError::new(
                    "libraries cannot define `main` (use a `module` header for entrypoints)",
                    func.span,
                ));
            }
        }
    }
    None
}

fn module_name_from_path(path: &Path) -> String {
    path.file_stem()
        .and_then(|os| os.to_str())
        .unwrap_or("module")
        .to_string()
}

fn resolve_import_relative(base: &Path, import_path: &ImportPath) -> PathBuf {
    let mut path = if import_path.segments.is_empty() {
        PathBuf::new()
    } else {
        import_path.to_relative_path()
    };
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

fn resolve_namespaced_import(
    manifest: &PackageManifest,
    import_path: &ImportPath,
) -> Option<PathBuf> {
    if import_path.segments.is_empty() {
        return None;
    }
    if import_path.segments[0] == "deps" && import_path.segments.len() >= 2 {
        let import_name =
            crate::project::manifest::canonical_module_name(&import_path.segments[1..].join("::"));
        for dep in manifest.dependencies() {
            let dep_name = crate::project::manifest::canonical_module_name(&dep.name);
            if import_name != dep_name && !import_name.starts_with(&(dep_name.clone() + "::")) {
                continue;
            }
            if let Some(path) = manifest
                .dependency_manifest(&dep.name)
                .and_then(|dependency| dependency.module_path(&import_name))
            {
                return Some(path);
            }
        }
    }
    if import_path.segments[0] == "libs" && import_path.segments.len() >= 2 {
        let lib_name =
            crate::project::manifest::canonical_module_name(&import_path.segments[1..].join("::"));
        for entry in manifest.module_entries() {
            if entry.kind == crate::language::ast::ModuleKind::Library && entry.name == lib_name {
                return Some(entry.path.clone());
            }
        }
    }
    None
}

pub fn canonicalize(path: &Path) -> Result<PathBuf, std::io::Error> {
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

pub fn find_manifest(start: &Path) -> Option<PathBuf> {
    let absolute = std::path::absolute(start).ok()?;
    let directory = if absolute.is_dir() {
        absolute.as_path()
    } else {
        absolute.parent()?
    };
    // An editor may ask about a file whose parent directories do not exist yet.
    let existing = directory
        .ancestors()
        .find_map(|parent| parent.canonicalize().ok())?;
    existing
        .ancestors()
        .map(|parent| parent.join("prime.toml"))
        .find(|file| file.is_file())
}

pub fn manifest_error_message(err: ManifestError) -> String {
    err.to_string()
}

#[cfg(test)]
mod manifest_path_tests {
    use super::find_manifest;
    use std::fs;

    #[test]
    fn manifest_lookup_follows_the_actual_directory_of_unsaved_files() {
        let tmp = tempfile::tempdir().unwrap();
        let project = tmp.path().join("project");
        let sibling = tmp.path().join("sibling");
        fs::create_dir(&project).unwrap();
        fs::create_dir(&sibling).unwrap();
        let manifest = project.join("prime.toml");
        fs::write(&manifest, "").unwrap();
        assert_ne!(
            find_manifest(&project.join("../sibling/new.prime")),
            Some(manifest.clone())
        );
        assert_eq!(
            find_manifest(&project.join("new/src/main.prime")),
            Some(manifest.canonicalize().unwrap())
        );
    }
}
