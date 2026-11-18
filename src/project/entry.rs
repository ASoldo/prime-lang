use std::path::{Path, PathBuf};
use std::{
    env, fs,
    io::{self, Read},
};

use super::manifest::PackageManifest;
use super::package::{self, PackageError};
use toml::Value;

#[derive(Clone, Copy)]
pub enum EntryPoint<'a> {
    Stdin,
    Path(&'a Path),
    Module(&'a str),
}

impl<'a> EntryPoint<'a> {
    pub fn from_target(target: &'a str) -> Self {
        let target = target.trim();
        if target == "-" {
            return Self::Stdin;
        }
        let path = Path::new(target);
        if target.contains(['/', '\\'])
            || path.extension().is_some_and(|ext| ext == "prime")
            || path.exists()
        {
            Self::Path(path)
        } else {
            Self::Module(target)
        }
    }
}

pub fn load_manifest_for_entry(
    entry: EntryPoint<'_>,
    project: Option<&str>,
) -> Result<Option<PackageManifest>, PackageError> {
    let cwd = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let search_path = match entry {
        EntryPoint::Path(path) => path,
        EntryPoint::Module(_) | EntryPoint::Stdin => cwd.as_path(),
    };
    let Some(manifest_path) = package::find_manifest(search_path) else {
        if project.is_some() {
            return Err(PackageError::Manifest {
                path: cwd.join("prime.toml"),
                message: "--project requires a workspace manifest".into(),
            });
        }
        return Ok(None);
    };
    // Try workspace members first for module targets
    if matches!(entry, EntryPoint::Module(_))
        || (matches!(entry, EntryPoint::Stdin) && project.is_some())
    {
        if let Ok(text) = std::fs::read_to_string(&manifest_path) {
            if let Ok(value) = toml::from_str::<Value>(&text) {
                if let Some(ws) = value.get("workspace").and_then(|v| v.as_table()) {
                    let members = ws
                        .get("members")
                        .and_then(|v| v.as_array())
                        .cloned()
                        .unwrap_or_default();
                    let root = manifest_path
                        .parent()
                        .map(|p| p.to_path_buf())
                        .unwrap_or_else(|| PathBuf::from("."));
                    let target_module = if let EntryPoint::Module(name) = entry {
                        Some(name)
                    } else {
                        None
                    };
                    for member in members {
                        if let Some(path_str) = member.as_str() {
                            let manifest_path = root.join(path_str).join("prime.toml");
                            if !manifest_path.exists() {
                                continue;
                            }
                            if let Some(project) = project {
                                if path_str != project {
                                    let package_name = fs::read_to_string(&manifest_path)
                                        .ok()
                                        .and_then(|text| toml::from_str::<Value>(&text).ok())
                                        .and_then(|value| {
                                            value
                                                .get("package")?
                                                .get("name")?
                                                .as_str()
                                                .map(str::to_owned)
                                        });
                                    if package_name.as_deref() != Some(project) {
                                        continue;
                                    }
                                }
                            }
                            match PackageManifest::load(&manifest_path) {
                                Ok(manifest) => {
                                    if let Some(target_module) = target_module {
                                        if manifest.module_path(target_module).is_some() {
                                            return Ok(Some(manifest));
                                        }
                                    } else {
                                        return Ok(Some(manifest));
                                    }
                                }
                                Err(err) => {
                                    return Err(PackageError::Manifest {
                                        path: manifest_path.clone(),
                                        message: package::manifest_error_message(err),
                                    });
                                }
                            }
                        }
                    }
                    if let (EntryPoint::Stdin, Some(project)) = (entry, project) {
                        return Err(PackageError::Manifest {
                            path: manifest_path,
                            message: format!("workspace member `{project}` not found"),
                        });
                    }
                } else if matches!(entry, EntryPoint::Stdin) && project.is_some() {
                    return Err(PackageError::Manifest {
                        path: manifest_path,
                        message: "--project selects a member of a workspace".into(),
                    });
                }
            }
        }
    }
    PackageManifest::load(&manifest_path)
        .map(Some)
        .map_err(|err| PackageError::Manifest {
            path: manifest_path.clone(),
            message: package::manifest_error_message(err),
        })
}

pub fn resolve_entry_path(
    entry: EntryPoint<'_>,
    manifest: Option<&PackageManifest>,
    project: Option<&str>,
) -> Result<PathBuf, PackageError> {
    match entry {
        EntryPoint::Stdin => Ok(PathBuf::from(STDIN_PATH)),
        EntryPoint::Path(path) => package::canonicalize(path).map_err(|error| PackageError::Io {
            path: path.to_path_buf(),
            error,
        }),
        EntryPoint::Module(name) => {
            if let Some(manifest) = manifest {
                if let Some(path) = manifest.module_path(name) {
                    return Ok(path);
                }
                let manifest_path = manifest.path.clone();
                return Err(PackageError::Manifest {
                    path: manifest_path,
                    message: format!("module `{name}` not found in manifest"),
                });
            }
            if project.is_some() {
                return Err(PackageError::Manifest {
                    path: env::current_dir()
                        .unwrap_or_else(|_| PathBuf::from("."))
                        .join("prime.toml"),
                    message: "`--project` requires a workspace manifest".into(),
                });
            }
            Ok(default_module_path(name, None))
        }
    }
}

pub(super) const STDIN_PATH: &str = "<stdin>";

/// A CLI source keeps its diagnostic identity separate from its import directory.
pub enum EntrySource {
    File(PathBuf),
    Stdin { text: String, directory: PathBuf },
}

impl EntrySource {
    pub fn resolve(
        entry: EntryPoint<'_>,
        manifest: Option<&PackageManifest>,
        project: Option<&str>,
    ) -> Result<Self, PackageError> {
        if matches!(entry, EntryPoint::Stdin) {
            let io_error = |error| PackageError::Io {
                path: STDIN_PATH.into(),
                error,
            };
            let directory = match (project, manifest) {
                (Some(_), Some(manifest)) => manifest.root().to_owned(),
                _ => env::current_dir().map_err(io_error)?,
            };
            let mut text = String::new();
            io::stdin()
                .lock()
                .read_to_string(&mut text)
                .map_err(io_error)?;
            Ok(Self::Stdin { text, directory })
        } else {
            let path = resolve_entry_path(entry, manifest, project)?;
            let canonical = path
                .canonicalize()
                .map_err(|error| PackageError::Io { path, error })?;
            Ok(Self::File(canonical))
        }
    }

    pub fn path(&self) -> &Path {
        match self {
            Self::File(path) => path,
            Self::Stdin { .. } => Path::new(STDIN_PATH),
        }
    }

    pub fn is_prime(&self) -> bool {
        matches!(self, Self::Stdin { .. })
            || self.path().extension().is_some_and(|ext| ext == "prime")
    }

    pub fn read_source(&self, path: &Path) -> io::Result<String> {
        match self {
            Self::Stdin { text, .. } if path == self.path() => Ok(text.clone()),
            _ => super::read_module_source(path),
        }
    }

    pub fn load(
        &self,
        manifest: Option<PackageManifest>,
    ) -> Result<package::Package, PackageError> {
        match self {
            Self::File(path) => package::load_resolved_package(path, manifest),
            Self::Stdin { text, directory } => {
                package::load_stdin_package(self.path(), directory, text, manifest)
            }
        }
    }
}

pub fn load_package_with_manifest(
    entry: EntryPoint<'_>,
    manifest: Option<PackageManifest>,
) -> Result<package::Package, PackageError> {
    if matches!(entry, EntryPoint::Stdin) {
        return EntrySource::resolve(entry, manifest.as_ref(), None)?.load(manifest);
    }
    let path = resolve_entry_path(entry, manifest.as_ref(), None)?;
    // Callers without a selected manifest can still discover one beside the entry.
    match manifest {
        Some(manifest) => package::load_resolved_package(&path, Some(manifest)),
        None => package::load_package(&path),
    }
}

fn default_module_path(name: &str, root: Option<&Path>) -> PathBuf {
    let mut path = PathBuf::new();
    let segments = name.split([':', '.']);
    for segment in segments {
        if !segment.is_empty() {
            path.push(segment);
        }
    }
    path.set_extension("prime");
    if let Some(root) = root {
        root.join(path)
    } else {
        path
    }
}
