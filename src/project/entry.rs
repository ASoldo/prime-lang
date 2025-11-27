use std::env;
use std::path::{Path, PathBuf};

use super::manifest::PackageManifest;
use super::package::{self, PackageError};
use toml::Value;

#[derive(Clone, Copy)]
pub enum EntryPoint<'a> {
    Path(&'a Path),
    Module(&'a str),
}

pub fn load_manifest_for_entry(
    entry: EntryPoint<'_>,
    project: Option<&str>,
) -> Result<Option<PackageManifest>, PackageError> {
    let cwd = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let search_path_buf;
    let search_path = match entry {
        EntryPoint::Path(path) => path,
        EntryPoint::Module(_) => {
            search_path_buf = cwd;
            search_path_buf.as_path()
        }
    };
    let Some(manifest_path) = package::find_manifest(search_path) else {
        return Ok(None);
    };
    // Try workspace members first for module targets
    if matches!(entry, EntryPoint::Module(_)) {
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
                                    if let Ok(member_text) = std::fs::read_to_string(&manifest_path)
                                    {
                                        if let Ok(member_value) =
                                            toml::from_str::<Value>(&member_text)
                                        {
                                            if member_value
                                                .get("package")
                                                .and_then(|pkg| pkg.get("name"))
                                                .and_then(|v| v.as_str())
                                                != Some(project)
                                            {
                                                continue;
                                            }
                                        }
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

pub fn load_package_with_manifest(
    entry: EntryPoint<'_>,
    manifest: Option<PackageManifest>,
) -> Result<package::Package, PackageError> {
    match entry {
        EntryPoint::Path(path) => package::load_package(path),
        EntryPoint::Module(name) => {
            if let Some(manifest) = manifest {
                if let Some(path) = manifest.module_path(name) {
                    return package::load_package(&path);
                }
                return Err(PackageError::Manifest {
                    path: manifest.path.clone(),
                    message: format!("module `{name}` not found in manifest"),
                });
            }
            let path = default_module_path(name, None);
            package::load_package(&path)
        }
    }
}

fn default_module_path(name: &str, root: Option<&Path>) -> PathBuf {
    let mut path = PathBuf::new();
    let segments = name.split(|c| c == ':' || c == '.');
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
