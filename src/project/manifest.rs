use serde::Deserialize;
use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};
use toml::Value;

#[derive(Debug)]
pub struct PackageManifest {
    root: PathBuf,
    modules: HashMap<String, ModuleInfo>,
    reverse: HashMap<PathBuf, String>,
    pub package_name: Option<String>,
    pub entry: Option<String>,
    pub path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub name: String,
    pub path: PathBuf,
    pub package: Option<String>,
    pub visibility: ModuleVisibility,
    pub doc: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModuleVisibility {
    Public,
    Package,
    Private,
}

#[derive(Debug)]
pub enum ManifestError {
    Io {
        path: PathBuf,
        error: std::io::Error,
    },
    Parse {
        path: PathBuf,
        message: String,
    },
    ModulePath {
        module: String,
        path: PathBuf,
        error: std::io::Error,
    },
    InvalidModule {
        module: Option<String>,
        message: String,
    },
}

#[derive(Deserialize)]
struct RawPackage {
    name: Option<String>,
    entry: Option<String>,
    version: Option<String>,
    kind: Option<String>,
}

#[derive(Deserialize)]
struct RawModuleEntry {
    name: String,
    path: String,
    package: Option<String>,
    visibility: Option<String>,
    doc: Option<String>,
}

impl PackageManifest {
    pub fn load(path: &Path) -> Result<Self, ManifestError> {
        let content = fs::read_to_string(path).map_err(|error| ManifestError::Io {
            path: path.to_path_buf(),
            error,
        })?;
        let value: Value = toml::from_str(&content).map_err(|error| ManifestError::Parse {
            path: path.to_path_buf(),
            message: error.to_string(),
        })?;
        let root = path
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| PathBuf::from("."));

        let (modules, reverse) = parse_modules(&value, &root)?;

        let (package_name, entry) = value
            .get("package")
            .map(|pkg| pkg.clone().try_into::<RawPackage>())
            .transpose()
            .map_err(|error| ManifestError::Parse {
                path: path.to_path_buf(),
                message: error.to_string(),
            })?
            .map(|pkg| (pkg.name, pkg.entry))
            .unwrap_or((None, None));

        Ok(Self {
            root,
            modules,
            reverse,
            package_name,
            entry,
            path: path.to_path_buf(),
        })
    }

    pub fn module_path(&self, name: &str) -> Option<PathBuf> {
        self.modules.get(name).map(|info| info.path.clone())
    }

    pub fn module_name_for_path(&self, path: &Path) -> Option<String> {
        let canonical = path.canonicalize().ok()?;
        self.reverse.get(&canonical).cloned()
    }

    pub fn module_entries(&self) -> Vec<ModuleInfo> {
        self.modules.values().cloned().collect()
    }
}

fn parse_modules(
    value: &Value,
    root: &Path,
) -> Result<(HashMap<String, ModuleInfo>, HashMap<PathBuf, String>), ManifestError> {
    let mut modules = HashMap::new();
    let mut reverse = HashMap::new();
    match value.get("modules") {
        Some(Value::Table(table)) => {
            for (name, entry) in table {
                let Some(rel_path) = entry.as_str() else {
                    return Err(ManifestError::InvalidModule {
                        module: Some(name.clone()),
                        message: "Module path must be a string".into(),
                    });
                };
                let info = build_module_info(root, &name, rel_path, None, None, None)?;
                reverse.insert(info.path.clone(), name.clone());
                modules.insert(name.clone(), info);
            }
        }
        Some(Value::Array(entries)) => {
            for item in entries {
                let raw: RawModuleEntry =
                    item.clone()
                        .try_into()
                        .map_err(|error| ManifestError::InvalidModule {
                            module: None,
                            message: error.to_string(),
                        })?;
                if modules.contains_key(&raw.name) {
                    return Err(ManifestError::InvalidModule {
                        module: Some(raw.name.clone()),
                        message: "Duplicate module entry".into(),
                    });
                }
                let info = build_module_info(
                    root,
                    &raw.name,
                    &raw.path,
                    raw.package.as_deref(),
                    raw.visibility.as_deref(),
                    raw.doc.as_deref(),
                )?;
                reverse.insert(info.path.clone(), raw.name.clone());
                modules.insert(raw.name.clone(), info);
            }
        }
        Some(other) => {
            return Err(ManifestError::InvalidModule {
                module: None,
                message: format!("Unsupported module entry type: {other:?}"),
            });
        }
        None => {}
    }
    Ok((modules, reverse))
}

fn build_module_info(
    root: &Path,
    name: &str,
    rel_path: &str,
    package: Option<&str>,
    visibility: Option<&str>,
    doc: Option<&str>,
) -> Result<ModuleInfo, ManifestError> {
    let resolved = root.join(rel_path);
    let canonical = resolved
        .canonicalize()
        .map_err(|error| ManifestError::ModulePath {
            module: name.to_string(),
            path: resolved.clone(),
            error,
        })?;
    let visibility =
        parse_module_visibility(visibility).map_err(|message| ManifestError::InvalidModule {
            module: Some(name.to_string()),
            message,
        })?;
    Ok(ModuleInfo {
        name: name.to_string(),
        path: canonical,
        package: package.map(|s| s.to_string()),
        visibility,
        doc: doc.map(|s| s.to_string()),
    })
}

fn parse_module_visibility(value: Option<&str>) -> Result<ModuleVisibility, String> {
    match value {
        None => Ok(ModuleVisibility::Public),
        Some("pub") => Ok(ModuleVisibility::Public),
        Some("pub(crate)") | Some("package") => Ok(ModuleVisibility::Package),
        Some("private") => Ok(ModuleVisibility::Private),
        Some(other) => Err(format!("Unknown module visibility `{}`", other)),
    }
}
