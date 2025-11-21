use crate::language::ast::ModuleKind;
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
    tests: HashMap<String, ModuleInfo>,
    pub path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub name: String,
    pub path: PathBuf,
    pub package: Option<String>,
    pub visibility: ModuleVisibility,
    pub doc: Option<String>,
    pub kind: ModuleKind,
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
struct RawModuleEntry {
    name: String,
    path: String,
    package: Option<String>,
    visibility: Option<String>,
    doc: Option<String>,
    kind: Option<String>,
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

        let (modules, reverse, tests) = parse_modules(&value, &root)?;

        Ok(Self {
            root,
            modules,
            reverse,
            tests,
            path: path.to_path_buf(),
        })
    }

    pub fn module_path(&self, name: &str) -> Option<PathBuf> {
        self.modules
            .get(name)
            .or_else(|| self.tests.get(name))
            .map(|info| info.path.clone())
    }

    pub fn module_kind(&self, name: &str) -> Option<ModuleKind> {
        self.modules
            .get(name)
            .or_else(|| self.tests.get(name))
            .map(|info| info.kind)
    }

    pub fn module_name_for_path(&self, path: &Path) -> Option<String> {
        let canonical = path.canonicalize().ok()?;
        self.reverse.get(&canonical).cloned()
    }

    pub fn module_entries(&self) -> Vec<ModuleInfo> {
        self.modules
            .values()
            .chain(self.tests.values())
            .cloned()
            .collect()
    }

    pub fn root(&self) -> &Path {
        &self.root
    }
}

fn parse_modules(
    value: &Value,
    root: &Path,
) -> Result<
    (
        HashMap<String, ModuleInfo>,
        HashMap<PathBuf, String>,
        HashMap<String, ModuleInfo>,
    ),
    ManifestError,
> {
    let mut modules = HashMap::new();
    let mut reverse = HashMap::new();
    let mut tests = HashMap::new();
    if let Some(Value::Array(entries)) = value.get("libraries") {
        for item in entries {
            let raw: RawModuleEntry =
                item.clone()
                    .try_into()
                    .map_err(|error| ManifestError::InvalidModule {
                        module: None,
                        message: error.to_string(),
                    })?;
            let canonical_name = canonical_module_name(&raw.name);
            if modules.contains_key(&canonical_name) {
                return Err(ManifestError::InvalidModule {
                    module: Some(raw.name.clone()),
                    message: "Duplicate library entry".into(),
                });
            }
            let info = build_module_info(
                root,
                &canonical_name,
                &raw.path,
                raw.package.as_deref(),
                raw.visibility.as_deref(),
                raw.doc.as_deref(),
                Some("library"),
            )?;
            reverse.insert(info.path.clone(), canonical_name.clone());
            modules.insert(canonical_name, info);
        }
    }
    match value.get("modules") {
        Some(Value::Table(table)) => {
            for (name, entry) in table {
                let Some(rel_path) = entry.as_str() else {
                    return Err(ManifestError::InvalidModule {
                        module: Some(name.clone()),
                        message: "Module path must be a string".into(),
                    });
                };
                let canonical = canonical_module_name(name);
                let info = build_module_info(root, &canonical, rel_path, None, None, None, None)?;
                reverse.insert(info.path.clone(), canonical.clone());
                modules.insert(canonical, info);
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
                let canonical_name = canonical_module_name(&raw.name);
                if modules.contains_key(&canonical_name) {
                    return Err(ManifestError::InvalidModule {
                        module: Some(raw.name.clone()),
                        message: "Duplicate module entry".into(),
                    });
                }
                let info = build_module_info(
                    root,
                    &canonical_name,
                    &raw.path,
                    raw.package.as_deref(),
                    raw.visibility.as_deref(),
                    raw.doc.as_deref(),
                    raw.kind.as_deref(),
                )?;
                reverse.insert(info.path.clone(), canonical_name.clone());
                modules.insert(canonical_name, info);
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
    if let Some(Value::Array(entries)) = value.get("tests") {
        for item in entries {
            let raw: RawModuleEntry =
                item.clone()
                    .try_into()
                    .map_err(|error| ManifestError::InvalidModule {
                        module: None,
                        message: error.to_string(),
                    })?;
            let canonical_name = canonical_module_name(&raw.name);
            if tests.contains_key(&canonical_name) {
                return Err(ManifestError::InvalidModule {
                    module: Some(raw.name.clone()),
                    message: "Duplicate test entry".into(),
                });
            }
            let info = build_module_info(
                root,
                &canonical_name,
                &raw.path,
                raw.package.as_deref(),
                raw.visibility.as_deref(),
                raw.doc.as_deref(),
                Some("test"),
            )?;
            reverse.insert(info.path.clone(), canonical_name.clone());
            tests.insert(canonical_name, info);
        }
    }
    Ok((modules, reverse, tests))
}

fn build_module_info(
    root: &Path,
    name: &str,
    rel_path: &str,
    package: Option<&str>,
    visibility: Option<&str>,
    doc: Option<&str>,
    kind: Option<&str>,
) -> Result<ModuleInfo, ManifestError> {
    let canonical_name = canonical_module_name(name);
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
    let kind = parse_module_kind(kind).map_err(|message| ManifestError::InvalidModule {
        module: Some(name.to_string()),
        message,
    })?;
    Ok(ModuleInfo {
        name: canonical_name,
        path: canonical,
        package: package.map(|s| s.to_string()),
        visibility,
        doc: doc.map(|s| s.to_string()),
        kind,
    })
}

fn parse_module_kind(kind: Option<&str>) -> Result<ModuleKind, String> {
    match kind {
        None => Ok(ModuleKind::Module),
        Some("module") => Ok(ModuleKind::Module),
        Some("library") => Ok(ModuleKind::Library),
        Some("test") => Ok(ModuleKind::Test),
        Some(other) => Err(format!(
            "invalid kind `{}` (expected module|library|test)",
            other
        )),
    }
}

pub fn canonical_module_name(name: &str) -> String {
    let mut segments = Vec::new();
    for part in name.split(|c| c == ':' || c == '.') {
        if part.is_empty() {
            continue;
        }
        if part == ":" {
            continue;
        }
        segments.push(part.trim_matches(':').to_string());
    }
    if segments.is_empty() {
        name.to_string()
    } else {
        segments.join("::")
    }
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
