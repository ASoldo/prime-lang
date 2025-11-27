use super::deps;
use super::lock::{LockedDependency, Lockfile};
use super::manifest_helpers::entries_from_value;
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
    dependencies: Vec<Dependency>,
    dependency_manifests: Vec<PackageManifest>,
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

#[derive(Debug, Clone)]
pub struct Dependency {
    pub name: String,
    pub package: Option<String>,
    pub source: DependencySource,
    pub features: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum DependencySource {
    Git { url: String, cache: PathBuf },
    Path { path: PathBuf },
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

#[derive(Deserialize)]
struct RawDependencyEntry {
    name: String,
    package: Option<String>,
    git: Option<String>,
    path: Option<String>,
    features: Option<Vec<String>>,
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
        let dependencies = parse_dependencies(&value, &root)?;
        let mut dependency_manifests = Vec::new();
        let _cache_root = root.join(".prime/deps");
        for dep in &dependencies {
            match &dep.source {
                DependencySource::Path { path } => {
                    let manifest_path = path.join("prime.toml");
                    if manifest_path.exists() {
                        if let Ok(dep_manifest) = PackageManifest::load(&manifest_path) {
                            dependency_manifests.push(dep_manifest);
                        }
                    }
                }
                DependencySource::Git { cache, .. } => {
                    let manifest_path = cache.join("prime.toml");
                    if manifest_path.exists() {
                        if let Ok(dep_manifest) = PackageManifest::load(&manifest_path) {
                            dependency_manifests.push(dep_manifest);
                        }
                    }
                }
            }
        }

        Ok(Self {
            root,
            modules,
            reverse,
            tests,
            dependencies,
            dependency_manifests,
            path: path.to_path_buf(),
        })
    }

    pub fn module_path(&self, name: &str) -> Option<PathBuf> {
        let name = canonical_module_name(name);
        if let Some(path) = self
            .modules
            .get(&name)
            .or_else(|| self.tests.get(&name))
            .map(|info| info.path.clone())
        {
            return Some(path);
        }
        for dep in &self.dependency_manifests {
            if let Some(path) = dep.module_path(&name) {
                return Some(path);
            }
        }
        let _cache_root = self.root.join(".prime/deps");
        for dep in &self.dependencies {
            let manifest_path = match &dep.source {
                DependencySource::Path { path } => path.join("prime.toml"),
                DependencySource::Git { cache, .. } => cache.join("prime.toml"),
            };
            if manifest_path.exists() {
                if let Ok(dep_manifest) = PackageManifest::load(&manifest_path) {
                    if let Some(path) = dep_manifest.module_path(&name) {
                        return Some(path);
                    }
                }
            }
        }
        None
    }

    pub fn module_kind(&self, name: &str) -> Option<ModuleKind> {
        let name = canonical_module_name(name);
        if let Some(kind) = self
            .modules
            .get(&name)
            .or_else(|| self.tests.get(&name))
            .map(|info| info.kind)
        {
            return Some(kind);
        }
        for dep in &self.dependency_manifests {
            if let Some(kind) = dep.module_kind(&name) {
                return Some(kind);
            }
        }
        None
    }

    pub fn module_name_for_path(&self, path: &Path) -> Option<String> {
        let canonical = path.canonicalize().ok()?;
        if let Some(name) = self.reverse.get(&canonical).cloned() {
            return Some(name);
        }
        for dep in &self.dependency_manifests {
            if let Some(name) = dep.module_name_for_path(path) {
                return Some(name);
            }
        }
        None
    }

    pub fn module_entries(&self) -> Vec<ModuleInfo> {
        let mut entries: Vec<_> = self
            .modules
            .values()
            .chain(self.tests.values())
            .cloned()
            .collect();
        for dep in &self.dependency_manifests {
            entries.extend(dep.module_entries());
        }
        entries
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn dependencies(&self) -> &[Dependency] {
        &self.dependencies
    }

    pub fn lock_entries(&self) -> Option<Vec<LockedDependency>> {
        let lock_path = self
            .path
            .parent()
            .unwrap_or_else(|| Path::new("."))
            .join("prime.lock");
        super::lock::load_lockfile(&lock_path).map(|lock| lock.dependencies)
    }

    pub fn lock_dependencies(&self) -> Lockfile {
        let mut locked = Vec::new();
        for dep in &self.dependencies {
            let (git, path) = match &dep.source {
                DependencySource::Git { url, .. } => (Some(url.clone()), None),
                DependencySource::Path { path } => (
                    None,
                    Some(
                        path.strip_prefix(&self.root)
                            .unwrap_or(path)
                            .to_string_lossy()
                            .to_string(),
                    ),
                ),
            };
            locked.push(LockedDependency {
                name: dep.name.clone(),
                package: dep.package.clone(),
                git,
                path,
                rev: None,
                features: if dep.features.is_empty() {
                    None
                } else {
                    Some(dep.features.clone())
                },
            });
        }
        Lockfile {
            dependencies: locked,
        }
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
    if let Some(module_value) = value.get("module") {
        let raw = parse_raw_entry(module_value.clone(), None, Some("module"))?;
        insert_entry(
            &mut modules,
            &mut reverse,
            root,
            raw,
            Some("module"),
            "module",
        )?;
    }
    for entry in entries_from_value(value.get("modules")) {
        let raw = parse_raw_entry(entry, None, None)?;
        insert_entry(&mut modules, &mut reverse, root, raw, None, "module")?;
    }
    for entry in entries_from_value(value.get("libraries")) {
        let raw = parse_raw_entry(entry, None, Some("library"))?;
        insert_entry(
            &mut modules,
            &mut reverse,
            root,
            raw,
            Some("library"),
            "library",
        )?;
    }
    for entry in entries_from_value(value.get("tests")) {
        let raw = parse_raw_entry(entry, None, Some("test"))?;
        insert_entry(&mut tests, &mut reverse, root, raw, Some("test"), "test")?;
    }
    Ok((modules, reverse, tests))
}

fn parse_dependencies(value: &Value, root: &Path) -> Result<Vec<Dependency>, ManifestError> {
    let mut deps = Vec::new();
    let cache_root = root.join(".prime/deps");
    for entry in entries_from_value(value.get("dependencies")) {
        let raw: RawDependencyEntry =
            entry
                .clone()
                .try_into()
                .map_err(|error| ManifestError::InvalidModule {
                    module: entry
                        .get("name")
                        .and_then(|v| v.as_str())
                        .map(|s| s.to_string()),
                    message: error.to_string(),
                })?;
        if raw.git.is_some() && raw.path.is_some() {
            return Err(ManifestError::InvalidModule {
                module: Some(raw.name),
                message: "dependency cannot set both git and path".into(),
            });
        }
        let source = if let Some(url) = raw.git {
            let cache = cache_root.join(deps::slug(&raw.name, &url));
            DependencySource::Git { url, cache }
        } else if let Some(rel) = raw.path {
            DependencySource::Path {
                path: root.join(rel),
            }
        } else {
            return Err(ManifestError::InvalidModule {
                module: Some(raw.name),
                message: "dependency requires git or path".into(),
            });
        };
        let features = raw.features.unwrap_or_default();
        deps.push(Dependency {
            name: raw.name,
            package: raw.package,
            source,
            features,
        });
    }
    Ok(deps)
}

fn parse_raw_entry(
    value: Value,
    default_name: Option<&str>,
    default_kind: Option<&str>,
) -> Result<RawModuleEntry, ManifestError> {
    let mut value = value;
    let mut name_for_error = value
        .get("name")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
        .or_else(|| default_name.map(|s| s.to_string()));
    if let Some(table) = value.as_table_mut() {
        if let Some(name) = default_name {
            table
                .entry("name")
                .or_insert_with(|| Value::String(name.to_string()));
        }
        if let Some(kind) = default_kind {
            table
                .entry("kind")
                .or_insert_with(|| Value::String(kind.to_string()));
        }
        if name_for_error.is_none() {
            name_for_error = table
                .get("name")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string());
        }
    }
    value
        .try_into()
        .map_err(|error| ManifestError::InvalidModule {
            module: name_for_error,
            message: error.to_string(),
        })
}

fn insert_entry(
    target: &mut HashMap<String, ModuleInfo>,
    reverse: &mut HashMap<PathBuf, String>,
    root: &Path,
    raw: RawModuleEntry,
    default_kind: Option<&str>,
    label: &str,
) -> Result<(), ManifestError> {
    if raw.name.contains('.') {
        return Err(ManifestError::InvalidModule {
            module: Some(raw.name.clone()),
            message: "module name cannot contain '.'; use '::' or '/'".into(),
        });
    }
    let canonical_name = canonical_module_name(&raw.name);
    if target.contains_key(&canonical_name) {
        return Err(ManifestError::InvalidModule {
            module: Some(raw.name.clone()),
            message: format!("Duplicate {label} entry"),
        });
    }
    let info = build_module_info(
        root,
        &canonical_name,
        &raw.path,
        raw.package.as_deref(),
        raw.visibility.as_deref(),
        raw.doc.as_deref(),
        raw.kind.as_deref().or(default_kind),
    )?;
    reverse.insert(info.path.clone(), canonical_name.clone());
    target.insert(canonical_name, info);
    Ok(())
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
    if name.contains('.') {
        return Err(ManifestError::InvalidModule {
            module: Some(name.to_string()),
            message: "module name cannot contain '.'; use '::' or '/'".into(),
        });
    }
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
    let normalized = name.replace("::", "/");
    for part in normalized.split('/') {
        let trimmed = part.trim_matches(':');
        if trimmed.is_empty() {
            continue;
        }
        segments.push(trimmed.to_string());
    }
    if segments.is_empty() {
        name.to_string()
    } else {
        segments.join("::")
    }
}

pub fn manifest_key_for(name: &str) -> String {
    let mut out = String::new();
    for ch in name.chars() {
        match ch {
            ':' | '.' => out.push('_'),
            ch if ch.is_ascii_alphanumeric() => out.push(ch),
            '-' | '_' => out.push(ch),
            _ => out.push('-'),
        }
    }
    if out.is_empty() { "entry".into() } else { out }
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
