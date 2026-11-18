use super::deps;
use super::lock::{LockedDependency, Lockfile};
use super::manifest_helpers::entries_from_value;
use crate::language::ast::ModuleKind;
use serde::Deserialize;
use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};
use toml::Value;

#[derive(Debug)]
pub struct PackageManifest {
    root: PathBuf,
    modules: HashMap<String, ModuleInfo>,
    reverse: HashMap<PathBuf, String>,
    tests: HashMap<String, ModuleInfo>,
    dependencies: Vec<Dependency>,
    dependency_manifests: HashMap<String, Arc<PackageManifest>>,
    pub path: PathBuf,
    build: Option<BuildSettings>,
}

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub name: String,
    pub path: PathBuf,
    pub package: Option<String>,
    pub visibility: ModuleVisibility,
    pub doc: Option<String>,
    pub kind: ModuleKind,
    pub no_std: bool,
}

#[derive(Debug, Clone)]
pub struct BuildSettings {
    pub target: Option<String>,
    pub platform: Option<String>,
    pub toolchain: ToolchainSettings,
    pub flash: FlashSettings,
    pub runtime: RuntimeSettings,
}

#[derive(Debug, Clone, Default)]
pub struct ToolchainSettings {
    pub cc: Option<String>,
    pub ar: Option<String>,
    pub objcopy: Option<String>,
    pub ld_script: Option<String>,
    pub startup_obj: Option<String>,
    pub ld_flags: Option<String>,
    pub esptool: Option<String>,
    pub env: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone, Default)]
pub struct FlashSettings {
    pub enabled: bool,
    pub port: Option<String>,
    pub baud: Option<u32>,
    pub address: Option<String>,
}

#[derive(Debug, Clone, Default)]
pub struct RuntimeSettings {
    pub channel_slots: Option<usize>,
    pub channel_capacity: Option<usize>,
    pub task_slots: Option<usize>,
    pub recv_poll_ms: Option<u32>,
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
    Cycle {
        chain: Vec<PathBuf>,
    },
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

impl std::fmt::Display for ManifestError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ManifestError::Cycle { chain } => {
                write!(f, "dependency cycle: ")?;
                for (index, path) in chain.iter().enumerate() {
                    if index > 0 {
                        write!(f, " -> ")?;
                    }
                    write!(f, "{}", path.display())?;
                }
                Ok(())
            }
            ManifestError::Io { path, error } => write!(f, "{}: {}", path.display(), error),
            ManifestError::Parse { path, message } => write!(f, "{}: {}", path.display(), message),
            ManifestError::ModulePath {
                module,
                path,
                error,
            } => write!(
                f,
                "module `{}` path error at {}: {}",
                module,
                path.display(),
                error
            ),
            ManifestError::InvalidModule { module, message } => {
                if let Some(name) = module {
                    write!(f, "invalid module `{}`: {}", name, message)
                } else {
                    f.write_str(message)
                }
            }
        }
    }
}

impl std::error::Error for ManifestError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ManifestError::Io { error, .. } | ManifestError::ModulePath { error, .. } => {
                Some(error)
            }
            _ => None,
        }
    }
}

#[derive(Deserialize)]
struct RawModuleEntry {
    name: String,
    path: String,
    package: Option<String>,
    visibility: Option<String>,
    doc: Option<String>,
    kind: Option<String>,
    no_std: Option<bool>,
}

#[derive(Deserialize)]
struct RawDependencyEntry {
    name: String,
    package: Option<String>,
    git: Option<String>,
    path: Option<String>,
    features: Option<Vec<String>>,
}

#[derive(Deserialize, Default)]
struct RawBuildTable {
    target: Option<String>,
    platform: Option<String>,
    toolchain: Option<RawToolchainTable>,
    flash: Option<RawFlashTable>,
    runtime: Option<RawRuntimeTable>,
}

#[derive(Deserialize, Default)]
struct RawToolchainTable {
    cc: Option<String>,
    ar: Option<String>,
    objcopy: Option<String>,
    ld_script: Option<String>,
    startup_obj: Option<String>,
    ld_flags: Option<String>,
    esptool: Option<String>,
    env: Option<HashMap<String, String>>,
}

#[derive(Deserialize, Default)]
struct RawFlashTable {
    enabled: Option<bool>,
    port: Option<String>,
    baud: Option<u32>,
    address: Option<String>,
}

#[derive(Deserialize, Default)]
struct RawRuntimeTable {
    channel_slots: Option<usize>,
    channel_capacity: Option<usize>,
    task_slots: Option<usize>,
    recv_poll_ms: Option<u32>,
}

impl PackageManifest {
    pub fn load(path: &Path) -> Result<Self, ManifestError> {
        let canonical = canonical_manifest(path)?;
        Self::load_node(&canonical, &mut Vec::new(), &mut HashMap::new(), false)
    }

    /// Restore each package's direct Git pins before opening its children.
    pub fn fetch_locked(path: &Path) -> Result<Self, ManifestError> {
        let path = canonical_manifest(path)?;
        Self::load_node(&path, &mut Vec::new(), &mut HashMap::new(), true)
    }

    fn load_node(
        path: &Path,
        active: &mut Vec<PathBuf>,
        loaded: &mut HashMap<PathBuf, Arc<Self>>,
        fetch_locked: bool,
    ) -> Result<Self, ManifestError> {
        if let Some(start) = active.iter().position(|ancestor| ancestor == path) {
            let mut chain = active[start..].to_vec();
            chain.push(path.to_owned());
            return Err(ManifestError::Cycle { chain });
        }
        active.push(path.to_owned());
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

        let context = |err: ManifestError| match err {
            ManifestError::InvalidModule { .. } => ManifestError::Parse {
                path: path.to_owned(),
                message: err.to_string(),
            },
            other => other,
        };
        let parsed = parse_modules(&value, &root).map_err(context)?;
        let ParsedModules {
            modules,
            reverse,
            tests,
        } = parsed;
        let dependencies = parse_dependencies(&value, &root).map_err(context)?;
        if fetch_locked {
            super::lock::check_package_lock(path, &dependencies, active.len() == 1, true).map_err(
                |message| ManifestError::Parse {
                    path: path.with_file_name("prime.lock"),
                    message,
                },
            )?;
        }
        let build = parse_build_settings(&value);
        let mut dependency_manifests = HashMap::new();
        for dep in &dependencies {
            let dir = match &dep.source {
                DependencySource::Path { path } => path,
                DependencySource::Git { cache, .. } => cache,
            };
            let child_path = dir.join("prime.toml");
            let child_path = match canonical_manifest(&child_path) {
                Ok(path) => path,
                Err(ManifestError::Io { error, .. })
                    if error.kind() == std::io::ErrorKind::NotFound
                        && matches!(dep.source, DependencySource::Git { .. }) =>
                {
                    // An empty/absent cache is valid before add's first successful fetch.
                    // Once checked out, a Git package must provide a manifest.
                    let git_dir = dir.join(".git");
                    if !git_dir.try_exists().map_err(|error| ManifestError::Io {
                        path: git_dir,
                        error,
                    })? {
                        continue;
                    }
                    return Err(ManifestError::Io {
                        path: child_path,
                        error,
                    });
                }
                Err(err) => return Err(err),
            };
            let child = if let Some(child) = loaded.get(&child_path) {
                Arc::clone(child)
            } else {
                let child = Arc::new(Self::load_node(&child_path, active, loaded, fetch_locked)?);
                loaded.insert(child_path, Arc::clone(&child));
                child
            };
            dependency_manifests.insert(dep.name.clone(), child);
        }
        active.pop();

        Ok(Self {
            root,
            modules,
            reverse,
            tests,
            dependencies,
            dependency_manifests,
            path: path.to_path_buf(),
            build,
        })
    }

    pub fn build_settings(&self) -> Option<&BuildSettings> {
        self.build.as_ref()
    }

    pub fn dependency_manifest(&self, name: &str) -> Option<&Self> {
        self.dependency_manifests.get(name).map(Arc::as_ref)
    }

    // Visit a shared dependency only once, retaining declaration-order lookup precedence.
    pub(super) fn manifests(&self) -> impl Iterator<Item = &Self> {
        let mut pending = vec![self];
        let mut visited = HashSet::new();
        std::iter::from_fn(move || {
            while let Some(manifest) = pending.pop() {
                if visited.insert(&manifest.path) {
                    pending.extend(
                        manifest
                            .dependencies
                            .iter()
                            .rev()
                            .filter_map(|dep| manifest.dependency_manifest(&dep.name)),
                    );
                    return Some(manifest);
                }
            }
            None
        })
    }

    fn module(&self, name: &str) -> Option<&ModuleInfo> {
        let name = canonical_module_name(name);
        self.manifests()
            .find_map(|m| m.modules.get(&name).or_else(|| m.tests.get(&name)))
    }

    pub fn module_path(&self, name: &str) -> Option<PathBuf> {
        self.module(name).map(|module| module.path.clone())
    }

    pub fn module_kind(&self, name: &str) -> Option<ModuleKind> {
        self.module(name).map(|module| module.kind)
    }

    pub fn module_no_std(&self, name: &str) -> bool {
        self.module(name).is_some_and(|module| module.no_std)
    }

    pub fn module_name_for_path(&self, path: &Path) -> Option<String> {
        let canonical = path.canonicalize().ok()?;
        self.manifests()
            .find_map(|m| m.reverse.get(&canonical).cloned())
    }

    pub fn module_entries(&self) -> Vec<ModuleInfo> {
        self.manifests()
            .flat_map(|m| m.modules.values().chain(m.tests.values()).cloned())
            .collect()
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
        let previous = self.lock_entries().unwrap_or_default();
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
            let rev = match &dep.source {
                DependencySource::Git { url, cache } => {
                    super::lock::checkout_revision(cache).ok().or_else(|| {
                        previous
                            .iter()
                            .find(|entry| {
                                super::lock::dependency_key(&entry.name)
                                    == super::lock::dependency_key(&dep.name)
                                    && entry.git.as_ref() == Some(url)
                                    && entry.path.is_none()
                            })
                            .and_then(|entry| entry.rev.clone())
                    })
                }
                DependencySource::Path { .. } => None,
            };
            locked.push(LockedDependency {
                name: dep.name.clone(),
                package: dep.package.clone(),
                git,
                path,
                rev,
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

fn canonical_manifest(path: &Path) -> Result<PathBuf, ManifestError> {
    path.canonicalize().map_err(|error| ManifestError::Io {
        path: path.to_owned(),
        error,
    })
}

struct ParsedModules {
    modules: HashMap<String, ModuleInfo>,
    reverse: HashMap<PathBuf, String>,
    tests: HashMap<String, ModuleInfo>,
}

fn parse_modules(value: &Value, root: &Path) -> Result<ParsedModules, ManifestError> {
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
    Ok(ParsedModules {
        modules,
        reverse,
        tests,
    })
}

fn parse_dependencies(value: &Value, root: &Path) -> Result<Vec<Dependency>, ManifestError> {
    let mut deps = Vec::new();
    let mut names = HashSet::new();
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
        if !names.insert(super::lock::dependency_key(&raw.name)) {
            return Err(ManifestError::InvalidModule {
                module: Some(raw.name),
                message: "duplicate dependency name".into(),
            });
        }
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

fn parse_build_settings(value: &Value) -> Option<BuildSettings> {
    let raw: RawBuildTable = value
        .get("build")
        .and_then(|build| build.clone().try_into().ok())
        .unwrap_or_default();
    if raw.target.is_none()
        && raw.platform.is_none()
        && raw.toolchain.is_none()
        && raw.flash.is_none()
        && raw.runtime.is_none()
    {
        return None;
    }
    let toolchain_raw = raw.toolchain.unwrap_or_default();
    let toolchain = ToolchainSettings {
        cc: toolchain_raw.cc,
        ar: toolchain_raw.ar,
        objcopy: toolchain_raw.objcopy,
        ld_script: toolchain_raw.ld_script,
        startup_obj: toolchain_raw.startup_obj,
        ld_flags: toolchain_raw.ld_flags,
        esptool: toolchain_raw.esptool,
        env: toolchain_raw.env,
    };
    let flash_raw = raw.flash.unwrap_or_default();
    let flash = FlashSettings {
        enabled: flash_raw.enabled.unwrap_or(false),
        port: flash_raw.port,
        baud: flash_raw.baud,
        address: flash_raw.address,
    };
    let runtime_raw = raw.runtime.unwrap_or_default();
    let runtime = RuntimeSettings {
        channel_slots: runtime_raw.channel_slots,
        channel_capacity: runtime_raw.channel_capacity,
        task_slots: runtime_raw.task_slots,
        recv_poll_ms: runtime_raw.recv_poll_ms,
    };
    Some(BuildSettings {
        target: raw.target,
        platform: raw.platform,
        toolchain,
        flash,
        runtime,
    })
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
        raw.no_std,
    )?;
    reverse.insert(info.path.clone(), canonical_name.clone());
    target.insert(canonical_name, info);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn build_module_info(
    root: &Path,
    name: &str,
    rel_path: &str,
    package: Option<&str>,
    visibility: Option<&str>,
    doc: Option<&str>,
    kind: Option<&str>,
    no_std: Option<bool>,
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
        no_std: no_std.unwrap_or(false),
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
