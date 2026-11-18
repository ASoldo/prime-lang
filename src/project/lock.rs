use super::{
    PackageError,
    manifest::{Dependency, DependencySource, PackageManifest, canonical_module_name},
};
use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::Path,
    process::Command,
};

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Lockfile {
    pub dependencies: Vec<LockedDependency>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockedDependency {
    pub name: String,
    pub package: Option<String>,
    pub git: Option<String>,
    pub path: Option<String>,
    pub rev: Option<String>,
    pub features: Option<Vec<String>>,
}

pub fn load_lockfile(path: &Path) -> Option<Lockfile> {
    let Ok(text) = fs::read_to_string(path) else {
        return None;
    };
    toml::from_str(&text).ok()
}

// Version 3 manifests use `::`; older shipped lockfiles used dotted names.
pub(super) fn dependency_key(name: &str) -> String {
    canonical_module_name(&name.replace('.', "::"))
}

pub fn require_frozen(
    manifest: Option<&PackageManifest>,
    entry: &Path,
) -> Result<(), PackageError> {
    let Some(root) = manifest else {
        return Err(PackageError::Manifest {
            path: entry.to_owned(),
            message: "--frozen requires a package manifest".into(),
        });
    };
    for package in root.manifests() {
        let path = package.path.with_file_name("prime.lock");
        let fail = |reason: String| PackageError::Manifest {
            path: path.clone(),
            message: format!("--frozen: {reason}"),
        };
        let text = match fs::read_to_string(&path) {
            Ok(text) => text,
            Err(err)
                if err.kind() == std::io::ErrorKind::NotFound
                    && package.path != root.path
                    && package.dependencies().is_empty() =>
            {
                continue;
            }
            Err(err) => return Err(fail(format!("cannot read lockfile: {err}"))),
        };
        let lock = toml::from_str(&text).map_err(|err| fail(format!("invalid lockfile: {err}")))?;
        validate_lock(package, lock).map_err(fail)?;
    }
    Ok(())
}

fn validate_lock(manifest: &PackageManifest, lock: Lockfile) -> Result<(), String> {
    let mut entries = BTreeMap::new();
    for entry in lock.dependencies {
        let key = dependency_key(&entry.name);
        if entries.insert(key.clone(), entry).is_some() {
            return Err(format!("duplicate lock entry for dependency `{key}`"));
        }
    }
    for dep in manifest.dependencies() {
        let entry = entries
            .remove(&dependency_key(&dep.name))
            .ok_or_else(|| format!("missing lock entry for dependency `{}`", dep.name))?;
        check_dependency(manifest.root(), dep, &entry)
            .map_err(|reason| format!("dependency `{}`: {reason}", dep.name))?;
    }
    if let Some((_, extra)) = entries.first_key_value() {
        return Err(format!("lock lists undeclared dependency `{}`", extra.name));
    }
    Ok(())
}

fn check_dependency(
    root: &Path,
    dep: &Dependency,
    locked: &LockedDependency,
) -> Result<(), String> {
    if dep.package != locked.package {
        return Err("package selection differs from the lockfile".into());
    }
    let features: BTreeSet<_> = dep.features.iter().collect();
    let locked_features: BTreeSet<_> = locked.features.iter().flatten().collect();
    if features != locked_features {
        return Err("features differ from the lockfile".into());
    }
    match (&dep.source, &locked.path, &locked.git) {
        (DependencySource::Path { path }, Some(locked_path), None) => {
            let resolved = root
                .join(locked_path)
                .canonicalize()
                .map_err(|err| format!("cannot resolve locked path `{locked_path}`: {err}"))?;
            if path.canonicalize().map_err(|err| err.to_string())? != resolved {
                return Err(format!(
                    "path {} differs from locked path {locked_path}",
                    path.display()
                ));
            }
        }
        (DependencySource::Git { url, cache }, None, Some(locked_url)) => {
            if !same_git_source(root, url, root, locked_url) {
                return Err(format!(
                    "Git source `{url}` differs from locked source `{locked_url}`"
                ));
            }
            let rev = locked
                .rev
                .as_deref()
                .filter(|rev| !rev.is_empty())
                .ok_or_else(|| "missing locked Git revision".to_owned())?;
            let head = checkout_revision(cache)?;
            if !head.eq_ignore_ascii_case(rev) {
                return Err(format!(
                    "checkout at {} has revision {head}, lock requires {rev}",
                    cache.display()
                ));
            }
            let origin = git_output(cache, &["config", "--get", "remote.origin.url"])?;
            if !same_git_source(root, url, cache, &origin) {
                return Err(format!(
                    "checkout origin `{origin}` differs from declared Git source `{url}`"
                ));
            }
            if !git_output(cache, &["status", "--porcelain", "--untracked-files=no"])?.is_empty() {
                return Err(format!(
                    "checkout at {} has modified tracked files",
                    cache.display()
                ));
            }
        }
        _ => {
            return Err(
                "source kind differs from the lockfile (expected exactly one path or Git source)"
                    .into(),
            );
        }
    }
    Ok(())
}

fn same_git_source(left_root: &Path, left: &str, right_root: &Path, right: &str) -> bool {
    let remote = |url: &str| url.contains(':') && !Path::new(url).is_absolute();
    if remote(left) || remote(right) {
        return left == right;
    }
    // Git records relative local clone URLs as absolute paths. An offline
    // checkout remains usable even when its original directory is gone.
    let left = left_root.join(left);
    let right = right_root.join(right);
    match (left.canonicalize(), right.canonicalize()) {
        (Ok(left), Ok(right)) => left == right,
        _ => normalized_absolute(&left) == normalized_absolute(&right),
    }
}

fn normalized_absolute(path: &Path) -> std::path::PathBuf {
    let mut result = std::path::PathBuf::new();
    for component in path.components() {
        if component == std::path::Component::ParentDir {
            result.pop();
        } else {
            result.push(component);
        }
    }
    result
}

pub(super) fn checkout_revision(cache: &Path) -> Result<String, String> {
    // Without its own .git, git -C would report an enclosing project's HEAD.
    if !cache.join(".git").exists() {
        return Err(format!("missing Git checkout at {}", cache.display()));
    }
    git_output(cache, &["rev-parse", "--verify", "HEAD"])
}

fn git_output(dir: &Path, args: &[&str]) -> Result<String, String> {
    let output = Command::new("git")
        .arg("--no-optional-locks")
        .arg("-C")
        .arg(dir)
        .args(args)
        .env_remove("GIT_DIR")
        .env_remove("GIT_WORK_TREE")
        .env_remove("GIT_INDEX_FILE")
        .env("GIT_NO_LAZY_FETCH", "1")
        .output()
        .map_err(|err| format!("cannot inspect Git checkout {}: {err}", dir.display()))?;
    if !output.status.success() {
        return Err(format!(
            "git {} failed in {}: {}",
            args.join(" "),
            dir.display(),
            String::from_utf8_lossy(&output.stderr).trim()
        ));
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_owned())
}

pub fn write_lockfile(path: &Path, lock: &Lockfile) -> Result<(), String> {
    let text = toml::to_string_pretty(lock)
        .map_err(|err| format!("failed to serialize lockfile: {err}"))?;
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .map_err(|err| format!("failed to create {}: {err}", parent.display()))?;
    }
    fs::write(path, text).map_err(|err| format!("failed to write {}: {err}", path.display()))
}
