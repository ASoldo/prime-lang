use serde::{Deserialize, Serialize};
use std::{
    collections::hash_map::DefaultHasher,
    fs,
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
    process::Command,
};

pub fn sanitize_name(name: &str) -> String {
    let mut out = String::new();
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
            out.push(ch);
        } else {
            out.push('-');
        }
    }
    if out.is_empty() {
        "prime-dep".into()
    } else {
        out
    }
}

pub fn slug(name: &str, repo: &str) -> String {
    let mut hasher = DefaultHasher::new();
    repo.hash(&mut hasher);
    let hash = hasher.finish();
    format!("{}-{:x}", sanitize_name(name), hash)
}

#[allow(dead_code)]
pub fn deps_cache_root() -> PathBuf {
    PathBuf::from(".prime/deps")
}

pub fn tools_root() -> PathBuf {
    PathBuf::from(".prime/tools")
}

pub fn ensure_git_checkout(
    repo: &str,
    name_hint: &str,
    rev: Option<&str>,
    root: &Path,
) -> Result<PathBuf, String> {
    fs::create_dir_all(root)
        .map_err(|err| format!("failed to create cache dir {}: {err}", root.display()))?;
    let dest = root.join(slug(name_hint, repo));
    if dest.exists() {
        update_git_checkout(&dest, rev)?;
        return Ok(dest);
    }
    let status = Command::new("git")
        .arg("clone")
        .arg("--depth")
        .arg("1")
        .arg(repo)
        .arg(&dest)
        .status()
        .map_err(|err| format!("failed to spawn git clone: {err}"))?;
    if !status.success() {
        return Err(format!("git clone failed for {repo}"));
    }
    if let Some(rev) = rev {
        checkout_rev(&dest, rev)?;
    }
    Ok(dest)
}

pub fn update_git_checkout(dest: &Path, rev: Option<&str>) -> Result<(), String> {
    let fetch = Command::new("git")
        .arg("-C")
        .arg(dest)
        .arg("fetch")
        .arg("--all")
        .arg("--tags")
        .status()
        .map_err(|err| format!("failed to spawn git fetch: {err}"))?;
    if !fetch.success() {
        return Err(format!("git fetch failed in {}", dest.display()));
    }
    if let Some(rev) = rev {
        checkout_rev(dest, rev)?;
    } else {
        let pull = Command::new("git")
            .arg("-C")
            .arg(dest)
            .arg("pull")
            .arg("--ff-only")
            .status()
            .map_err(|err| format!("failed to spawn git pull: {err}"))?;
        if !pull.success() {
            return Err(format!("git pull failed in {}", dest.display()));
        }
    }
    Ok(())
}

fn checkout_rev(dest: &Path, rev: &str) -> Result<(), String> {
    let status = Command::new("git")
        .arg("-C")
        .arg(dest)
        .args(["checkout", rev])
        .status()
        .map_err(|err| format!("failed to spawn git checkout: {err}"))?;
    if !status.success() {
        return Err(format!("git checkout {rev} failed in {}", dest.display()));
    }
    Ok(())
}

#[allow(dead_code)]
pub fn repo_head_rev(path: &Path) -> Option<String> {
    let output = Command::new("git")
        .arg("-C")
        .arg(path)
        .args(["rev-parse", "HEAD"])
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let text = String::from_utf8_lossy(&output.stdout).trim().to_string();
    if text.is_empty() { None } else { Some(text) }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct ToolRegistry {
    pub tools: Vec<ToolEntry>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolEntry {
    pub name: String,
    pub repo: String,
}

pub fn load_tools_registry() -> Result<ToolRegistry, String> {
    let path = tools_root().join("registry.toml");
    if !path.exists() {
        return Ok(ToolRegistry::default());
    }
    let text = fs::read_to_string(&path)
        .map_err(|err| format!("failed to read {}: {err}", path.display()))?;
    toml::from_str(&text).map_err(|err| format!("failed to parse {}: {err}", path.display()))
}

pub fn save_tools_registry(registry: &ToolRegistry) -> Result<(), String> {
    let path = tools_root().join("registry.toml");
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .map_err(|err| format!("failed to create {}: {err}", parent.display()))?;
    }
    let text = toml::to_string_pretty(registry)
        .map_err(|err| format!("failed to serialize registry: {err}"))?;
    fs::write(&path, text).map_err(|err| format!("failed to write {}: {err}", path.display()))
}

pub fn install_tool(repo: &str, name: Option<&str>) -> Result<PathBuf, String> {
    let name_hint = name.unwrap_or_else(|| repo.rsplit('/').next().unwrap_or("prime-tool"));
    let dest = ensure_git_checkout(repo, name_hint, None, &tools_root())?;
    let mut registry = load_tools_registry()?;
    registry.tools.retain(|t| t.name != name_hint);
    registry.tools.push(ToolEntry {
        name: name_hint.to_string(),
        repo: repo.to_string(),
    });
    save_tools_registry(&registry)?;
    Ok(dest)
}

pub fn update_tools(target: Option<&str>) -> Result<Vec<PathBuf>, String> {
    let registry = load_tools_registry()?;
    let mut updated = Vec::new();
    for entry in registry.tools {
        if target.is_some() && target != Some(entry.name.as_str()) {
            continue;
        }
        let dest = tools_root().join(slug(&entry.name, &entry.repo));
        if dest.exists() {
            update_git_checkout(&dest, None)?;
            updated.push(dest);
        }
    }
    Ok(updated)
}

pub fn uninstall_tool(name: &str) -> Result<(), String> {
    let mut registry = load_tools_registry()?;
    let mut removed = false;
    registry.tools.retain(|entry| {
        let keep = entry.name != name;
        if !keep {
            let dest = tools_root().join(slug(&entry.name, &entry.repo));
            let _ = std::fs::remove_dir_all(&dest);
            removed = true;
        }
        keep
    });
    if !removed {
        return Err(format!("tool `{name}` not found"));
    }
    save_tools_registry(&registry)?;
    Ok(())
}
