use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;

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

pub fn write_lockfile(path: &Path, lock: &Lockfile) -> Result<(), String> {
    let text = toml::to_string_pretty(lock)
        .map_err(|err| format!("failed to serialize lockfile: {err}"))?;
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .map_err(|err| format!("failed to create {}: {err}", parent.display()))?;
    }
    fs::write(path, text).map_err(|err| format!("failed to write {}: {err}", path.display()))
}
