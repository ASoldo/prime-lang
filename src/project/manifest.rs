use serde::Deserialize;
use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

#[derive(Debug)]
pub struct PackageManifest {
    root: PathBuf,
    modules: HashMap<String, PathBuf>,
    reverse: HashMap<PathBuf, String>,
    pub package_name: Option<String>,
    pub entry: Option<String>,
    pub path: PathBuf,
}

#[derive(Debug)]
pub enum ManifestError {
    Io { path: PathBuf, error: std::io::Error },
    Parse { path: PathBuf, message: String },
    ModulePath {
        module: String,
        path: PathBuf,
        error: std::io::Error,
    },
}

#[derive(Deserialize)]
struct RawManifest {
    package: Option<RawPackage>,
    modules: Option<HashMap<String, String>>,
}

#[derive(Deserialize)]
struct RawPackage {
    name: Option<String>,
    entry: Option<String>,
}

impl PackageManifest {
    pub fn load(path: &Path) -> Result<Self, ManifestError> {
        let content = fs::read_to_string(path).map_err(|error| ManifestError::Io {
            path: path.to_path_buf(),
            error,
        })?;
        let raw: RawManifest = toml::from_str(&content).map_err(|error| ManifestError::Parse {
            path: path.to_path_buf(),
            message: error.to_string(),
        })?;
        let root = path
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| PathBuf::from("."));

        let mut modules = HashMap::new();
        let mut reverse = HashMap::new();
        if let Some(raw_modules) = raw.modules {
            for (name, rel_path) in raw_modules {
                let resolved = root.join(&rel_path);
                let canonical = resolved
                    .canonicalize()
                    .map_err(|error| ManifestError::ModulePath {
                        module: name.clone(),
                        path: resolved.clone(),
                        error,
                    })?;
                reverse.insert(canonical.clone(), name.clone());
                modules.insert(name, canonical);
            }
        }

        let (package_name, entry) = if let Some(pkg) = raw.package {
            (pkg.name, pkg.entry)
        } else {
            (None, None)
        };

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
        self.modules.get(name).cloned()
    }

    pub fn module_name_for_path(&self, path: &Path) -> Option<String> {
        let canonical = path.canonicalize().ok()?;
        self.reverse.get(&canonical).cloned()
    }
}
