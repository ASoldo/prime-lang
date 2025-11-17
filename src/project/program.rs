use std::path::Path;

use crate::language::ast::Module;

use super::manifest::PackageManifest;

pub use super::package::{FileErrors, Package, PackageError, find_manifest, load_package};

#[allow(dead_code)]
pub fn apply_manifest_header(path: &Path, module: &mut Module) {
    let Some(manifest_path) = find_manifest(path) else {
        return;
    };
    let Ok(manifest) = PackageManifest::load(&manifest_path) else {
        return;
    };
    apply_manifest_header_with_manifest(Some(&manifest), path, module);
}

pub fn apply_manifest_header_with_manifest(
    manifest: Option<&PackageManifest>,
    path: &Path,
    module: &mut Module,
) {
    let Some(manifest) = manifest else {
        return;
    };
    if module.declared_name.is_none() {
        if let Some(name) = manifest.module_name_for_path(path) {
            module.declared_name = Some(name);
        }
    }
}

pub fn warn_manifest_drift(start: &Path) {
    let Some(manifest_path) = find_manifest(start) else {
        return;
    };
    let manifest = match PackageManifest::load(&manifest_path) {
        Ok(manifest) => manifest,
        Err(err) => {
            eprintln!(
                "prime-lang warning: failed to load manifest {}: {:?}",
                manifest_path.display(),
                err
            );
            return;
        }
    };
    for entry in manifest.module_entries() {
        if !entry.path.exists() {
            eprintln!(
                "prime-lang warning: manifest module `{}` points to missing file `{}`",
                entry.name,
                entry.path.display()
            );
        }
    }
}
