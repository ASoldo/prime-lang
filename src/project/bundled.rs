//! The portable core ships as Prime source in the executable.
use super::package::FileErrors;
use crate::language::{
    ast::{ImportPath, Module},
    parser::parse_module,
};
use std::{
    fs, io,
    path::{Path, PathBuf},
};

pub(crate) const CORE_PATH: &str = "<bundled>/core.prime";
const CORE: &str = include_str!("../../workspace/core/core.prime");

/// Read a module by the source identity recorded in the AST or check report.
pub fn read_module_source(path: &Path) -> io::Result<String> {
    if path == Path::new(CORE_PATH) {
        Ok(CORE.to_owned())
    } else {
        fs::read_to_string(path)
    }
}

// Called only after checking manifest mappings. A broken link or an unreadable
// local override must surface its own error instead of selecting the bundle.
pub(crate) fn relative_core(directory: &Path, import: &ImportPath) -> Option<PathBuf> {
    fn missing(path: &Path) -> bool {
        fs::symlink_metadata(path).is_err_and(|err| err.kind() == io::ErrorKind::NotFound)
    }

    match import.segments.as_slice() {
        [core] if core == "core" => {}
        [core, prelude] if core == "core" && prelude == "prelude" => {
            let existing = directory.join("core/prelude.prime");
            if !missing(&existing) {
                return Some(existing);
            }
        }
        _ => return None,
    }
    let local = directory.join("core.prime");
    Some(if missing(&local) {
        PathBuf::from(CORE_PATH)
    } else {
        local
    })
}

pub(crate) fn parse_core() -> Result<Module, FileErrors> {
    let path = PathBuf::from(CORE_PATH);
    let mut module = parse_module("core", path.clone(), CORE).map_err(|err| FileErrors {
        path,
        source: CORE.to_owned(),
        errors: err.errors,
    })?;
    module.no_std = true;
    Ok(module)
}
