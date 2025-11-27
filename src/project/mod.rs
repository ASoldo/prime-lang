pub mod deps;
pub mod diagnostics;
pub mod entry;
pub mod lock;
pub mod manifest;
mod manifest_helpers;
mod package;
pub mod program;

pub use entry::{
    EntryPoint, load_manifest_for_entry, load_package_with_manifest, resolve_entry_path,
};
pub use package::{ModuleUnit, canonicalize};
pub use program::{
    FileErrors, Package, PackageError, apply_manifest_header_with_manifest, find_manifest,
    load_package, warn_manifest_drift,
};
