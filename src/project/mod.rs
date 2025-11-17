pub mod diagnostics;
pub mod manifest;
mod package;
pub mod program;

pub use program::{
    FileErrors, Package, PackageError, apply_manifest_header_with_manifest, find_manifest,
    load_package, warn_manifest_drift,
};
