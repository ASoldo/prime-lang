pub mod manifest;
mod package;
pub mod program;

pub use program::{
    apply_manifest_header, find_manifest, load_package, warn_manifest_drift, FileErrors, Package,
    PackageError,
};
