use std::{
    fs::{self, File},
    hash::{DefaultHasher, Hash, Hasher},
    io,
    path::{Path, PathBuf},
};

/// The CLI carries the same sources that Cargo tracks when compiling it.
/// Keep each sibling next to abi.rs so its relative module paths work.
const SOURCES: &[(&str, &[u8])] = &[
    ("tcp.rs", include_bytes!("../tcp.rs")),
    ("abi.rs", include_bytes!("../abi.rs")),
    ("audio.rs", include_bytes!("../audio.rs")),
    ("graphics.rs", include_bytes!("../graphics.rs")),
];

pub(crate) struct RuntimePackage {
    pub manifest: PathBuf,
    pub target_dir: PathBuf,
    pub source_id: String,
    // Cargo releases its target lock before the native linker opens the archive.
    // Keep builds sharing this target directory serialized through the link step.
    _build_lock: File,
}

impl RuntimePackage {
    pub fn prepare(cache: &Path, embedded: bool) -> io::Result<Self> {
        let (kind, manifest, lockfile) = if embedded {
            (
                "embedded",
                include_str!("embedded.Cargo.toml"),
                include_str!("embedded.Cargo.lock"),
            )
        } else {
            (
                "host",
                include_str!("host.Cargo.toml"),
                include_str!("host.Cargo.lock"),
            )
        };
        // A compiler flag identifies this bundle to Cargo. Dependency artifacts
        // can then stay warm across CLI updates without reusing an old ABI.
        let mut hash = DefaultHasher::new();
        (SOURCES, manifest, lockfile).hash(&mut hash);
        let source_id = format!("{:016x}", hash.finish());
        let cache = cache.join(kind);
        let package = cache.join(&source_id);
        fs::create_dir_all(&package)?;
        let build_lock = File::options()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(cache.join("build.lock"))?;
        build_lock.lock()?;
        for (name, bytes) in SOURCES.iter().copied().chain([
            ("Cargo.toml", manifest.as_bytes()),
            ("Cargo.lock", lockfile.as_bytes()),
        ]) {
            let path = package.join(name);
            match fs::read(&path) {
                Ok(existing) if existing == bytes => continue,
                Err(err) if err.kind() != io::ErrorKind::NotFound => return Err(err),
                _ => fs::write(path, bytes)?,
            }
        }
        Ok(Self {
            manifest: package.join("Cargo.toml"),
            target_dir: cache.join("target"),
            source_id,
            _build_lock: build_lock,
        })
    }
}
