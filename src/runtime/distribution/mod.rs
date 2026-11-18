use std::{
    fs::{self, File},
    hash::{DefaultHasher, Hash, Hasher},
    io,
    path::{Path, PathBuf},
};

/// The CLI carries the same sources that Cargo tracks when compiling it.
/// Keep each sibling next to abi.rs so its relative module paths work.
const BASE_SOURCES: &[(&str, &[u8])] = &[
    ("net.rs", include_bytes!("../net.rs")),
    ("net_abi.rs", include_bytes!("../net_abi.rs")),
    ("tcp.rs", include_bytes!("../tcp.rs")),
    ("abi.rs", include_bytes!("../abi.rs")),
];

const MEDIA_SOURCES: &[(&str, &[u8])] = &[
    ("host.rs", include_bytes!("host.rs")),
    ("audio_abi.rs", include_bytes!("../audio_abi.rs")),
    ("graphics_abi.rs", include_bytes!("../graphics_abi.rs")),
    ("audio.rs", include_bytes!("../audio.rs")),
    ("graphics.rs", include_bytes!("../graphics.rs")),
];

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum RuntimeKind {
    Minimal,
    Graphics,
    Audio,
    Multimedia,
    Embedded,
}

impl RuntimeKind {
    pub fn for_object(path: &Path) -> Result<Self, String> {
        use object::{Object, ObjectSymbol};
        let bytes =
            fs::read(path).map_err(|err| format!("Cannot read {}: {err}", path.display()))?;
        let file = object::File::parse(&*bytes)
            .map_err(|err| format!("Cannot inspect {}: {err}", path.display()))?;
        let mut graphics = false;
        let mut audio = false;
        for symbol in file.symbols().filter(|s| s.is_undefined()) {
            let name = symbol.name_bytes().map_err(|err| err.to_string())?;
            // Mach-O and some COFF targets prefix C symbols with an underscore.
            let name = name.strip_prefix(b"_").unwrap_or(name);
            graphics |= name.starts_with(b"prime_gfx_");
            audio |= name.starts_with(b"prime_audio_");
        }
        Ok(match (graphics, audio) {
            (false, false) => Self::Minimal,
            (true, false) => Self::Graphics,
            (false, true) => Self::Audio,
            (true, true) => Self::Multimedia,
        })
    }

    pub fn features(self) -> Option<&'static str> {
        match self {
            Self::Graphics => Some("graphics"),
            Self::Audio => Some("audio"),
            Self::Multimedia => Some("graphics,audio"),
            Self::Minimal | Self::Embedded => None,
        }
    }
}

pub(crate) struct RuntimePackage {
    pub manifest: PathBuf,
    pub target_dir: PathBuf,
    pub source_id: String,
    // Cargo releases its target lock before the native linker opens the archive.
    // Keep builds sharing this target directory serialized through the link step.
    _build_lock: File,
}

impl RuntimePackage {
    pub fn prepare(cache: &Path, kind: RuntimeKind) -> io::Result<Self> {
        let directory = match kind {
            RuntimeKind::Minimal => "host-minimal",
            RuntimeKind::Graphics => "host-graphics",
            RuntimeKind::Audio => "host-audio",
            RuntimeKind::Multimedia => "host-multimedia",
            RuntimeKind::Embedded => "embedded",
        };
        let (manifest, lockfile, extra_sources) = match kind {
            RuntimeKind::Embedded => (
                include_str!("embedded.Cargo.toml"),
                include_str!("embedded.Cargo.lock"),
                &[][..],
            ),
            RuntimeKind::Minimal => (
                include_str!("minimal.Cargo.toml"),
                // Both packages have no external dependencies.
                include_str!("embedded.Cargo.lock"),
                &[][..],
            ),
            RuntimeKind::Graphics | RuntimeKind::Audio | RuntimeKind::Multimedia => (
                include_str!("host.Cargo.toml"),
                include_str!("host.Cargo.lock"),
                MEDIA_SOURCES,
            ),
        };
        // A compiler flag identifies this bundle to Cargo. Dependency artifacts
        // can then stay warm across CLI updates without reusing an old ABI.
        let mut hash = DefaultHasher::new();
        (BASE_SOURCES, extra_sources, manifest, lockfile).hash(&mut hash);
        let source_id = format!("{:016x}", hash.finish());
        let cache = cache.join(directory);
        let package = cache.join(&source_id);
        fs::create_dir_all(&package)?;
        let build_lock = File::options()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(cache.join("build.lock"))?;
        build_lock.lock()?;
        for (name, bytes) in BASE_SOURCES.iter().chain(extra_sources).copied().chain([
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
