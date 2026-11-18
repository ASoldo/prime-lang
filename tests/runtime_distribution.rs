#[path = "../src/runtime/distribution/mod.rs"]
mod runtime_distribution;

use runtime_distribution::{RuntimeKind, RuntimePackage};
use std::{fs, process::Command, time::SystemTime};
use tempfile::tempdir;

#[test]
fn host_package_can_move_with_its_sources_and_lockfile() {
    let tmp = tempdir().unwrap();
    fs::write(tmp.path().join("Cargo.toml"), "[workspace]\nmembers = []\n").unwrap();
    let cache = tmp.path().join("old cache");
    let host = RuntimePackage::prepare(&cache, RuntimeKind::Multimedia).unwrap();
    let source_dir = host.manifest.parent().unwrap().to_owned();
    drop(host);
    let relocated = tmp.path().join("relocated package");
    fs::rename(source_dir, &relocated).unwrap();
    fs::remove_dir_all(cache).unwrap();

    for (name, source) in [
        ("tcp.rs", include_bytes!("../src/runtime/tcp.rs").as_slice()),
        ("abi.rs", include_bytes!("../src/runtime/abi.rs").as_slice()),
        (
            "audio.rs",
            include_bytes!("../src/runtime/audio.rs").as_slice(),
        ),
        (
            "graphics.rs",
            include_bytes!("../src/runtime/graphics.rs").as_slice(),
        ),
    ] {
        assert_eq!(fs::read(relocated.join(name)).unwrap().as_slice(), source);
    }
    assert_eq!(
        fs::read(relocated.join("Cargo.lock")).unwrap(),
        include_bytes!("../src/runtime/distribution/host.Cargo.lock").as_slice(),
    );
    let metadata = Command::new("cargo")
        .args(["metadata", "--format-version=1", "--frozen", "--no-deps"])
        .current_dir(&relocated)
        .output()
        .unwrap();
    assert!(
        metadata.status.success(),
        "{}",
        String::from_utf8_lossy(&metadata.stderr)
    );
    let metadata: serde_json::Value = serde_json::from_slice(&metadata.stdout).unwrap();
    let abi = relocated.join("host.rs").canonicalize().unwrap();
    assert_eq!(
        metadata["packages"][0]["targets"][0]["src_path"],
        abi.to_str().unwrap()
    );
}

#[test]
fn warm_cache_preserves_sources_and_restores_damaged_files() {
    let tmp = tempdir().unwrap();
    let host = RuntimePackage::prepare(tmp.path(), RuntimeKind::Multimedia).unwrap();
    let dir = host.manifest.parent().unwrap();
    let abi = dir.join("abi.rs");
    let lock = dir.join("Cargo.lock");
    let expected_lock = fs::read(&lock).unwrap();
    fs::File::options()
        .write(true)
        .open(&abi)
        .unwrap()
        .set_modified(SystemTime::UNIX_EPOCH)
        .unwrap();
    fs::write(&lock, b"stale lockfile").unwrap();
    fs::remove_file(dir.join("graphics.rs")).unwrap();
    drop(host);
    let host = RuntimePackage::prepare(tmp.path(), RuntimeKind::Multimedia).unwrap();
    assert_eq!(fs::read(lock).unwrap(), expected_lock);
    assert_eq!(
        fs::metadata(abi).unwrap().modified().unwrap(),
        SystemTime::UNIX_EPOCH
    );
    assert!(host.manifest.with_file_name("graphics.rs").is_file());
}

#[test]
fn embedded_package_resolves_with_an_empty_offline_registry() {
    let tmp = tempdir().unwrap();
    let host = RuntimePackage::prepare(tmp.path(), RuntimeKind::Multimedia).unwrap();
    let embedded = RuntimePackage::prepare(tmp.path(), RuntimeKind::Embedded).unwrap();
    assert_ne!(embedded.target_dir, host.target_dir);
    assert_ne!(embedded.source_id, host.source_id);
    let metadata = Command::new("cargo")
        .args([
            "metadata",
            "--frozen",
            "--format-version=1",
            "--manifest-path",
        ])
        .arg(&embedded.manifest)
        .env("CARGO_HOME", tmp.path().join("empty-cargo-home"))
        .output()
        .unwrap();
    assert!(
        metadata.status.success(),
        "{}",
        String::from_utf8_lossy(&metadata.stderr)
    );
    let metadata: serde_json::Value = serde_json::from_slice(&metadata.stdout).unwrap();
    let packages = metadata["packages"].as_array().unwrap();
    assert_eq!(packages.len(), 1);
    assert_eq!(packages[0]["name"], "runtime-abi");
}

#[cfg(feature = "std-builtins")]
#[test]
fn relocated_cli_builds_minimal_stdin_offline_alongside_imported_media() {
    use std::io::Write;
    use std::process::Stdio;

    let tmp = tempdir().unwrap();
    let dir = tmp.path();
    let cli = dir.join("prime-lang");
    fs::copy(env!("CARGO_BIN_EXE_prime-lang"), &cli).unwrap();
    let cache = dir.join("cargo-target");
    let command = || {
        let mut cmd = Command::new(&cli);
        cmd.current_dir(dir).env("CARGO_TARGET_DIR", &cache);
        cmd
    };
    fs::write(
        dir.join("media.prime"),
        "library media; pub fn close_media() { gfx_close(); audio_stop_all(); }",
    )
    .unwrap();
    fs::write(
        dir.join("main.prime"),
        "module demo; import media::{close_media}; fn main() { close_media(); out(42); }",
    )
    .unwrap();

    // Fail the link even on development hosts if a text binary requests ALSA.
    let libs = dir.join("libs");
    fs::create_dir(&libs).unwrap();
    fs::write(
        libs.join("libasound.so"),
        "deliberately invalid linker input",
    )
    .unwrap();
    let mut minimal = command()
        .args(["build", "-", "--name", "minimal"])
        .env("CARGO_HOME", dir.join("empty-registry"))
        .env("CARGO_NET_OFFLINE", "true")
        .env("LIBRARY_PATH", libs)
        .env("PKG_CONFIG", dir.join("missing-pkg-config"))
        .stdin(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    minimal
        .stdin
        .take()
        .unwrap()
        .write_all(
            b"import core; tcp_close(); out(fs_exists(\"main.prime\")); out(\"prime_gfx_open\");",
        )
        .unwrap();
    let media = command()
        .args(["build", "main.prime", "--name", "media"])
        .output()
        .unwrap();
    for result in [minimal.wait_with_output().unwrap(), media] {
        assert!(
            result.status.success(),
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
    }
    for (name, features, expected) in [
        ("minimal", None, b"true\nprime_gfx_open\n".as_slice()),
        ("media", Some("graphics,audio"), b"42\n".as_slice()),
    ] {
        let artifacts = dir.join(".build.prime").join(name);
        let kind = RuntimeKind::for_object(&artifacts.join(format!("{name}.o"))).unwrap();
        assert_eq!(kind.features(), features);
        let run = Command::new(artifacts.join(name))
            .current_dir(dir)
            .output()
            .unwrap();
        assert!(run.status.success());
        assert_eq!(run.stdout, expected);
    }
    for kind in ["host-minimal", "host-multimedia"] {
        assert!(
            cache
                .join("prime-runtime")
                .join(kind)
                .join("build.lock")
                .is_file()
        );
    }
}

#[test]
fn a_single_media_reference_selects_only_its_runtime_feature() {
    let tmp = tempdir().unwrap();
    for (symbol, feature) in [
        ("prime_gfx_close", "graphics"),
        ("prime_audio_stop_all", "audio"),
    ] {
        let source = tmp.path().join("call.c");
        let obj = tmp.path().join("call.o");
        fs::write(
            &source,
            format!("extern void {symbol}(void); void f(void) {{ {symbol}(); }}"),
        )
        .unwrap();
        assert!(
            Command::new("gcc")
                .arg("-c")
                .arg(source)
                .arg("-o")
                .arg(&obj)
                .status()
                .unwrap()
                .success()
        );
        assert_eq!(
            RuntimeKind::for_object(&obj).unwrap().features(),
            Some(feature)
        );
    }
}
