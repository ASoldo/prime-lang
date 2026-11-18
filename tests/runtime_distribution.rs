#[path = "../src/runtime/distribution/mod.rs"]
mod runtime_distribution;

use runtime_distribution::RuntimePackage;
use std::{fs, process::Command, time::SystemTime};
use tempfile::tempdir;

#[test]
fn host_package_can_move_with_its_sources_and_lockfile() {
    let tmp = tempdir().unwrap();
    fs::write(tmp.path().join("Cargo.toml"), "[workspace]\nmembers = []\n").unwrap();
    let cache = tmp.path().join("old cache");
    let host = RuntimePackage::prepare(&cache, false).unwrap();
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
    let abi = relocated.join("abi.rs").canonicalize().unwrap();
    assert_eq!(
        metadata["packages"][0]["targets"][0]["src_path"],
        abi.to_str().unwrap()
    );
}

#[test]
fn warm_cache_preserves_sources_and_restores_damaged_files() {
    let tmp = tempdir().unwrap();
    let host = RuntimePackage::prepare(tmp.path(), false).unwrap();
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
    let host = RuntimePackage::prepare(tmp.path(), false).unwrap();
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
    let host = RuntimePackage::prepare(tmp.path(), false).unwrap();
    let embedded = RuntimePackage::prepare(tmp.path(), true).unwrap();
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
