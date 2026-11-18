use std::{fs, path::Path, process::Command};

fn build(dir: &Path, name: &str) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_prime-lang"));
    cmd.current_dir(dir)
        .args(["build", "main.prime", "--name", name]);
    cmd
}

#[test]
fn failed_host_rebuild_preserves_the_last_successful_artifacts() {
    let temp = tempfile::tempdir().unwrap();
    let cwd = temp.path();
    let entry = cwd.join("main.prime");
    let dir = cwd.join(".build.prime/demo");
    fs::write(&entry, "module demo; fn main() { out(11); }").unwrap();
    assert!(build(cwd, "demo").status().unwrap().success());
    let artifacts = ["demo.ll", "demo.o", "demo"]
        .map(|name| (dir.join(name), fs::read(dir.join(name)).unwrap()));
    fs::write(&entry, "module demo; fn main() { out(22); }").unwrap();
    let failed = build(cwd, "demo")
        .env("PRIME_LLC", cwd.join("missing-llc"))
        .output()
        .unwrap();
    assert!(!failed.status.success());
    assert!(String::from_utf8_lossy(&failed.stderr).contains("llc"));
    for (path, original) in artifacts {
        assert_eq!(fs::read(path).unwrap(), original);
    }
    assert_eq!(
        Command::new(dir.join("demo")).output().unwrap().stdout,
        b"11\n"
    );
}

#[test]
fn rejects_paths_as_artifact_names_before_loading_the_program() {
    let work = tempfile::tempdir().unwrap();
    for name in ["", ".", "..", "sub/demo", "demo/", ".prime-build.lock"] {
        let output = build(work.path(), name).output().unwrap();
        assert_eq!(output.status.code(), Some(2), "name {name:?}");
    }
    assert!(!work.path().join(".build.prime").exists());
}
