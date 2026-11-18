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

#[test]
fn object_can_be_linked_externally_and_survives_failed_rebuild() {
    let temp = tempfile::tempdir().unwrap();
    let dir = temp.path();
    fs::write(
        dir.join("main.prime"),
        "fn main() { if 21 * 2 == 42 { out(`42`); } }",
    )
    .unwrap();
    let cli = |args: &[&str]| {
        let mut cmd = Command::new(env!("CARGO_BIN_EXE_prime-lang"));
        cmd.current_dir(dir).args(args);
        cmd
    };
    let args = [
        "build",
        "main.prime",
        "--emit-object",
        "app.o",
        "--no-flash",
    ];
    let build = cli(&args).output().unwrap();
    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(!dir.join(".build.prime").exists());
    let original = fs::read(dir.join("app.o")).unwrap();
    let failed = cli(&args)
        .env("PRIME_LLC", dir.join("missing"))
        .output()
        .unwrap();
    assert!(!failed.status.success());
    assert_eq!(fs::read(dir.join("app.o")).unwrap(), original);

    assert!(
        Command::new("cc")
            .current_dir(dir)
            .args(["app.o", "-o", "app"])
            .status()
            .unwrap()
            .success()
    );
    let interpreted = cli(&["run", "main.prime"]).output().unwrap();
    let native = Command::new(dir.join("app")).output().unwrap();
    assert!(interpreted.status.success() && native.status.success());
    assert_eq!(interpreted.stdout, b"42\n");
    assert_eq!(native.stdout, interpreted.stdout);
}

#[test]
fn sdk_sockets_require_the_idf_component_profile() {
    let temp = tempfile::tempdir().unwrap();
    let dir = temp.path();
    fs::write(
        dir.join("main.prime"),
        "module demo; fn main() { idf_udp_bind(0); }",
    )
    .unwrap();
    for (target, platform, no_std, success) in [
        ("xtensa-esp32-espidf", "esp-idf", false, true),
        ("xtensa-esp32-espidf", "esp-idf", true, false),
        ("xtensa-esp32-espidf", "esp32", false, false),
        ("xtensa-esp32-none-elf", "esp-idf", false, false),
        ("", "host", false, false),
    ] {
        let build = if target.is_empty() {
            String::new()
        } else {
            format!("[build]\ntarget = \"{target}\"\nplatform = \"{platform}\"")
        };
        fs::write(dir.join("prime.toml"), format!(
            "manifest_version = \"3\"\n[module]\nname = \"demo\"\npath = \"main.prime\"\nno_std = {no_std}\n{build}\n"
        )).unwrap();
        let result = Command::new(env!("CARGO_BIN_EXE_prime-lang"))
            .current_dir(dir)
            .args(["check", "main.prime", "--format", "json"])
            .output()
            .unwrap();
        let report = String::from_utf8(result.stdout).unwrap();
        assert_eq!(result.status.success(), success, "{report}");
        if success {
            let linked = Command::new(env!("CARGO_BIN_EXE_prime-lang"))
                .current_dir(dir)
                .args(["build", "main.prime", "--no-flash"])
                .output()
                .unwrap();
            assert!(!linked.status.success());
            assert!(String::from_utf8_lossy(&linked.stderr).contains("idf.py build"));
        } else {
            assert!(report.contains("ESP-IDF SDK socket component"), "{report}");
        }
    }
}
