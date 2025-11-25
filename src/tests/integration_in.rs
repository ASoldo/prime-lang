use std::{
    env, fs,
    io::Write,
    path::PathBuf,
    process::{Command, Stdio},
};

fn bin_path() -> String {
    if let Ok(path) = env::var("CARGO_BIN_EXE_prime-lang") {
        return path;
    }
    let mut fallback =
        PathBuf::from(env::var("CARGO_MANIFEST_DIR").expect("manifest dir not set by cargo"));
    fallback.push("target");
    fallback.push("debug");
    fallback.push("prime-lang");
    if cfg!(windows) {
        fallback.set_extension("exe");
    }
    if fallback.exists() {
        return fallback.to_string_lossy().into_owned();
    }
    panic!(
        "binary path not set by cargo test and fallback {:?} not found",
        fallback
    );
}

fn root() -> String {
    env::var("CARGO_MANIFEST_DIR").expect("manifest dir not set by cargo")
}

#[test]
fn prime_tests_support_scripted_input() {
    let mut cmd = Command::new(bin_path());
    cmd.current_dir(root())
        .arg("test")
        .arg("tests/input_read.prime")
        .env("PRIME_TEST_INPUTS", "21|abc")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    let output = cmd.output().expect("failed to run prime-lang test");
    assert!(
        output.status.success(),
        "prime tests failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn build_mode_in_preserves_values() {
    let build_name = "integration_in_build";
    let artifact_dir = PathBuf::from(root()).join(".build.prime").join(build_name);
    let _ = fs::remove_dir_all(&artifact_dir);

    let status = Command::new(bin_path())
        .current_dir(root())
        .args([
            "build",
            "tests/build_input_demo.prime",
            "--name",
            build_name,
        ])
        .status()
        .expect("failed to run build");
    assert!(status.success(), "build failed for integration input demo");

    let binary = artifact_dir.join(build_name);
    let mut child = Command::new(&binary)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to run compiled binary");
    child
        .stdin
        .as_mut()
        .expect("child stdin missing")
        .write_all(b"16\nabc\n")
        .expect("failed to write stdin");
    let output = child
        .wait_with_output()
        .expect("failed to read binary output");
    assert!(
        output.status.success(),
        "binary exited with failure: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("age recorded: 16"),
        "missing age output:\n{stdout}"
    );
    assert!(
        stdout.contains("temp error: invalid integer input"),
        "missing temp error output:\n{stdout}"
    );
}
