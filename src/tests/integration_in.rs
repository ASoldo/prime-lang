use std::{
    env, fs,
    io::Write,
    path::PathBuf,
    process::{Command, Stdio},
    sync::OnceLock,
};
use tempfile::tempdir;

static BUILD_PRIME_LANG_BIN: OnceLock<()> = OnceLock::new();

fn ensure_prime_lang_binary() {
    BUILD_PRIME_LANG_BIN.get_or_init(|| {
        let status = Command::new("cargo")
            .current_dir(root())
            .args(["build", "--quiet", "--bin", "prime-lang"])
            .status()
            .expect("cargo build prime-lang");
        assert!(status.success(), "cargo build prime-lang failed");
    });
}

fn bin_path() -> String {
    if let Ok(path) = env::var("CARGO_BIN_EXE_prime-lang") {
        return path;
    }
    ensure_prime_lang_binary();
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

fn git_init(dir: &PathBuf) {
    let status = Command::new("git")
        .arg("-C")
        .arg(dir)
        .arg("init")
        .status()
        .expect("git init");
    assert!(status.success(), "git init failed");
    Command::new("git")
        .arg("-C")
        .arg(dir)
        .args(["config", "user.name", "Prime Tests"])
        .status()
        .expect("git config name");
    Command::new("git")
        .arg("-C")
        .arg(dir)
        .args(["config", "user.email", "tests@example.invalid"])
        .status()
        .expect("git config email");
}

fn git_commit_all(dir: &PathBuf, msg: &str) {
    let status = Command::new("git")
        .arg("-C")
        .arg(dir)
        .args(["add", "."])
        .status()
        .expect("git add");
    assert!(status.success(), "git add failed");
    let status = Command::new("git")
        .arg("-C")
        .arg(dir)
        .args(["commit", "-m", msg])
        .status()
        .expect("git commit");
    assert!(status.success(), "git commit failed");
}

#[test]
fn prime_tests_support_scripted_input() {
    let mut cmd = Command::new(bin_path());
    cmd.current_dir(root())
        .arg("test")
        .arg("workspace/tests/input_read/input_read.prime")
        .env(
            "PRIME_TEST_INPUTS",
            "21|abc|true|maybe|98.6|nope|Prime|Y|200|42|-1|500|70000|1000000|128|3.14|badf",
        )
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
#[ignore = "input handling flakiness on CI; skip until stabilized"]
fn build_mode_in_preserves_values() {
    let build_name = "integration_in_build";
    let artifact_dir = PathBuf::from(root()).join(".build.prime").join(build_name);
    let _ = fs::remove_dir_all(&artifact_dir);

    let status = Command::new(bin_path())
        .current_dir(root())
        .args([
            "build",
            "workspace/tests/build_input_demo/build_input_demo.prime",
            "--name",
            build_name,
        ])
        .env_remove("PRIME_TEST_INPUTS")
        .env_remove("PRIME_TEST_INPUTS_FILE")
        .status()
        .expect("failed to run build");
    assert!(status.success(), "build failed for integration input demo");

    let binary = artifact_dir.join(build_name);
    let mut child = Command::new(&binary)
        .env_remove("PRIME_TEST_INPUTS")
        .env_remove("PRIME_TEST_INPUTS_FILE")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to run compiled binary");
    {
        let mut stdin = child.stdin.take().expect("child stdin missing");
        stdin
            .write_all(b"16\nabc\n")
            .expect("failed to write stdin");
    }
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

#[test]
fn channel_demo_build_matches_run_output() {
    let build_name = "channel_demo_parity";
    let root_dir = PathBuf::from(root());
    let artifact_dir = root_dir.join(".build.prime").join(build_name);
    let _ = fs::remove_dir_all(&artifact_dir);

    let status = Command::new(bin_path())
        .current_dir(&root_dir)
        .args([
            "build",
            "workspace/demos/channel_demo/channel_demo.prime",
            "--name",
            build_name,
        ])
        .status()
        .expect("build channel demo");
    assert!(status.success(), "channel demo build failed");

    let binary = artifact_dir.join(build_name);
    let build_output = Command::new(&binary)
        .output()
        .expect("run compiled channel demo");
    assert!(
        build_output.status.success(),
        "compiled channel demo failed: {}",
        String::from_utf8_lossy(&build_output.stderr)
    );

    let run_output = Command::new(bin_path())
        .current_dir(&root_dir)
        .args(["run", "workspace/demos/channel_demo/channel_demo.prime"])
        .output()
        .expect("run interpreted channel demo");
    assert!(
        run_output.status.success(),
        "interpreted channel demo failed: {}",
        String::from_utf8_lossy(&run_output.stderr)
    );

    let build_stdout = String::from_utf8_lossy(&build_output.stdout)
        .trim()
        .to_string();
    let run_stdout = String::from_utf8_lossy(&run_output.stdout)
        .trim()
        .to_string();
    assert_eq!(
        build_stdout, run_stdout,
        "build/run output mismatch for channel demo"
    );
}

#[test]
fn async_recv_task_build_matches_run_output() {
    let workspace = tempdir().expect("temp workspace");
    let root_dir = workspace.path();
    let build_name = "async_channel_parity";

    // minimal package manifest with core dependency so prelude is available
    let manifest = format!(
        r#"
manifest_version = "3"

[package]
name = "async_bin"
version = "0.1.0"

[dependencies]
core = {{ name = "core", path = "{}/workspace/core" }}

[module]
name = "async_bin::main"
path = "main.prime"
visibility = "pub"
"#,
        root().replace('\\', "\\\\")
    );
    fs::write(root_dir.join("prime.toml"), manifest).expect("write manifest");

    fs::write(
        root_dir.join("main.prime"),
        r#"
module async_bin::main;

import core::types::prelude::{*};

fn main() {
  let (tx, rx) = channel[int32]();
  let task = async {
    let Option[int32] received = await recv_task(rx);
    match received {
      Some(v) => out(`got {v}`),
      None => out("closed"),
    }
  };
  let _ = send(tx, 7);
  close(tx);
  await task;
}
"#,
    )
    .expect("write program");

    let artifact_dir = root_dir.join(".build.prime").join(build_name);
    let _ = fs::remove_dir_all(&artifact_dir);

    let status = Command::new(bin_path())
        .current_dir(root_dir)
        .args(["build", "main.prime", "--name", build_name])
        .status()
        .expect("build async_bin");
    assert!(status.success(), "async_bin build failed");

    let binary = artifact_dir.join(build_name);
    let build_output = Command::new(&binary)
        .output()
        .expect("run compiled async_bin");
    assert!(
        build_output.status.success(),
        "compiled async_bin failed: {}",
        String::from_utf8_lossy(&build_output.stderr)
    );

    let run_output = Command::new(bin_path())
        .current_dir(root_dir)
        .args(["run", "main.prime"])
        .output()
        .expect("run interpreted async_bin");
    assert!(
        run_output.status.success(),
        "interpreted async_bin failed: {}",
        String::from_utf8_lossy(&run_output.stderr)
    );

    let build_stdout = String::from_utf8_lossy(&build_output.stdout)
        .trim()
        .to_string();
    let run_stdout = String::from_utf8_lossy(&run_output.stdout)
        .trim()
        .to_string();
    assert_eq!(
        build_stdout, run_stdout,
        "build/run output mismatch for async recv_task"
    );
}

#[test]
fn async_cancel_timeout_build_matches_run_output() {
    let workspace = tempdir().expect("temp workspace");
    let root_dir = workspace.path();
    let build_name = "async_cancel_parity";

    let manifest = format!(
        r#"
manifest_version = "3"

[package]
name = "async_cancel_bin"
version = "0.1.0"

[dependencies]
core = {{ name = "core", path = "{}/workspace/core" }}

[module]
name = "async_cancel_bin::main"
path = "main.prime"
visibility = "pub"
"#,
        root().replace('\\', "\\\\")
    );
    fs::write(root_dir.join("prime.toml"), manifest).expect("write manifest");

    fs::write(
        root_dir.join("main.prime"),
        r#"
module async_cancel_bin::main;

import core::types::prelude::{*};

fn main() {
  let Task[()] t1 = sleep_task(50);
  let Result[(), string] r1 = await_timeout(t1, 5);
  match r1 {
    Ok(_) => out("await_timeout ok"),
    Err(msg) => out(`await_timeout err {msg}`),
    _ => out("await_timeout other"),
  }

  let CancelToken tok = cancel_token();
  let Task[()] t2 = sleep_task(50);
  cancel(tok);
  let Result[(), string] r2 = await_cancel(t2, tok);
  match r2 {
    Ok(_) => out("await_cancel ok"),
    Err(msg) => out(`await_cancel err {msg}`),
    _ => out("await_cancel other"),
  }

  let CancelToken tok2 = cancel_token();
  let Task[()] t3 = sleep_task(50);
  cancel(tok2);
  let Result[(), string] r3 = await_cancel_timeout(t3, tok2, 50);
  match r3 {
    Ok(_) => out("await_cancel_timeout ok"),
    Err(msg) => out(`await_cancel_timeout err {msg}`),
    _ => out("await_cancel_timeout other"),
  }

  let CancelToken tok3 = cancel_token();
  let Task[()] t4 = sleep_task(50);
  let Result[(), string] r4 = await_cancel_timeout(t4, tok3, 5);
  match r4 {
    Ok(_) => out("await_cancel_timeout2 ok"),
    Err(msg) => out(`await_cancel_timeout2 err {msg}`),
    _ => out("await_cancel_timeout2 other"),
  }
}
"#,
    )
    .expect("write program");

    let artifact_dir = root_dir.join(".build.prime").join(build_name);
    let _ = fs::remove_dir_all(&artifact_dir);

    let status = Command::new(bin_path())
        .current_dir(root_dir)
        .args(["build", "main.prime", "--name", build_name])
        .status()
        .expect("build async_cancel_bin");
    assert!(status.success(), "async_cancel_bin build failed");

    let binary = artifact_dir.join(build_name);
    let build_output = Command::new(&binary)
        .output()
        .expect("run compiled async_cancel_bin");
    assert!(
        build_output.status.success(),
        "compiled async_cancel_bin failed: {}",
        String::from_utf8_lossy(&build_output.stderr)
    );

    let run_output = Command::new(bin_path())
        .current_dir(root_dir)
        .args(["run", "main.prime"])
        .output()
        .expect("run interpreted async_cancel_bin");
    assert!(
        run_output.status.success(),
        "interpreted async_cancel_bin failed: {}",
        String::from_utf8_lossy(&run_output.stderr)
    );

    let build_stdout = String::from_utf8_lossy(&build_output.stdout)
        .trim()
        .to_string();
    let run_stdout = String::from_utf8_lossy(&run_output.stdout)
        .trim()
        .to_string();
    assert_eq!(
        build_stdout, run_stdout,
        "build/run output mismatch for await_* cancellation/timeout"
    );
}

#[test]
fn async_demo_matches_golden_output() {
    let expected = fs::read_to_string("workspace/tests/golden/async_demo_output.txt")
        .expect("expected async demo golden output");
    let output = Command::new(bin_path())
        .current_dir(root())
        .args(["run", "workspace/demos/async_demo/async_demo.prime"])
        .output()
        .expect("run async demo");
    assert!(
        output.status.success(),
        "async demo failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();
    assert_eq!(
        stdout,
        expected.trim(),
        "async demo output differed from golden"
    );
}

#[test]
fn no_std_parity_demo_runs_under_host() {
    let output = Command::new(bin_path())
        .current_dir(root())
        .args(["run", "workspace/demos/no_std_parity/main.prime"])
        .output()
        .expect("run no_std parity demo");
    assert!(
        output.status.success(),
        "no_std parity demo failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let expected_lines = [
        "no_std parity demo start",
        "defer probe start guard=0",
        "defer ran guard=1",
        "async result parity joined",
        "async try parity ok 7",
        "match some 3",
        "timeout parity join 11",
        "await_timeout parity err timeout",
        "await_cancel parity err cancelled",
        "await_cancel_timeout parity err timeout",
        "no_std parity demo done",
    ];
    let mut cursor = 0;
    for line in expected_lines {
        let Some(pos) = stdout[cursor..].find(line) else {
            panic!("missing `{line}` in no_std parity output:\n{stdout}");
        };
        cursor += pos + line.len();
    }
}

#[test]
fn git_dependency_is_loaded_via_manifest() {
    let workspace = tempdir().expect("workspace tempdir");
    let workspace_root = workspace.path().to_path_buf();

    // dependency repo
    let dep_dir = workspace_root.join("dep");
    fs::create_dir_all(&dep_dir).expect("dep dir");
    git_init(&dep_dir);
    fs::write(
        dep_dir.join("prime.toml"),
        r#"
manifest_version = "3"

[package]
name = "dep"
version = "0.1.0"

[libraries]
dep_lib = { name = "dep::lib", path = "lib.prime", visibility = "pub" }
"#,
    )
    .expect("write dep manifest");
    fs::write(
        dep_dir.join("lib.prime"),
        r#"
library dep::lib;

pub fn value() -> int32 {
  7
}
"#,
    )
    .expect("write dep lib");
    git_commit_all(&dep_dir, "init dep");

    // workspace with app member
    fs::write(
        workspace_root.join("prime.toml"),
        r#"
manifest_version = "3"

[workspace]
members = ["app"]
"#,
    )
    .expect("write workspace manifest");
    let app_dir = workspace_root.join("app");
    fs::create_dir_all(&app_dir).expect("app dir");
    fs::write(
        app_dir.join("prime.toml"),
        r#"
manifest_version = "3"

[package]
name = "app"
version = "0.1.0"

[module]
name = "app::main"
path = "main.prime"
visibility = "pub"
"#,
    )
    .expect("write app manifest");
    fs::write(
        app_dir.join("main.prime"),
        r#"
module app::main;

import dep::lib;

fn main() {
  out(7);
}
"#,
    )
    .expect("write app main");

    // add dependency via CLI
    let status = Command::new(bin_path())
        .current_dir(&app_dir)
        .args([
            "add",
            "dep::lib",
            "--dep-path",
            dep_dir.to_str().unwrap(),
            "--features",
            "featA",
        ])
        .status()
        .expect("run prime-lang add");
    assert!(status.success(), "prime-lang add failed");

    // run entry
    let output = Command::new(bin_path())
        .current_dir(&workspace_root)
        .args(["run", "app/main.prime", "--project", "app"])
        .output()
        .expect("run prime-lang run");
    assert!(
        output.status.success(),
        "run failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("7"),
        "expected dependency value in output, got:\n{stdout}"
    );
}

#[test]
fn diagnostics_mut_borrow_reports_borrower_and_help() {
    let tmp = tempdir().expect("temp dir");
    let file = tmp.path().join("borrow_diag.prime");
    std::fs::write(
        &file,
        r#"
module tests::borrow_diag;

fn main() {
  let mut int32 x = 1;
  let &mut int32 a = &mut x;
  let &mut int32 b = &mut x;
  out(a);
  out(b);
}
"#,
    )
    .expect("write borrow diag");

    let output = Command::new(bin_path())
        .current_dir(root())
        .args(["run", file.to_str().unwrap()])
        .output()
        .expect("run borrow diag");
    assert!(
        !output.status.success(),
        "borrow diag unexpectedly succeeded"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("first mutably borrowed by `a`"),
        "missing borrower detail:\n{stderr}"
    );
    assert!(
        stderr.contains("consider cloning or reordering borrows"),
        "missing borrow hint:\n{stderr}"
    );
}

#[test]
fn diagnostics_move_while_borrowed_mentions_origin() {
    let tmp = tempdir().expect("temp dir");
    let file = tmp.path().join("move_diag.prime");
    std::fs::write(
        &file,
        r#"
module tests::move_diag;

fn main() {
  let mut []int32 values = [1, 2];
  let &mut []int32 handle = &mut values;
  let []int32 moved = move values;
  out(handle.len());
  out(moved.len());
}
"#,
    )
    .expect("write move diag");

    let output = Command::new(bin_path())
        .current_dir(root())
        .args(["run", file.to_str().unwrap()])
        .output()
        .expect("run move diag");
    assert!(!output.status.success(), "move diag unexpectedly succeeded");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("cannot be moved because it is mutably borrowed"),
        "missing move/borrow message:\n{stderr}"
    );
    assert!(
        stderr.contains("borrow started at bytes"),
        "missing borrow span hint:\n{stderr}"
    );
}

#[test]
fn tool_install_and_uninstall_manages_registry() {
    let temp = tempdir().expect("tempdir for tools");
    let root = temp.path();
    // create tiny tool repo
    let tool_dir = root.join("tool");
    fs::create_dir_all(&tool_dir).expect("tool dir");
    git_init(&tool_dir);
    fs::write(tool_dir.join("README.md"), "tool").expect("write tool readme");
    git_commit_all(&tool_dir, "init tool");

    let status = Command::new(bin_path())
        .current_dir(root)
        .args([
            "install",
            "--git",
            tool_dir.to_str().unwrap(),
            "--name",
            "local-tool",
        ])
        .status()
        .expect("install tool");
    assert!(status.success(), "install failed");
    let registry_path = root.join(".prime/tools/registry.toml");
    assert!(registry_path.exists(), "registry missing");

    let uninstall = Command::new(bin_path())
        .current_dir(root)
        .args(["uninstall", "local-tool"])
        .status()
        .expect("uninstall tool");
    assert!(uninstall.success(), "uninstall failed");
    let registry = fs::read_to_string(&registry_path).expect("read registry");
    assert!(
        !registry.contains("local-tool"),
        "registry should not list local-tool after uninstall"
    );
}

#[test]
fn path_dependency_from_local_library() {
    let workspace = tempdir().expect("workspace tempdir");
    let root = workspace.path();

    // scaffold library and app projects
    let status = Command::new(bin_path())
        .current_dir(root)
        .args(["new", "libpkg", "--lib"])
        .status()
        .expect("create lib");
    assert!(status.success(), "lib create failed");
    let status = Command::new(bin_path())
        .current_dir(root)
        .args(["new", "app"])
        .status()
        .expect("create app");
    assert!(status.success(), "app create failed");

    // write workspace manifest
    fs::write(
        root.join("prime.toml"),
        r#"
manifest_version = "3"

[workspace]
members = ["app", "libpkg"]
"#,
    )
    .expect("write workspace prime.toml");

    // add dependency from app to libpkg
    let status = Command::new(bin_path())
        .current_dir(root.join("app"))
        .args(["add", "libpkg::lib", "--dep-path", "../libpkg"])
        .status()
        .expect("add dependency");
    assert!(status.success(), "add dependency failed");

    // rewrite app main to use the library
    fs::write(
        root.join("app/main.prime"),
        r#"
module app::main;

import libpkg::lib;

fn main() {
  out(example());
}
"#,
    )
    .expect("rewrite main");

    // run the app from workspace root
    let output = Command::new(bin_path())
        .current_dir(root)
        .args(["run", "app::main", "--project", "app"])
        .output()
        .expect("run app");
    assert!(
        output.status.success(),
        "run failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("0"),
        "expected library value in output, got:\n{stdout}"
    );
}

#[test]
fn esp32_blink_smoke_builds_when_enabled() {
    if env::var("PRIME_EMBEDDED_SMOKE").is_err() {
        eprintln!("skipping esp32 blink smoke test (set PRIME_EMBEDDED_SMOKE=1 to enable)");
        return;
    }
    let status = Command::new(bin_path())
        .current_dir(root())
        .args([
            "build",
            "workspace/demos/esp32_blink/esp32_blink.prime",
            "--name",
            "esp32_blink_ci",
            "--no-flash",
        ])
        .status()
        .expect("run esp32 blink build");
    assert!(status.success(), "esp32 blink build failed");
}
