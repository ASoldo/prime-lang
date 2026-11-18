//! Exercise the installed CLI layout and the same native artifact across file changes.
#![cfg(feature = "std-builtins")]
use std::{fs, path::Path, process::Command};
use tempfile::tempdir;

fn execute(exe: &Path, dir: &Path, args: &[&str]) -> String {
    let output = Command::new(exe)
        .args(args)
        .current_dir(dir)
        .output()
        .unwrap();
    assert!(output.status.success(), "{output:?}");
    String::from_utf8(output.stdout).unwrap()
}

#[test]
fn runtime_filesystem_uses_the_executing_process_directory() {
    let tmp = tempdir().unwrap();
    let root = tmp.path();
    let cli = root.join("prime-lang");
    fs::copy(env!("CARGO_BIN_EXE_prime-lang"), &cli).unwrap();
    fs::write(
        root.join("probe.prime"),
        r#"
module probe;
enum Result[T, E] { Err(E), Ok(T) }
fn main() {
  out(fs_exists("path"));
  out(fs_read("path"));
  let reader = spawn fs_read("path");
  match join(reader) {
    Result::Err(e) => out(e),
    Result::Ok(path) => {
      if let Ok(copy_path) = fs_read("path") { out(copy_path == path); }
      match fs_read(path) {
        Result::Err(e) => out(e),
        Result::Ok(text) if text == "skip" => out("skipped"),
        Result::Ok(text) => {
          out(text == "hello");
          match fs_write(path + ".copy", `[{text}]`) {
            Result::Err(e) => out(e),
            Result::Ok(unit) => out(unit),
          }
        },
      }
    },
  }
  let mut []uint8 bytes = [0, 127];
  let uint8 last = if fs_exists("path") { 255 } else { 128 };
  bytes.push(last);
  let string output = if fs_exists("path") { "other-bytes" } else { "bytes" };
  match fs_write_bytes(output, bytes) {
    Result::Err(e) => out(e),
    Result::Ok(_) => out("bytes written"),
  }
}
"#,
    )
    .unwrap();
    fs::write(root.join("bytes"), b"preserve until execution").unwrap();
    execute(&cli, root, &["build", "probe.prime", "--name", "probe"]);
    assert_eq!(
        fs::read(root.join("bytes")).unwrap(),
        b"preserve until execution"
    );
    let native = root.join(".build.prime/probe/probe");
    for mode in ["native", "run"] {
        let dir = root.join(mode);
        fs::create_dir(&dir).unwrap();
        fs::copy(root.join("probe.prime"), dir.join("probe.prime")).unwrap();
        let run = || {
            if mode == "native" {
                execute(&native, &dir, &[])
            } else {
                execute(&cli, &dir, &["run", "probe.prime"])
            }
        };
        let missing = fs::read_to_string(dir.join("path")).unwrap_err();
        assert_eq!(
            run(),
            format!("false\nResult::Err({missing})\n{missing}\nbytes written\n")
        );
        assert_eq!(fs::read(dir.join("bytes")).unwrap(), [0, 127, 128]);
        fs::write(dir.join("path"), "document").unwrap();
        fs::write(dir.join("document"), "hello").unwrap();
        assert_eq!(
            run(),
            "true\nResult::Ok(document)\ntrue\ntrue\n()\nbytes written\n"
        );
        assert_eq!(fs::read(dir.join("other-bytes")).unwrap(), [0, 127, 255]);
        assert_eq!(fs::read(dir.join("document.copy")).unwrap(), b"[hello]");
        fs::write(dir.join("document"), [0xff]).unwrap();
        let invalid = fs::read_to_string(dir.join("document")).unwrap_err();
        assert_eq!(
            run(),
            format!("true\nResult::Ok(document)\ntrue\n{invalid}\nbytes written\n")
        );
        fs::write(dir.join("document"), "revised").unwrap();
        fs::remove_file(dir.join("document.copy")).unwrap();
        fs::create_dir(dir.join("document.copy")).unwrap();
        let error = fs::write(dir.join("document.copy"), "").unwrap_err();
        assert_eq!(
            run(),
            format!("true\nResult::Ok(document)\ntrue\nfalse\n{error}\nbytes written\n")
        );
    }
}

#[test]
fn no_std_rejects_filesystem_calls_before_emitting_artifacts() {
    let tmp = tempdir().unwrap();
    let dir = tmp.path();
    fs::write(
        dir.join("prime.toml"),
        "[package]\nname='probe'\n[module]\nname='probe'\npath='probe.prime'\nno_std=true\n",
    )
    .unwrap();
    fs::write(
        dir.join("probe.prime"),
        "module probe; fn main() { out(fs_exists(\"path\")); }",
    )
    .unwrap();
    for action in ["check", "build"] {
        let output = Command::new(env!("CARGO_BIN_EXE_prime-lang"))
            .args([action, "probe.prime"])
            .current_dir(dir)
            .output()
            .unwrap();
        assert!(!output.status.success(), "{output:?}");
        assert!(
            String::from_utf8_lossy(&output.stderr).contains("requires the host std filesystem"),
            "{output:?}"
        );
        assert!(!dir.join(".build.prime").exists());
    }
}

#[test]
fn parallel_build_evaluator_cannot_write_application_files() {
    let tmp = tempdir().unwrap();
    fs::write(
        tmp.path().join("probe.prime"),
        r#"
module probe;
enum Result[T, E] { Ok(T), Err(E) }
fn main() {
  let writer = spawn fs_write("side-effect", "unexpected");
  let _ = join(writer);
}
"#,
    )
    .unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_prime-lang"))
        .args(["build", "probe.prime"])
        .env("PRIME_BUILD_PARALLEL", "1")
        .current_dir(tmp.path())
        .output()
        .unwrap();
    assert!(!output.status.success(), "{output:?}");
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("requires runtime filesystem access"),
        "{output:?}"
    );
    assert!(!tmp.path().join("side-effect").exists());
}
