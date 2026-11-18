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

#[test]
fn runtime_exits_preserve_payload_and_cleanup_order() {
    let tmp = tempdir().unwrap();
    let dir = tmp.path();
    let cli = Path::new(env!("CARGO_BIN_EXE_prime-lang"));
    fs::write(
        dir.join("probe.prime"),
        r#"
module probe;
enum Result[T, E] { Err(E), Ok(T) }
struct Tracker { label: string; }
impl Drop for Tracker { fn drop(self: &mut Tracker) { out(self.label); } }

fn read() -> Result[string, string] {
  let Tracker tracker = Tracker { label: "drop read" };
  let mut string state = "defer read";
  defer { out(state); state = "changed"; };
  let string text = fs_read("input")?;
  out(state);
  defer out("read ok");
  Ok(text + "!")
}

fn probe() -> Result[string, string] {
  let Tracker tracker = Tracker { label: "drop probe" };
  defer out("defer probe");
  let Result[string, string] caught = try {
    let Tracker tracker = Tracker { label: "drop try" };
    defer out("defer try");
    let string text = read()?;
    out("try ok");
    text
  };
  out("after try");
  match caught {
    Err(message) => {
      defer out("defer arm");
      return Err("caught: " + message);
    },
    Ok(text) => out(text),
  }
  out("after match");
  Ok("done")
}
fn from_try() -> Result[string, string] {
  let Result[string, string] caught = try {
    let string first = fs_read("input")?;
    match fs_read("input") {
      Ok(text) if text == first => { return Ok("return from try: " + text); },
      _ => { return Err("unexpected arm"); },
    }
    "unreachable"
  };
  out("caught in try");
  caught
}
fn main() {
  match probe() { Err(message) => out(message), Ok(text) => out(text) }
  out(from_try());
}
"#,
    )
    .unwrap();
    execute(cli, dir, &["check", "probe.prime", "--format=json"]);
    execute(cli, dir, &["build", "probe.prime", "--name", "probe"]);
    let native = dir.join(".build.prime/probe/probe");
    let missing = fs::read_to_string(dir.join("input")).unwrap_err();
    for (input, expected) in [
        (None, format!("defer read\ndrop read\ndefer try\ndrop try\nafter try\ndefer arm\ndefer probe\ndrop probe\ncaught: {missing}\ncaught in try\nResult::Err({missing})\n")),
        (Some("hello"), "defer read\nread ok\ndefer read\ndrop read\ntry ok\ndefer try\ndrop try\nafter try\nhello!\nafter match\ndefer probe\ndrop probe\ndone\nResult::Ok(return from try: hello)\n".into()),
    ] {
        if let Some(input) = input { fs::write(dir.join("input"), input).unwrap(); }
        let interpreted = execute(cli, dir, &["run", "probe.prime"]);
        assert_eq!(interpreted, expected);
        assert_eq!(execute(&native, dir, &[]), interpreted);
    }
}

#[test]
fn aggregate_result_payloads_follow_runtime_input_in_one_executable() {
    let tmp = tempdir().unwrap();
    let dir = tmp.path();
    let cli = Path::new(env!("CARGO_BIN_EXE_prime-lang"));
    let shared = include_str!("../scripts/tests/fixtures/runtime_result/main.prime");
    let (functions, _) = shared.split_once("fn main()").unwrap();
    let source = format!(
        "{functions}{}",
        r#"
struct Pair { z: int32; a: int32; }
fn pair(ok: bool) -> Result[Pair, string] {
  if ok { Ok(Pair { z: 9, a: 3 }) } else { Err("pair") }
}
fn fields(ok: bool) -> Result[int32, string] {
  let Pair direct = pair(ok)?;
  let task = async { Pair { z: 8, a: 2 } };
  let Pair waited = await_timeout(task, 10000)?;
  let nested = async { let Result[(int32, int32), string] r = Ok((5, 4)); r };
  let Result[(int32, int32), string] result = await_timeout(nested, 10000)?;
  let (x, y) = result?;
  Ok(direct.z * 100000 + direct.a * 10000 + waited.z * 1000 + waited.a * 100 + x * 10 + y)
}
fn main() {
  let bool ok = fs_exists("input");
  let int32 n = if ok { 2 } else { 0 };
  probe(n);
  match guarded(n) { Err(e) => out(e), Ok(v) => out(v) }
  match fields(ok) { Ok(v) => out(v), Err(msg) => out(msg) }
}
"#
    );
    fs::write(dir.join("result.prime"), source).unwrap();
    execute(cli, dir, &["build", "result.prime", "--name", "result"]);
    let native = dir.join(".build.prime/result/result");
    for (input, expected) in [
        (
            false,
            "leave read\ndrop read\nleave try\nouter\ncancelled\npair\n",
        ),
        (
            true,
            "read ok\nleave read\ndrop read\ntry ok\nleave try\n42\n12\n938254\n",
        ),
    ] {
        if input {
            fs::write(dir.join("input"), "").unwrap();
        }
        let interpreted = execute(cli, dir, &["run", "result.prime"]);
        assert_eq!(interpreted, expected);
        assert_eq!(execute(&native, dir, &[]), interpreted);
    }
}

#[test]
fn fs_demo_returns_after_a_failed_write() {
    let tmp = tempdir().unwrap();
    let dir = tmp.path();
    let cli = Path::new(env!("CARGO_BIN_EXE_prime-lang"));
    let source =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("workspace/demos/fs_demo/fs_demo.prime");
    let source = source.to_str().unwrap();
    execute(cli, dir, &["check", source, "--format=json"]);
    execute(cli, dir, &["build", source, "--name", "demo"]);
    let native = dir.join(".build.prime/demo/demo");
    let subdir = dir.join("workspace/demos/fs_demo");
    let path = "workspace/demos/fs_demo/tmp_demo.txt";
    let missing = fs::write(dir.join(path), "").unwrap_err();
    for (create, expected) in [
        (
            false,
            format!("writing to {path}\nwrite failed: {missing}\n"),
        ),
        (
            true,
            format!("writing to {path}\nwrite ok\nexists? true\nread: demo file contents\n"),
        ),
    ] {
        if create {
            fs::create_dir_all(&subdir).unwrap();
        }
        let interpreted = execute(cli, dir, &["run", source]);
        assert_eq!(interpreted, expected);
        assert_eq!(execute(&native, dir, &[]), interpreted);
    }
}
