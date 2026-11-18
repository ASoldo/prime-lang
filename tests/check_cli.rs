use serde_json::{Value, json};
use std::{
    fs,
    path::Path,
    process::{Command, Output},
};
use tempfile::tempdir;

fn check(dir: &Path, args: &[&str], status: i32) -> Output {
    let output = Command::new(env!("CARGO_BIN_EXE_prime-lang"))
        .arg("check")
        .args(args)
        .env_remove("PRIME_TARGET")
        .current_dir(dir)
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(status), "{output:?}");
    output
}

fn report(output: &Output) -> Value {
    assert!(output.stderr.is_empty(), "{output:?}");
    let value: Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(value["schema_version"], 1);
    value
}

#[test]
fn checks_an_import_graph_without_running_initializers_main_or_codegen() {
    let dir = tempdir().unwrap();
    let root = dir.path();
    fs::write(root.join("prime.toml"), "[workspace]\nmembers = ['app']\n").unwrap();
    let app = root.join("app");
    fs::create_dir(&app).unwrap();
    let manifest = app.join("prime.toml");
    fs::write(&manifest, "[package]\nname='example'\n[module]\nname='demo'\npath='entry.prime'\n[libraries]\nhelper='helper.prime'").unwrap();
    let entry = app.join("entry.prime");
    fs::write(
        &entry,
        r#"
module demo;
import helper::{identity};
enum Result[T, E] { Ok(T), Err(E) }
const INITIAL: Result[(), string] = fs_write("initializer-ran", "unexpected");
macro twice(x: int32) -> int32 { x + x }
fn main() {
  let _ = fs_write("main-ran", "unexpected");
  out(identity(~twice(21)));
}
"#,
    )
    .unwrap();
    let helper = app.join("helper.prime");
    fs::write(
        &helper,
        "library helper;\npub fn identity(x: int32) -> int32 { x }",
    )
    .unwrap();
    let output = check(
        root,
        &["demo", "--project", "example", "--format", "json"],
        0,
    );
    let result = report(&output);
    assert_eq!(result["outcome"], "ok");
    assert_eq!(result["entry_path"], json!(entry.canonicalize().unwrap()));
    assert_eq!(
        result["manifest_path"],
        json!(manifest.canonicalize().unwrap())
    );
    assert_eq!(
        result["paths"],
        json!([
            entry.canonicalize().unwrap(),
            helper.canonicalize().unwrap()
        ])
    );
    assert_eq!(result["diagnostics"], json!([]));

    let text = check(root, &["app/entry.prime"], 0);
    assert!(
        String::from_utf8(text.stdout)
            .unwrap()
            .contains("Checked app/entry.prime")
    );
    assert!(text.stderr.is_empty());
    for name in [
        "initializer-ran",
        "main-ran",
        ".build.prime",
        ".prime",
        "prime.lock",
    ] {
        assert!(
            !root.join(name).exists() && !app.join(name).exists(),
            "{name}"
        );
    }
}

#[test]
fn reports_imported_type_errors_with_utf8_byte_offsets() {
    let dir = tempdir().unwrap();
    fs::write(
        dir.path().join("entry.prime"),
        "module entry; import helper;",
    )
    .unwrap();
    let source = "library helper;\n// café ☕\nfn broken() -> int32 { absent }\nfn wrong_type() { let int32 n = true; }\n";
    let helper = dir.path().join("helper.prime");
    fs::write(&helper, source).unwrap();
    let output = check(dir.path(), &["entry.prime", "--format=json"], 1);
    let result = report(&output);
    assert_eq!(result["outcome"], "error");
    let diag = &result["diagnostics"][0];
    assert_eq!(diag["phase"], "type");
    assert_eq!(diag["path"], json!(helper.canonicalize().unwrap()));
    assert!(diag["message"].as_str().unwrap().contains("absent"));
    let start = source.find("absent").unwrap();
    assert_eq!(
        diag["span"],
        json!({"start": start, "end": start + "absent".len()})
    );
    let mismatch = &result["diagnostics"][1];
    assert_eq!(mismatch["phase"], "type");
    assert_eq!(mismatch["message"], "expected `int32`, found `bool`");
    let text = check(dir.path(), &["entry.prime", "--format=text"], 1);
    assert!(text.stdout.is_empty());
    assert!(
        String::from_utf8(text.stderr)
            .unwrap()
            .contains("type error")
    );
}

#[test]
fn syntax_macro_and_input_errors_have_distinct_diagnostics() {
    let dir = tempdir().unwrap();
    let file = dir.path().join("demo.prime");
    let source = "// 漢字\nmodule demo; §";
    fs::write(&file, source).unwrap();
    let result = report(&check(dir.path(), &["demo.prime", "--format=json"], 1));
    let diag = &result["diagnostics"][0];
    assert_eq!(diag["phase"], "syntax");
    assert_eq!(
        diag["span"],
        json!({"start": source.find('§').unwrap(), "end": source.len()})
    );

    fs::write(&file, "module demo; fn main() { ~no_such_macro(); }").unwrap();
    let result = report(&check(dir.path(), &["demo.prime", "--format=json"], 1));
    assert_eq!(result["diagnostics"][0]["phase"], "macro");
    assert!(result["diagnostics"][0]["span"].is_object());

    let missing = report(&check(dir.path(), &["no-file.prime", "--format=json"], 1));
    assert_eq!(missing["outcome"], "error");
    assert_eq!(missing["diagnostics"][0]["phase"], "io");
    assert!(
        missing["diagnostics"][0]["path"]
            .as_str()
            .unwrap()
            .ends_with("no-file.prime")
    );
    assert!(missing["diagnostics"][0]["span"].is_null());
    check(dir.path(), &[], 2);
    check(dir.path(), &["demo.prime", "--format=yaml"], 2);
}

#[test]
fn reports_are_repeatable_for_multiple_errors_and_missing_enum_variants() {
    let dir = tempdir().unwrap();
    let source = r#"module demo;
enum Color { Red, Green, Blue, White, Black, }
fn partial(c: Color) -> int32 {
  match c { Color::Red => 1, }
}
fn broken() -> int32 { unknown }
"#;
    fs::write(dir.path().join("demo.prime"), source).unwrap();
    let args = ["demo.prime", "--format=json"];
    let first = check(dir.path(), &args, 1);
    let result = report(&first);
    let diagnostics = result["diagnostics"].as_array().unwrap();
    assert_eq!(diagnostics.len(), 2, "{result}");
    assert!(
        diagnostics[0]["message"]
            .as_str()
            .unwrap()
            .contains("Green, Blue, White, Black")
    );
    for _ in 0..3 {
        assert_eq!(first.stdout, check(dir.path(), &args, 1).stdout);
    }
}

#[test]
fn frozen_check_requires_a_readable_lock_and_preserves_it() {
    let dir = tempdir().unwrap();
    let root = dir.path();
    fs::write(
        root.join("prime.toml"),
        "[module]\nname='demo'\npath='demo.prime'",
    )
    .unwrap();
    fs::write(root.join("demo.prime"), "module demo;").unwrap();
    let args = ["demo", "--format=json", "--frozen"];
    let missing = report(&check(root, &args, 1));
    assert_eq!(missing["diagnostics"][0]["phase"], "manifest");
    let lock = root.join("prime.lock");
    assert!(!lock.exists());
    let contents = "dependencies = []\n# sentinel\n";
    fs::write(&lock, contents).unwrap();
    assert_eq!(report(&check(root, &args, 0))["outcome"], "ok");
    assert_eq!(fs::read_to_string(lock).unwrap(), contents);
}

#[test]
fn named_entry_uses_the_selecting_manifest_even_when_its_file_lives_elsewhere() {
    let dir = tempdir().unwrap();
    let root = dir.path();
    let app = root.join("app");
    let external = root.join("external");
    fs::create_dir(&app).unwrap();
    fs::create_dir(&external).unwrap();
    fs::write(root.join("prime.toml"), "[workspace]\nmembers=['app']").unwrap();
    fs::write(app.join("prime.toml"), "[package]\nname='app'\n[module]\nname='demo'\npath='../external/demo.prime'\n[libraries]\nhelper='actual.prime'").unwrap();
    fs::write(app.join("prime.lock"), "dependencies=[]").unwrap();
    fs::write(
        app.join("actual.prime"),
        "library helper; pub fn read() -> int32 { 42 }",
    )
    .unwrap();
    fs::write(external.join("prime.toml"), "[broken").unwrap();
    fs::write(
        external.join("demo.prime"),
        "module demo; import helper::{read}; fn main() { out(read()); }",
    )
    .unwrap();

    let checked = report(&check(
        root,
        &["demo", "-p", "app", "--frozen", "--format=json"],
        0,
    ));
    assert_eq!(checked["paths"].as_array().unwrap().len(), 2);
    let ran = Command::new(env!("CARGO_BIN_EXE_prime-lang"))
        .current_dir(root)
        .args(["run", "demo", "-p", "app", "--frozen"])
        .output()
        .unwrap();
    assert!(
        ran.status.success(),
        "{}",
        String::from_utf8_lossy(&ran.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&ran.stdout).trim(), "42");
}

#[test]
fn native_runtime_try_reports_operator_location_and_target() {
    let dir = tempfile::tempdir().unwrap();
    // A multibyte character precedes the operator to distinguish byte offsets.
    let source = r#"module demo;
// é
enum Result[T, E] { Ok(T), Err(E) }
fn guarded() -> Result[int32, string] {
  await_timeout(sleep_task(10000), 0)?;
  Ok(7)
}
fn main() {
  match guarded() { Ok(n) => out(n), Err(msg) => out(msg) }
}
"#;
    let path = dir.path().join("demo.prime");
    std::fs::write(&path, source).unwrap();
    let invoke = |args: &[&str]| {
        std::process::Command::new(env!("CARGO_BIN_EXE_prime-lang"))
            .args(args)
            .current_dir(dir.path())
            .env_remove("PRIME_TARGET")
            .output()
            .unwrap()
    };
    let check = invoke(&["check", "demo.prime", "--format=json"]);
    assert!(check.status.success(), "{check:?}");
    let checked: serde_json::Value = serde_json::from_slice(&check.stdout).unwrap();
    assert_eq!(checked["schema_version"], 1);
    let run = invoke(&["run", "demo.prime"]);
    assert!(run.status.success(), "{run:?}");
    assert_eq!(String::from_utf8_lossy(&run.stdout).trim(), "timeout");
    let build = invoke(&["build", "demo.prime", "--codegen-format=json"]);
    assert_eq!(build.status.code(), Some(1), "{build:?}");
    let diagnostic: serde_json::Value = serde_json::from_slice(&build.stdout).unwrap();
    assert_eq!(diagnostic["code"], "E0NATIVETRY");
    assert_eq!(diagnostic["phase"], "codegen");
    assert_eq!(diagnostic["target"], "host");
    assert_eq!(
        diagnostic["path"],
        serde_json::json!(path.canonicalize().unwrap())
    );
    let start = source.find('?').unwrap();
    assert_eq!(
        diagnostic["span"],
        serde_json::json!({"start":start,"end":start+1})
    );
    assert!(diagnostic["message"].as_str().unwrap().contains("match"));
}

#[test]
fn task_boundaries_diagnose_nested_borrows_on_host_and_embedded() {
    let dir = tempdir().unwrap();
    fs::write(
        dir.path().join("main.prime"),
        include_str!("../scripts/tests/fixtures/task_ownership/borrowed.prime"),
    )
    .unwrap();
    for build in ["", "[build]\ntarget='riscv32imc-unknown-none-elf'\n"] {
        fs::write(
            dir.path().join("prime.toml"),
            format!(
                "[package]\nname='borrowed'\n[module]\nname='demo'\npath='main.prime'\n{build}"
            ),
        )
        .unwrap();
        let result = report(&check(dir.path(), &["main.prime", "--format=json"], 1));
        let errors = result["diagnostics"].as_array().unwrap();
        for context in [
            "capture `e`",
            "await result",
            "guarded await result",
            "capture `p`",
            "capture `updated`",
            "async result",
            "spawn result",
        ] {
            assert!(
                errors.iter().any(|e| e["code"] == "E0ASYNCSEND"
                    && e["message"].as_str().unwrap().contains(context)),
                "{context}: {result}"
            );
        }
    }
}
