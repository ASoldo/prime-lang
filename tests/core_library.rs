use serde_json::Value;
use std::{
    fs,
    path::Path,
    process::{Command, Output},
};
use tempfile::tempdir;

const CLEAN: &str = r#"module consumer;
import core::prelude::{Option, Result, Iterator, Iterable};

fn first(xs: []int32) -> Result[int32, string] {
  let Iterator[int32] it = xs.iter();
  match it.next() {
    Option::Some(x) => Result::Ok(x),
    Option::None => Result::Err("empty"),
  }
}

fn main() {
  let []int32 xs = [42];
  match first(xs) {
    Result::Ok(x) => out(x),
    Result::Err(msg) => out(msg),
  }
}"#;

fn manifest(dir: &Path, no_std: bool, direct_file: bool) {
    let core = Path::new(env!("CARGO_MANIFEST_DIR")).join("workspace/core");
    let libraries = if direct_file {
        format!(
            "[libraries]\nlegacy={{name='core::types',path={:?},visibility='pub',no_std=true}}",
            core.join("types.prime")
        )
    } else {
        format!("[dependencies]\ncore={{path={core:?}}}")
    };
    fs::write(dir.join("prime.toml"), format!(
        "manifest_version='3'\n[package]\nname='consumer'\n[module]\nname='consumer'\npath='main.prime'\nno_std={no_std}\n{libraries}\n"
    )).unwrap();
}

fn cli(dir: &Path, args: &[&str], success: bool) -> Output {
    let output = Command::new(env!("CARGO_BIN_EXE_prime-lang"))
        .current_dir(dir)
        .env_remove("PRIME_TARGET")
        .args(args)
        .output()
        .unwrap();
    assert_eq!(output.status.success(), success, "{args:?}: {output:?}");
    output
}

fn check(dir: &Path, source: &str, ok: bool) -> Value {
    fs::write(dir.join("main.prime"), source).unwrap();
    serde_json::from_slice(&cli(dir, &["check", "main.prime", "--format=json"], ok).stdout).unwrap()
}

#[test]
fn external_clean_core_runs_on_host_and_in_a_no_std_module() {
    let tmp = tempdir().unwrap();
    for no_std in [false, true] {
        manifest(tmp.path(), no_std, false);
        let report = check(tmp.path(), CLEAN, true);
        assert_eq!(report["paths"].as_array().unwrap().len(), 2, "{report}");
        let output = cli(tmp.path(), &["run", "main.prime"], true);
        assert_eq!(String::from_utf8(output.stdout).unwrap().trim(), "42");
        let report = check(
            tmp.path(),
            "module consumer; import core::prelude::{Player}; fn main() {}",
            false,
        );
        assert!(
            report["diagnostics"]
                .as_array()
                .unwrap()
                .iter()
                .any(|d| d["message"] == "`Player` not found in `core`"),
            "{report}"
        );
    }
    let report = check(
        tmp.path(),
        "module consumer; import core; fn main() { out(fs_exists(\"file\")); }",
        false,
    );
    assert!(
        report["diagnostics"]
            .as_array()
            .unwrap()
            .iter()
            .any(|d| d["code"] == "E0NS"),
        "{report}"
    );
}

#[test]
fn external_legacy_module_and_package_paths_keep_the_original_types() {
    let tmp = tempdir().unwrap();
    let program = r#"module consumer;
import core::types::prelude::{*};
import core::types::{Player, Quest, Iterator, Iterable, Result, Option};
fn main() {
  let Quest quest = Quest{"gather", 42, false};
  let Option[int32] reward = Some(quest.reward);
  match reward { Some(x) => out(x), None => out(0), }
}"#;
    for direct_file in [false, true] {
        manifest(tmp.path(), true, direct_file);
        check(tmp.path(), program, true);
        let output = cli(tmp.path(), &["run", "main.prime"], true);
        assert_eq!(String::from_utf8(output.stdout).unwrap().trim(), "42");
    }
    manifest(tmp.path(), false, false);
    let mixed = program.replace("fn main()", "import core;\nimport app::model;\nfn main()");
    check(tmp.path(), &mixed, true);
}

#[test]
fn external_consumer_can_use_a_package_that_maps_only_the_legacy_file() {
    let tmp = tempdir().unwrap();
    let diagram = Path::new(env!("CARGO_MANIFEST_DIR")).join("workspace/plugins/diagram");
    fs::write(tmp.path().join("prime.toml"), format!(
        "[module]\nname='consumer'\npath='main.prime'\n[dependencies]\ndiagram={{path={diagram:?}}}\n"
    )).unwrap();
    check(
        tmp.path(),
        r#"module consumer;
import prime::diagram::{Diagram, parse_diagram};
fn main() {
  match parse_diagram("title=External\nnode=in|Input|source") {
    Ok(diagram) => out(diagram.title),
    Err(msg) => out(msg),
  }
}"#,
        true,
    );
    assert_eq!(
        cli(tmp.path(), &["run", "main.prime"], true).stdout,
        b"External\n"
    );
}

#[test]
fn relocated_cli_checks_runs_and_builds_with_its_bundled_core() {
    let tmp = tempdir().unwrap();
    let cli = tmp.path().join("prime-lang");
    fs::copy(env!("CARGO_BIN_EXE_prime-lang"), &cli).unwrap();
    let consumer = tmp.path().join("consumer");
    fs::create_dir(&consumer).unwrap();
    let source = CLEAN.replace("import core::prelude", "import core;\nimport core::prelude");
    fs::write(consumer.join("main.prime"), &source).unwrap();

    let execute = |exe: &Path, args: &[&str]| {
        let output = Command::new(exe)
            .current_dir(&consumer)
            .args(args)
            .env_remove("PRIME_TARGET")
            .output()
            .unwrap();
        assert!(output.status.success(), "{args:?}: {output:?}");
        output.stdout
    };
    let report: Value =
        serde_json::from_slice(&execute(&cli, &["check", "main.prime", "--format=json"])).unwrap();
    assert!(report["manifest_path"].is_null());
    let paths = report["paths"].as_array().unwrap();
    assert_eq!(paths.len(), 2);
    assert!(
        paths.contains(&Value::from("<bundled>/core.prime")),
        "{report}"
    );
    assert_eq!(
        prime_lang::project::read_module_source(Path::new("<bundled>/core.prime")).unwrap(),
        include_str!("../workspace/core/core.prime")
    );
    assert_eq!(execute(&cli, &["run", "main.prime"]), b"42\n");
    assert_eq!(fs::read_dir(&consumer).unwrap().count(), 1);

    execute(&cli, &["build", "main.prime", "--name", "probe"]);
    assert_eq!(
        execute(&consumer.join(".build.prime/probe/probe"), &[]),
        b"42\n"
    );
}

#[test]
fn local_core_wins_and_missing_legacy_modules_still_report_their_paths() {
    let tmp = tempdir().unwrap();
    let dir = tmp.path();
    let legacy = check(
        dir,
        "module consumer; import core::types; fn main() {}",
        false,
    );
    let diagnostic = &legacy["diagnostics"][0];
    assert_eq!(diagnostic["phase"], "io");
    assert_eq!(
        diagnostic["path"],
        serde_json::json!(dir.join("core/types.prime"))
    );

    fs::create_dir(dir.join("nested")).unwrap();
    fs::write(
        dir.join("nested/lib.prime"),
        "library nested::lib; import core;",
    )
    .unwrap();
    let custom = dir.join("core.prime");
    fs::write(&custom, "library core; pub struct Marker { v: int32; }").unwrap();
    let source = "module consumer; import nested::lib; import core::prelude::{Marker}; fn main() { out(Marker{7}.v); }";
    let report = check(dir, source, true);
    assert!(
        !report["paths"].to_string().contains("<bundled>"),
        "{report}"
    );
    assert_eq!(cli(dir, &["run", "main.prime"], true).stdout, b"7\n");
    fs::write(&custom, "library core; pub struct Marker { broken: }").unwrap();
    let report = check(dir, source, false);
    assert_eq!(report["diagnostics"][0]["phase"], "syntax");
    assert_eq!(report["diagnostics"][0]["path"], serde_json::json!(custom));

    let mapped = dir.join("portable.prime");
    fs::write(&mapped, "library core; pub struct Marker { v: int32; }").unwrap();
    fs::write(dir.join("prime.toml"),
        "[module]\nname='consumer'\npath='main.prime'\n[libraries]\ncore={path='portable.prime',visibility='pub'}\n").unwrap();
    let report = check(dir, source, true);
    assert!(
        report["paths"]
            .as_array()
            .unwrap()
            .contains(&serde_json::json!(mapped))
    );
    assert!(!report["paths"].to_string().contains("<bundled>"));
}

#[test]
fn no_std_manifest_and_stdin_can_import_core_without_a_dependency() {
    use std::{io::Write, process::Stdio};
    let tmp = tempdir().unwrap();
    fs::write(
        tmp.path().join("prime.toml"),
        "[module]\nname='consumer'\npath='main.prime'\nno_std=true\n",
    )
    .unwrap();
    check(tmp.path(), CLEAN, true);
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_prime-lang"))
        .args(["check", "-", "--format=json"])
        .current_dir(tmp.path())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    cmd.stdin
        .take()
        .unwrap()
        .write_all(CLEAN.as_bytes())
        .unwrap();
    let output = cmd.wait_with_output().unwrap();
    assert!(output.status.success(), "{output:?}");
    let report: Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(report["entry_path"], "<stdin>");
    assert!(
        report["paths"]
            .as_array()
            .unwrap()
            .contains(&Value::from("<bundled>/core.prime"))
    );
}

#[test]
fn old_core_types_still_runs_alone_and_conflicts_point_into_the_right_file() {
    let tmp = tempdir().unwrap();
    let dir = tmp.path();
    fs::create_dir(dir.join("core")).unwrap();
    fs::write(
        dir.join("core/types.prime"),
        r#"library core::types;
export prelude { Option, Result };
pub enum Option[T] { Some(T), None, }
pub enum Result[T, E] { Ok(T), Err(E), }
"#,
    )
    .unwrap();
    let source =
        "module app; import core::types; fn main() { let Option[int32] n = Some(8); out(n); }";
    let report = check(dir, source, true);
    assert!(!report["paths"].to_string().contains("<bundled>"));
    assert!(
        cli(dir, &["run", "main.prime"], true)
            .stdout
            .ends_with(b"8)\n")
    );
    for import in ["import core;", "import core::prelude;"] {
        let mixed = source.replace("fn main", &format!("{import} fn main"));
        let report = check(dir, &mixed, false);
        for diagnostic in report["diagnostics"].as_array().unwrap() {
            let path = Path::new(diagnostic["path"].as_str().unwrap());
            let text = prime_lang::project::read_module_source(path).unwrap();
            let start = diagnostic["span"]["start"].as_u64().unwrap() as usize;
            let end = diagnostic["span"]["end"].as_u64().unwrap() as usize;
            let highlighted = text.get(start..end).expect("valid source range");
            assert!(
                highlighted.contains("import") || ["Option", "Result"].contains(&highlighted),
                "{diagnostic}: {highlighted}"
            );
        }
    }
}
