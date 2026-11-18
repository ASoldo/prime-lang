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
