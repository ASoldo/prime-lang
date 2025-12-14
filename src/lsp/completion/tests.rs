use super::*;
use crate::language::parser::parse_module;
use crate::project::manifest::{PackageManifest, manifest_key_for};
use std::fs;
use std::path::PathBuf;
use tempfile::tempdir;
use tower_lsp_server::ls_types::CompletionTriggerKind;

struct ManifestEntry<'a> {
    name: &'a str,
    path: &'a str,
    visibility: &'a str,
    package: Option<&'a str>,
    doc: Option<&'a str>,
}

fn manifest_with_entries(entries: &[ManifestEntry<'_>]) -> (tempfile::TempDir, PackageManifest) {
    let dir = tempdir().expect("tempdir");
    let manifest_path = dir.path().join("prime.toml");
    let mut manifest = String::from(
        r#"manifest_version = "3"

[package]
name = "demo"
version = "0.1.0"
"#,
    );
    manifest.push_str("\n[modules]\n");
    for entry in entries {
        let module_path = dir.path().join(entry.path);
        if let Some(parent) = module_path.parent() {
            fs::create_dir_all(parent).expect("create module dir");
        }
        fs::write(&module_path, "module demo;").expect("write module");
        let key = manifest_key_for(entry.name);
        manifest.push_str(&format!(
            "{key} = {{ name = \"{}\", path = \"{}\", visibility = \"{}\"",
            entry.name, entry.path, entry.visibility
        ));
        if let Some(package) = entry.package {
            manifest.push_str(&format!(", package = \"{package}\""));
        }
        if let Some(doc) = entry.doc {
            manifest.push_str(&format!(", doc = \"{doc}\""));
        }
        manifest.push_str(" }\n");
    }
    fs::write(&manifest_path, manifest).expect("write manifest");
    let manifest = PackageManifest::load(&manifest_path).expect("load manifest");
    (dir, manifest)
}

#[test]
fn detects_module_context_in_declarations() {
    let text = "module demo::core;";
    let ctx = module_path_completion_context(text, text.len()).expect("context");
    assert!(matches!(ctx.kind, ModulePathCompletionKind::Declaration));
    assert_eq!(ctx.prefix.as_deref(), Some("demo::core"));
}

#[test]
fn module_completion_prioritizes_expected_entry() {
    let entries = [
        ManifestEntry {
            name: "app::main",
            path: "src/main.prime",
            visibility: "pub",
            package: Some("app"),
            doc: Some("app entry"),
        },
        ManifestEntry {
            name: "lib::math",
            path: "lib/math.prime",
            visibility: "package",
            package: None,
            doc: None,
        },
    ];
    let (_dir, manifest) = manifest_with_entries(&entries);
    let prioritized =
        module_completion_items_from_manifest(&manifest, Some("app"), Some("app::main"));
    assert!(!prioritized.is_empty());
    assert_eq!(prioritized[0].label, "app::main");

    let all_items = module_completion_items_from_manifest(&manifest, None, None);
    let lib = all_items
        .iter()
        .find(|item| item.label == "lib::math")
        .expect("lib completion item");
    let detail = lib.detail.as_ref().expect("detail");
    assert!(detail.contains("[package]"));
}

#[test]
fn completion_prefix_uses_trigger_when_identifier_missing() {
    let ctx = CompletionContext {
        trigger_kind: CompletionTriggerKind::TRIGGER_CHARACTER,
        trigger_character: Some("a".into()),
    };
    let prefix = completion_prefix("a", 0, Some(&ctx)).expect("prefix");
    assert_eq!(prefix, "a");
}

#[test]
fn member_completion_uses_destructured_tuple_types() {
    let source = r#"
module demo::lab;

struct Result {
  normalized: int32;
}

fn make() -> (bool, Result) {
  return true, Result{ normalized: 3 };
}

fn main() {
  let (ok, res) = make();
  res.normalized;
}
"#;
    let module =
        parse_module("demo::lab", PathBuf::from("lab.prime"), source).expect("parse module");
    let structs = collect_struct_info(std::slice::from_ref(&module));
    let interfaces = collect_interface_info(std::slice::from_ref(&module));
    let offset = source
        .find("res.normalized")
        .map(|idx| idx + "res.".len())
        .expect("find member access");
    let chain = expression_chain_before_dot(source, offset).expect("expression chain before dot");
    let items =
        member_completion_items(source, &chain, &structs, &interfaces, None, &module, offset)
            .expect("completion items for res");
    assert!(
        items.iter().any(|item| item.label == "normalized"),
        "expected normalized field in completion items, got {:?}",
        items
            .iter()
            .map(|item| item.label.clone())
            .collect::<Vec<_>>()
    );
}

#[test]
fn macro_completion_respects_visibility() {
    let provider_src = r#"
module pkg::macros;

macro local_only() -> int32 { 1 }
pub(package) macro pkg_only() -> int32 { 2 }
pub macro exported() -> int32 { 3 }
"#;
    let same_pkg_src = r#"
module pkg::user;

import pkg::macros;

fn main() -> int32 { 0 }
"#;
    let other_pkg_src = r#"
module other::user;

import pkg::macros;

fn main() -> int32 { 0 }
"#;

    let provider = parse_module(
        "pkg::macros",
        PathBuf::from("pkg/macros.prime"),
        provider_src,
    )
    .expect("provider module");
    let same_pkg = parse_module("pkg::user", PathBuf::from("pkg/user.prime"), same_pkg_src)
        .expect("same package module");
    let other_pkg = parse_module(
        "other::user",
        PathBuf::from("other/user.prime"),
        other_pkg_src,
    )
    .expect("other package module");
    let modules = vec![provider.clone(), same_pkg.clone(), other_pkg.clone()];

    let same_pkg_items =
        general_completion_items(&same_pkg, &modules, None, None, &BuildTarget::host());
    assert!(
        same_pkg_items.iter().any(|item| item.label == "~pkg_only"),
        "expected package macro visible inside the package"
    );
    assert!(same_pkg_items.iter().any(|item| item.label == "~exported"));
    assert!(
        !same_pkg_items
            .iter()
            .any(|item| item.label == "~local_only"),
        "private macro should stay local to the defining module"
    );

    let other_pkg_items =
        general_completion_items(&other_pkg, &modules, None, None, &BuildTarget::host());
    assert!(other_pkg_items.iter().any(|item| item.label == "~exported"));
    assert!(
        !other_pkg_items.iter().any(|item| item.label == "~pkg_only"),
        "package macro should be hidden outside the package"
    );
    assert!(
        !other_pkg_items
            .iter()
            .any(|item| item.label == "~local_only"),
        "private macro should be hidden outside the defining module"
    );

    let pkg_only_detail = same_pkg_items
        .iter()
        .find(|item| item.label == "~pkg_only")
        .and_then(|item| item.detail.as_ref())
        .expect("detail for pkg_only macro");
    assert!(
        pkg_only_detail.contains("pub(package) macro"),
        "expected macro visibility in completion detail, got {pkg_only_detail}"
    );
}
