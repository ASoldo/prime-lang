use serde_json::{Value, json};
use std::{
    fs,
    path::Path,
    process::{Command, Output},
};
use tempfile::tempdir;

fn run(root: &Path, trace: bool) -> Output {
    let mut command = Command::new(env!("CARGO_BIN_EXE_prime-lang"));
    command.arg("run").arg("demo.prime");
    if trace {
        command.arg("--trace");
    }
    command
        .current_dir(root)
        .env_remove("PRIME_TARGET")
        .env("RUST_BACKTRACE", "1")
        .output()
        .unwrap()
}

fn records(output: &Output) -> Vec<Value> {
    std::str::from_utf8(&output.stderr)
        .unwrap()
        .lines()
        .map(|line| serde_json::from_str(line).unwrap_or_else(|err| panic!("{err}: {line}")))
        .collect()
}

#[test]
fn trace_follows_execution_through_imports_with_utf8_byte_spans() {
    let dir = tempdir().unwrap();
    let root = dir.path();
    let demo = "module demo;\nimport lib::{twice};\n// ☕\nfn unused() {}\nfn main() { out(twice(21)); }\n";
    let lib = "library lib;\n// čaj\nfn add(x: int32) -> int32 { x + x }\npub fn twice(x: int32) -> int32 { add(x) }\n";
    fs::write(root.join("demo.prime"), demo).unwrap();
    fs::write(root.join("lib.prime"), lib).unwrap();
    let plain = run(root, false);
    assert!(plain.status.success(), "{plain:?}");
    assert!(plain.stderr.is_empty());
    assert_eq!(plain.stdout, b"42\n");
    let traced = run(root, true);
    assert_eq!(traced.stdout, plain.stdout);
    assert_eq!(traced.status, plain.status);
    assert_eq!(run(root, true).stderr, traced.stderr);
    let events = records(&traced);
    assert_eq!(events.len(), 3);
    for (depth, (name, module, source)) in [
        ("main", "demo", demo),
        ("twice", "lib", lib),
        ("add", "lib", lib),
    ]
    .into_iter()
    .enumerate()
    {
        let start = source.find(&format!("fn {name}(")).unwrap() + 3;
        let path = root.join(format!("{module}.prime")).canonicalize().unwrap();
        assert_eq!(
            events[depth],
            json!({"schema_version":1, "event":"call", "module":module, "function":name, "path":path, "start":start, "end":start + name.len(), "depth":depth})
        );
    }
}

#[test]
fn reports_failing_expression_and_call_sites_in_both_modes() {
    let dir = tempdir().unwrap();
    let root = dir.path();
    let source = "module demo;\n// ☕\nfn quotient(x: int32) -> int32 { 42 / x }\nfn report(x: int32) { out(quotient(x)); }\nfn main() { out(7); report(0); }\n";
    let path = root.join("demo.prime");
    fs::write(&path, source).unwrap();
    let plain = run(root, false);
    assert_eq!(plain.status.code(), Some(1), "{plain:?}");
    assert_eq!(plain.stdout, b"7\n");
    let mut message = "Runtime panic: integer division by zero".to_string();
    for (name, expr) in [
        ("quotient", "42 / x"),
        ("report", "quotient(x)"),
        ("main", "report(0)"),
    ] {
        let start = source.rfind(expr).unwrap();
        let path = path.canonicalize().unwrap();
        message.push_str(&format!(
            "\n  at demo::{name} ({}, bytes {start}..{})",
            path.display(),
            start + expr.len()
        ));
    }
    assert_eq!(
        String::from_utf8(plain.stderr).unwrap(),
        format!("Runtime error: {message}\n")
    );
    let traced = run(root, true);
    assert_eq!(traced.stdout, plain.stdout);
    assert_eq!(traced.status, plain.status);
    let events = records(&traced);
    assert_eq!(events.len(), 4);
    assert_eq!(
        events[3],
        json!({"schema_version":1, "event":"runtime_error", "message":message})
    );
    assert_eq!(run(root, true).stderr, traced.stderr);
}
