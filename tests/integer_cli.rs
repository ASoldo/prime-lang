use serde_json::Value;
use std::{
    fs,
    path::Path,
    process::{Command, Output},
};
use tempfile::tempdir;

fn cli(work: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_prime-lang"))
        .current_dir(work)
        .env_remove("PRIME_TARGET")
        .args(args)
        .output()
        .unwrap()
}

fn success(output: &Output) -> &str {
    assert!(output.status.success(), "{output:?}");
    std::str::from_utf8(&output.stdout).unwrap()
}

fn run_both(body: &str) -> [Output; 2] {
    let tmp = tempdir().unwrap();
    let work = tmp.path();
    fs::write(work.join("main.prime"), format!("module probe; {body}")).unwrap();
    success(&cli(work, &["check", "main.prime", "--format", "json"]));
    let interpreted = cli(work, &["run", "main.prime"]);
    success(&cli(work, &["build", "main.prime", "--name", "probe"]));
    let native = Command::new(work.join(".build.prime/probe/probe"))
        .output()
        .unwrap();
    [interpreted, native]
}

#[test]
fn integer_comparisons_results_and_explicit_casts_agree() {
    let outputs = run_both(
        r#"
        enum V { Byte(uint8) }
        fn add_word(x: int64) -> int64 { x + 4294967296 }
        fn byte(x: uint8) -> uint8 { x }
        fn main() {
            let int64 a = 9007199254740993;
            let int64 b = 9007199254740992;
            out(a > b);
            let x = now_ms();
            let int64 n = 4294967296;
            out(x + n > x);
            out(x + n < x);
            let d = x - x;
            let choice: int64 = if d == 0 { d + n } else { 1 };
            out(choice);
            let mut int64 slot = 0;
            slot = n + d;
            out(slot);
            out(cast[uint8](256));
            out(cast[int8](255));
            out(cast[int8](128));
            out(cast[uint64](-1));
            let hi = cast[uint64](d - 1);
            let uint64 max = 18446744073709551615;
            out(hi == max);
            out(hi / cast[uint64](2));
            let V v = V::Byte(d + 255);
            match v { Byte(k) => out(k) }
            let int8 low = -128;
            out(low);
            out(add_word(d + n));
            out(byte(d + 255));
            let identity = |z: uint64| -> uint64 { z };
            out(identity(hi));
        }
    "#,
    );
    for output in &outputs {
        assert_eq!(
            success(output),
            "true\ntrue\nfalse\n4294967296\n4294967296\n0\n-1\n-128\n18446744073709551615\ntrue\n9223372036854775807\n255\n-128\n8589934592\n255\n18446744073709551615\n"
        );
    }
}

#[test]
fn dynamic_narrowing_and_arithmetic_fail_at_execution() {
    for body in [
        "enum V { Byte(uint8) } fn main() { let x = now_ms(); let wide = x - x + 256; let v = Byte(wide); out(v); }",
        "fn main() { let x = now_ms(); let uint8 a = cast[uint8](x - x + 255); out(a + 1); }",
        "fn byte(n: uint8) -> uint8 { n } fn main() { let x = now_ms(); out(byte(x - x + 256)); }",
        "fn byte(n: int64) -> uint8 { n } fn main() { let x = now_ms(); out(byte(x - x + 256)); }",
        "fn main() { let x = now_ms(); let uint8 small = x - x + 256; out(small); }",
    ] {
        for output in run_both(body) {
            assert!(!output.status.success(), "{output:?}");
            assert!(
                String::from_utf8_lossy(&output.stderr).contains("out of range for `uint8`"),
                "{output:?}"
            );
        }
    }
}

#[test]
fn check_rejects_constant_overflow_and_uses_target_pointer_width() {
    let work = tempdir().unwrap();
    for (expr, diagnostic) in [
        ("V::Byte(255 + 1)", "out of range for `uint8`"),
        ("Byte(16 * 16)", "out of range for `uint8`"),
        ("Byte(cast[int64](256))", "out of range for `uint8`"),
        ("Byte(1 / 0)", "integer division by zero"),
    ] {
        fs::write(
            work.path().join("main.prime"),
            format!("enum V {{ Byte(uint8) }} fn main() {{ let v = {expr}; }}"),
        )
        .unwrap();
        let output = cli(work.path(), &["check", "main.prime", "--format", "json"]);
        assert!(!output.status.success(), "{expr}");
        let report: Value = serde_json::from_slice(&output.stdout).unwrap();
        assert_eq!(report["schema_version"], 1);
        assert!(
            report["diagnostics"].to_string().contains(diagnostic),
            "{report}"
        );
    }
    fs::write(
        work.path().join("main.prime"),
        "fn main() { let usize n = 4294967295 + 1; out(n); }",
    )
    .unwrap();
    let small = Command::new(env!("CARGO_BIN_EXE_prime-lang"))
        .current_dir(work.path())
        .env("PRIME_TARGET", "riscv32imc-unknown-none-elf")
        .args(["check", "main.prime", "--format", "json"])
        .output()
        .unwrap();
    assert!(!small.status.success());
    assert!(String::from_utf8_lossy(&small.stdout).contains("out of range for `usize`"));
    let host = cli(work.path(), &["check", "main.prime", "--format", "json"]);
    assert_eq!(host.status.success(), usize::BITS == 64);
}

#[test]
fn interpreter_uses_the_selected_pointer_width_for_casts() {
    let work = tempdir().unwrap();
    fs::write(work.path().join("main.prime"), "fn main() { let mut int64 d = 0; out(cast[usize](d - 1)); out(cast[isize](d + 4294967295)); }").unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_prime-lang"))
        .current_dir(work.path())
        .env("PRIME_TARGET", "riscv32imc-unknown-none-elf")
        .args(["run", "main.prime"])
        .output()
        .unwrap();
    assert_eq!(success(&output), "4294967295\n-1\n");
    let host = cli(work.path(), &["run", "main.prime"]);
    let expected = format!("{}\n{}\n", usize::MAX, 4294967295_i64 as isize);
    assert_eq!(success(&host), expected);
}
