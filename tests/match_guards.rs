use std::{
    fs,
    io::Write,
    process::{Command, Stdio},
};
use tempfile::tempdir;

const PROGRAM: &str = r#"
module main;
enum Result[T, E] { Ok(T), Err(E) }

fn classify(input: Result[int32, string]) -> string {
  match input {
    Ok(n) if n > 0 => "positive",
    Ok(_) => "other",
    Err(_) => "invalid",
  }
}

fn above(n: int32, threshold: int32) -> bool {
  out(`test {n} > {threshold}`);
  n > threshold
}

fn main() {
  let input = in[int32]("");
  out(classify(input));
  let mut bool accept = false;
  match input {
    Ok(n) if above(n, 5) => out(`large {n}`),
    Ok(n) if above(n, 0) => out(`small {n}`),
    _ if { out("fallback guard"); accept } => out("unreachable"),
    rest => match rest {
      Ok(n) => out(`fallback {n}`),
      Err(msg) => out(msg),
    },
  }
  match in[bool]("") {
    Ok(flag) if flag == true => out("enabled"),
    _ => out("disabled"),
  }
}
"#;

#[test]
fn ordered_enum_guards_match_in_both_backends() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("main.prime"), PROGRAM).unwrap();
    let cli = env!("CARGO_BIN_EXE_prime-lang");
    let build = Command::new(cli)
        .current_dir(dir.path())
        .args(["build", "main.prime", "--name", "guards"])
        .output()
        .unwrap();
    assert!(build.status.success(), "{build:?}");
    let native = dir.path().join(".build.prime/guards/guards");

    for (input, expected) in [
        (
            "0\ntrue",
            "other\ntest 0 > 5\ntest 0 > 0\nfallback guard\nfallback 0\nenabled\n",
        ),
        (
            "1\nfalse",
            "positive\ntest 1 > 5\ntest 1 > 0\nsmall 1\ndisabled\n",
        ),
        ("8\ntrue", "positive\ntest 8 > 5\nlarge 8\nenabled\n"),
        (
            "bad\ntrue",
            "invalid\nfallback guard\ninvalid integer input\nenabled\n",
        ),
    ] {
        let mut interpreted = Command::new(cli);
        interpreted.args(["run", "main.prime"]);
        for mut cmd in [interpreted, Command::new(&native)] {
            let mut child = cmd
                .current_dir(dir.path())
                .env_remove("PRIME_TEST_INPUTS")
                .env_remove("PRIME_TEST_INPUTS_FILE")
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .unwrap();
            writeln!(child.stdin.take().unwrap(), "{input}").unwrap();
            let output = child.wait_with_output().unwrap();
            assert!(output.status.success(), "{cmd:?}: {output:?}");
            assert_eq!(
                String::from_utf8(output.stdout).unwrap(),
                expected,
                "{cmd:?} with input {input}"
            );
        }
    }
}
