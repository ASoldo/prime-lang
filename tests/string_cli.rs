//! Verify loop-carried strings and boxed string payloads through the host CLI.
use std::{fs, process::Command};

#[test]
fn mutable_string_survives_conditionals_and_loop_back_edges() {
    let work = tempfile::tempdir().unwrap();
    let dir = work.path();
    fs::write(
        dir.join("main.prime"),
        r#"
enum Result[T, E] { Ok(T), Err(E) }
enum Item { Text(string) }
fn decorate(n: int32) -> string {
  let mut string text = "A";
  for i in (n - n)..n {
    if i != 1 { text = text + "é"; }
  }
  text
}
fn main() {
  let mut string constant = "x";
  constant = constant + "y";
  out(constant.str_len());
  while let Result::Ok(n) = in[int32]("") {
    let value = decorate(n);
    match Item::Text(value + "!") { Item::Text(message) => out(message) }
  }
}
"#,
    )
    .unwrap();
    let cli = env!("CARGO_BIN_EXE_prime-lang");
    let build = Command::new(cli)
        .current_dir(dir)
        .args(["build", "main.prime", "--name", "strings"])
        .output()
        .unwrap();
    assert!(build.status.success(), "{build:?}");
    let input = dir.join("input");
    fs::write(&input, "3\n0\n1\n").unwrap();
    let mut interpreter = Command::new(cli);
    interpreter.args(["run", "main.prime"]);
    for mut command in [
        interpreter,
        Command::new(dir.join(".build.prime/strings/strings")),
    ] {
        let output = command
            .current_dir(dir)
            .env_remove("PRIME_TEST_INPUTS")
            .env_remove("PRIME_TEST_INPUTS_FILE")
            .stdin(fs::File::open(&input).unwrap())
            .output()
            .unwrap();
        assert!(output.status.success(), "{command:?}: {output:?}");
        assert_eq!(
            String::from_utf8(output.stdout).unwrap(),
            "2\nAéé!\nA!\nAé!\n"
        );
    }
}
