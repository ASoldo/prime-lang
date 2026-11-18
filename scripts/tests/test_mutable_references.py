"""Exercise named reference parameters through the host CLI and linked binary."""
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
PROGRAM = """module example;
import core::prelude::{Result};

fn bump(n: &mut int32) { *n = *n + 1; }
fn twice(n: &mut int32) { bump(n); bump(n); }
fn add_local(r: &mut int32) {
  let mut int32 n = 1;
  bump(&mut n);
  *r = *r + n;
}
fn toggle(flag: &mut bool) { *flag = !*flag; }

fn main() {
  match in[int32]("") {
    Ok(input) => {
      let mut int32 n = input;
      bump(&mut n);
      out(n);
      bump(&mut n);
      out(n);
      {
        let alias = &mut n;
        twice(alias);
        out(*alias);
      }
      let mut int32 i = 0;
      while i < 2 { twice(&mut n); i = i + 1; }
      out(n);
      if input > 0 { add_local(&mut n); }
      out(n);
      let mut bool flag = n > 0;
      toggle(&mut flag);
      out(flag);
    },
    Err(_) => out("invalid"),
  }
}
"""


class MutableReferences(unittest.TestCase):
    def setUp(self):
        scratch = tempfile.TemporaryDirectory()
        self.addCleanup(scratch.cleanup)
        self.work = Path(scratch.name)
        self.source = self.work / "example.prime"
        self.cli = os.environ.get("PRIME_BIN", str(ROOT / "target/debug/prime-lang"))
        self.env = os.environ.copy()
        self.env["CARGO_TARGET_DIR"] = str(Path(self.env.get("CARGO_TARGET_DIR", ROOT / "target")).resolve())
        for key in ("PRIME_TEST_INPUTS", "PRIME_TEST_INPUTS_FILE", "PRIME_TARGET"):
            self.env.pop(key, None)

    def execute(self, *args, data="", success=True):
        result = subprocess.run(args, cwd=self.work, env=self.env, input=data, text=True,
                                capture_output=True, timeout=300)
        self.assertEqual(result.returncode == 0, success, result)
        return result.stdout

    def check_and_build(self, source):
        self.source.write_text(source)
        result = json.loads(self.execute(self.cli, "check", self.source, "--format", "json"))
        self.assertEqual(result["outcome"], "ok")
        self.execute(self.cli, "build", self.source, "--name", "example")

    def compare(self, data, expected):
        for cmd in ((self.cli, "run", self.source), (self.work / ".build.prime/example/example",)):
            with self.subTest(command=cmd, data=data):
                self.assertEqual(self.execute(*cmd, data=data), expected)

    def test_architecture_example(self):
        section = (ROOT / "docs/Architecture.md").read_text().split("### A mutable parameter across a call\n", 1)[1]
        self.check_and_build(section.split("```prime\n", 1)[1].split("\n```", 1)[0])
        self.compare("", "7\n")

    def test_repeated_and_forwarded_borrows_of_runtime_input(self):
        self.check_and_build(PROGRAM)
        self.compare("6\n", "7\n8\n10\n14\n16\nfalse\n")
        self.compare("-11\n", "-10\n-9\n-7\n-3\n-3\ntrue\n")

    def test_exclusive_borrow_in_call(self):
        for body in (
            "both(&mut n, &mut n);",
            "let r = &mut n; both(r, r);",
            "let r = &mut n; bump(r); bump(&mut n);",
            "inspect(&mut n, &n);",
        ):
            with self.subTest(body=body):
                self.source.write_text("module example;\n"
                                       "fn bump(n: &mut int32) { *n = 1; }\n"
                                       "fn both(x: &mut int32, y: &mut int32) {}\n"
                                       "fn inspect(x: &mut int32, y: &int32) {}\n"
                                       "fn main() { let mut int32 n = 0; " + body + " }")
                report = json.loads(self.execute(self.cli, "check", self.source, "--format", "json", success=False))
                self.assertTrue(any("already mutably borrowed" in d["message"] for d in report["diagnostics"]), report)


if __name__ == "__main__":
    unittest.main()
