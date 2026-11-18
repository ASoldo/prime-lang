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
fn twice(n: &mut int32) { bump(&mut *n); bump(n); }
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

    def execute(self, *args, data="", success=True, error=None):
        result = subprocess.run(args, cwd=self.work, env=self.env, input=data, text=True,
                                capture_output=True, timeout=300)
        self.assertGreaterEqual(result.returncode, 0, result)
        self.assertEqual(result.returncode == 0, success, result)
        if error is not None:
            self.assertIn(error, result.stderr)
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

    def test_explicit_reborrow_updates_original_binding(self):
        self.check_and_build("""module example;
            fn bump(n: &mut int32) { *n = *n + 1; }
            fn relay(r: &mut int32) { bump(&mut *r); }
            fn main() { let mut int32 n = 6; relay(&mut n); out(n); }
        """)
        self.compare("", "7\n")

    def test_projected_places_update_only_the_selected_storage(self):
        self.check_and_build("""module example;
import core::prelude::{Result};
struct Pair { x: int32; y: int32; }
fn bump(r: &mut int32) { *r = *r + 1; }
fn relay(r: &mut int32) { bump(&mut *r); bump(r); }
fn both(a: &mut int32, b: &mut int32) { *a = *a + 10; *b = *b + 20; }
fn field(p: &mut Pair) { relay(&mut p.x); }
fn choose(i: int32) -> int32 { out("index"); i }
fn main() {
  match in[int32]("") {
    Ok(i) => {
      let mut Pair pair = Pair{10, 20};
      let Pair original = pair;
      if i == 0 { field(&mut pair); }
      both(&mut pair.x, &mut pair.y);
      out(pair.x); out(pair.y); out(original.x);
      let mut []int32 items = [3, 5, 7];
      both(&mut items[0], &mut items[2]);
      relay(&mut items[choose(i)]);
      out(slice_get_int(items, 0, -1)); out(slice_get_int(items, 1, -1)); out(slice_get_int(items, 2, -1));
    },
    Err(_) => out("invalid"),
  }
}
""")
        self.compare("0\n", "22\n40\n10\nindex\n15\n5\n27\n")
        self.compare("2\n", "20\n40\n10\nindex\n13\n5\n29\n")
        for index in ("-1", "3"):
            for cmd in ((self.cli, "run", self.source), (self.work / ".build.prime/example/example",)):
                with self.subTest(index=index, command=cmd):
                    self.execute(*cmd, data=index + "\n", success=False, error="borrow index out of bounds")

    def test_aliases_reaching_different_parameters(self):
        self.check_and_build("""module example;
            fn both(x: &mut int32, y: &mut int32) { out("entered"); }
            fn receive(a: []int32, b: []int32) {
                let mut []int32 first = a;
                let mut []int32 second = b;
                both(&mut first[0], &mut second[0]);
            }
            fn main() { let []int32 owner = [1]; receive(owner, owner); }
        """)
        for cmd in ((self.cli, "run", self.source), (self.work / ".build.prime/example/example",)):
            self.assertEqual(self.execute(*cmd, success=False, error="already mutably borrowed"), "")

    def test_empty_slice_is_checked_before_the_callee(self):
        self.check_and_build("""module example;
            fn ignore(value: &mut int32) { out("entered"); }
            fn main() { let mut []int32 empty = []; ignore(&mut empty[0]); }
        """)
        for cmd in ((self.cli, "run", self.source), (self.work / ".build.prime/example/example",)):
            self.assertEqual(self.execute(*cmd, success=False, error="borrow index out of bounds"), "")

    def test_fixed_array_and_mutable_owner_reference(self):
        self.check_and_build("""module example;
            import core::prelude::{Option};
            fn flip(flag: &mut bool) { *flag = !*flag; }
            fn relay(flags: &mut [2]bool) { flip(&mut flags[1]); }
            fn main() {
                let mut [2]bool flags = [false, false];
                relay(&mut flags);
                match flags[0] { Some(v) => out(v), None => out("missing"), }
                match flags[1] { Some(v) => out(v), None => out("missing"), }
            }
        """)
        self.compare("", "false\ntrue\n")

    def test_fixed_array_get_after_mutation(self):
        fixture = ROOT / "scripts/tests/fixtures/projected_lookup"
        self.check_and_build((fixture / "main.prime").read_text())
        self.compare("", (fixture / "expected.txt").read_text())

    def test_fixed_array_get_requires_one_integer(self):
        for lookup, message in (("values.get(false)", "an array index must be an integer"),
                                ("values.get(0, 1)", "`get` expects one array index")):
            with self.subTest(lookup=lookup):
                self.source.write_text("module example; import core::prelude::{Option};\n"
                                       "fn main() { let [1]int32 values = [7]; " + lookup + "; }")
                report = json.loads(self.execute(self.cli, "check", self.source, "--format", "json", success=False))
                self.assertEqual([d["message"] for d in report["diagnostics"]], [message])

    def test_projected_constant_initializer(self):
        self.check_and_build("""module example;
            struct Pair { x: int32; y: int32; }
            fn bump(r: &mut int32) { *r = *r + 1; }
            const X: int32 = { let mut Pair p = Pair{6, 9}; bump(&mut p.x); p.x };
            fn main() { out(X); }
        """)
        self.compare("", "7\n")

    def test_invalid_mutable_places_are_located_before_execution(self):
        header = """module example;
// UTF-8 byte offsets: č
struct Pair { field: int32; }
pub interface Update { fn replace(self: &mut Pair, other: &mut Pair); }
impl Update for Pair { fn replace(self: &mut Pair, other: &mut Pair) { *other = Pair{7}; } }
fn bump(n: &mut int32) { *n = *n + 1; }
fn both(x: &mut int32, y: &mut int32) {}
fn with_owner(x: &mut Pair, y: &mut int32) {}
fn identity(r: &mut int32) -> &mut int32 { r }
fn add(x: &mut int32, y: int32) {}
fn read(x: &Pair) -> int32 { x.field }
fn inspect(x: &mut int32, y: &int32) {}
"""
        for function, place, message in (
            ("fn main() { let Pair pair = Pair{field: 6}; bump(&mut pair.field); }",
             "&mut pair.field", "requires a mutable owner"),
            ("const X = { let Pair p = Pair{field: 6}; bump(&mut p.field); p.field };",
             "&mut p.field", "requires a mutable owner"),
            ("fn relay(p: &Pair) { bump(&mut p.field); }",
             "&mut p.field", "requires a mutable owner"),
            ("fn main() { let mut []int32 a = [1,2]; let mut int32 i = 1; both(&mut a[0], &mut a[i]); }",
             "&mut a[i]", "already mutably borrowed"),
            ("fn main() { let mut Pair p = Pair{6}; let r = &mut p; bump(&mut p.field); }",
             "&mut p.field", "already mutably borrowed"),
            ("fn main() { let mut Pair p = Pair{6}; bump(&mut p.missing); }",
             "&mut p.missing", "unknown field"),
            ("fn main() { let mut Pair p = Pair{6}; let escaped = identity(&mut p.field); }",
             "&mut p.field", "requires a call with an owned result"),
            ("fn main() { let mut Pair p = Pair{6}; with_owner(&mut p, &mut p.field); }",
             "&mut p.field", "already mutably borrowed"),
            ("fn main() { let mut Pair p = Pair{6}; add(&mut p.field, read(&p)); }",
             "p", "already mutably borrowed"),
            ("fn main() { let mut []int32 a = [1]; both(&mut a[0], &mut a[0]); }",
             "&mut a[0]", "already mutably borrowed"),
            ("fn main() { let mut Pair p = Pair{6}; let loan = &p; bump(&mut p.field); }",
             "&mut p.field", "active shared borrow"),
            ("fn main() { let mut []int32 a = [1]; let mut []int32 b = a; both(&mut a[0], &mut b[0]); }",
             "&mut b[0]", "already mutably borrowed"),
            ("fn main() { let mut Pair p = Pair{6}; both(&mut p.field, &mut p.field); }",
             "&mut p.field", "already mutably borrowed"),
            ("fn main() { bump(&mut Pair{field: 6}.field); }",
             "&mut Pair{field: 6}.field", "rooted in a mutable binding"),
            ("fn main() { let mut []int32 a = [6]; bump(&mut a[true]); }",
             "&mut a[true]", "requires an integer index"),
            ("fn relay(r: &mut int32) { let child = &mut *r; bump(child); }",
             "&mut *r", "a mutable reborrow requires a direct"),
            ("fn relay(r: &int32) { bump(&mut *r); }",
             "&mut *r", "expected a mutable reference"),
            ("fn main() { let mut int32 n = 6; let p = ptr_mut(&mut n); bump(&mut *p); }",
             "&mut *p", "expected a mutable reference"),
            ("fn relay(r: &mut int32) { inspect(&mut *r, &*r); }",
             "&*r", "already mutably borrowed"),
            ("fn relay(r: &mut Pair) { r.replace(&mut *r); }",
             "&mut *r", "already mutably borrowed"),
            ("fn relay(r: &mut int32) { both(r, &mut *r); }",
             "&mut *r", "already mutably borrowed"),
            ("fn main() { let []int32 items = [6]; bump(&mut items[0]); }",
             "&mut items[0]", "requires a mutable owner"),
        ):
            with self.subTest(function=function):
                source = header + function
                if "fn main()" not in function:
                    source += "\nfn main() {}"
                self.source.write_text(source)
                report = json.loads(self.execute(self.cli, "check", self.source, "--format", "json", success=False))
                diagnostic, = report["diagnostics"]
                self.assertEqual(diagnostic["phase"], "type")
                self.assertIn(message, diagnostic["message"])
                span = diagnostic["span"]
                self.assertEqual(source.encode()[span["start"]:span["end"]].decode(), place)
                for mode in ("run", "build"):
                    self.execute(self.cli, mode, self.source, success=False, error=message)

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
