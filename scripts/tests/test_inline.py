import json
import os
from pathlib import Path
import pty
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
BIN = Path(os.environ.get("PRIME_BIN", ROOT / "target/debug/prime-lang")).resolve()


class InlineCLI(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.env = dict(os.environ, PRIME_BIN=str(BIN), CARGO_TERM_COLOR="never")
        for name in ("PRIME_TARGET", "PRIME_TEST_INPUTS", "PRIME_TEST_INPUTS_FILE"):
            self.env.pop(name, None)

    def invoke(self, args, source="", status=0):
        result = subprocess.run(
            [BIN, *args], cwd=self.root, env=self.env,
            input=source.encode() if isinstance(source, str) else source,
            capture_output=True, timeout=300,
        )
        self.assertEqual(result.returncode, status, (result.stdout, result.stderr))
        return result

    def check(self, source, status=0, *options):
        result = self.invoke(["check", "-", "--format=json", *options], source, status)
        self.assertEqual(result.stderr, b"")
        report = json.loads(result.stdout)
        self.assertEqual(report["schema_version"], 1)
        self.assertEqual(report["target"], "-")
        return report

    def test_shell_heredoc_and_pipe(self):
        for command in ('"$PRIME_BIN"', '"$PRIME_BIN" -', '"$PRIME_BIN" run -'):
            result = subprocess.run(
                ["sh", "-c", command + " <<'PRIME'\nout(6 * 7);\nPRIME\n"],
                cwd=self.root, env=self.env, capture_output=True, timeout=15,
            )
            self.assertEqual((result.returncode, result.stdout, result.stderr), (0, b"42\n", b""))
        result = subprocess.run(
            ["sh", "-c", """printf '%s\\n' 'module demo; fn main() { out(42); }' | "$PRIME_BIN" """],
            cwd=self.root, env=self.env, capture_output=True, timeout=15,
        )
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, b"42\n", b""))
        self.assertEqual(list(self.root.iterdir()), [])

    def test_trace_uses_original_module_and_source(self):
        source = "module demo;\nfn twice(x: int32) -> int32 { x + x }\nfn main() { out(twice(21)); }\n"
        result = self.invoke(["run", "--trace", "-"], source)
        self.assertEqual(result.stdout, b"42\n")
        records = [json.loads(line) for line in result.stderr.splitlines()]
        self.assertEqual([rec["function"] for rec in records], ["main", "twice"])
        for record in records:
            self.assertEqual((record["module"], record["path"]), ("demo", "<stdin>"))
            name = source.encode()[record["start"]:record["end"]].decode()
            self.assertEqual(name, record["function"])

    def test_no_arguments_on_terminal(self):
        master, slave = pty.openpty()
        try:
            result = subprocess.run([BIN], stdin=slave, cwd=self.root, env=self.env,
                                    capture_output=True, timeout=10)
        finally:
            os.close(master)
            os.close(slave)
        self.assertIn(b"Usage:", result.stdout + result.stderr)

    def test_same_imported_script_interpreted_checked_and_built(self):
        helper = self.root / "helper.prime"
        helper.write_text("library helper; pub fn twice(n: int32) -> int32 { n + n }")
        source = "import helper::{twice};\n// café\nlet n = 21;\nout(twice(n));\n"
        result = self.check(source)
        self.assertEqual(result["paths"], sorted(["<stdin>", str(helper.resolve())]))
        self.assertEqual(result["entry_path"], "<stdin>")
        self.assertEqual(result["diagnostics"], [])
        interpreted = self.invoke(["run", "-"], source)
        self.assertEqual((interpreted.stdout, interpreted.stderr), (b"42\n", b""))
        self.assertEqual(list(self.root.iterdir()), [helper])
        self.invoke(["build", "-", "--name", "answer"], source)
        native = subprocess.run([self.root / ".build.prime/answer/answer"], cwd=self.root,
                                capture_output=True, timeout=15)
        self.assertEqual((native.returncode, native.stdout, native.stderr), (0, b"42\n", b""))

        (self.root / "saved.prime").write_text(source)
        self.assertEqual(self.invoke(["run", "saved.prime"]).stdout, b"42\n")
        self.assertEqual(self.invoke(["fmt", "saved.prime"]).stdout.decode().count("fn main"), 0)

    def test_errors_keep_original_byte_offsets_and_streams(self):
        for source, offending, phase in [
            ('// café ☕\nout(missing);\n', "missing", "type"),
            ('out("é");\nout(6 * );\n', ")", "syntax"),
            ('// café\nfn main( { out(42); }', "{", "syntax"),
            ('module demo;\nout(42);', "out", "syntax"),
            ('out(42);\nfn main() {}', "fn", "syntax"),
            ('module demo;\nfn main() { ~nonexistent(); }', "~nonexistent()", "macro"),
        ]:
            with self.subTest(source=source):
                diag = self.check(source, 1)["diagnostics"][0]
                self.assertEqual((diag["phase"], diag["path"]), (phase, "<stdin>"))
                span = diag["span"]
                self.assertEqual(source.encode()[span["start"]:span["end"]].decode(), offending)
                for args in (["run", "-"], ["build", "-", "--name", "bad"]):
                    result = self.invoke(args, source, 1)
                    self.assertEqual(result.stdout, b"")
                    self.assertIn(b"<stdin>", result.stderr)
                    self.assertIn(offending.encode(), result.stderr)
        self.assertEqual(self.check(b"out(\xff);", 1)["diagnostics"][0]["path"], "<stdin>")
        self.assertFalse((self.root / ".build.prime").exists())

    def test_frozen_selected_package_and_relative_import(self):
        member = self.root / "app"
        member.mkdir()
        (self.root / "prime.toml").write_text('[workspace]\nmembers = ["app"]\n')
        manifest = member / "prime.toml"
        manifest.write_text('[package]\nname = "sample"\n')
        (member / "helper.prime").write_text("library helper; pub fn answer() -> int32 { 42 }")
        source = "import helper::{answer}; out(answer());"
        opts = ["--project", "sample", "--frozen"]
        self.assertEqual(self.check(source, 1, *opts)["diagnostics"][0]["phase"], "manifest")
        self.invoke(["run", "-", *opts], source, 1)
        self.invoke(["build", "-", "--name", "sample", *opts], source, 1)
        self.assertIn("not found", self.check(source, 1, "-p", "missing")["diagnostics"][0]["message"])
        self.invoke(["sync", str(member)])
        self.assertEqual(self.check(source, 0, *opts)["manifest_path"], str(manifest.resolve()))
        self.assertEqual(self.invoke(["run", "-", *opts], source).stdout, b"42\n")
        self.root = member
        self.assertEqual(self.invoke(["run", "-", "--frozen"], source).stdout, b"42\n")

    def test_application_input_and_eof(self):
        source = """module reader;
enum Result[T, E] { Ok(T), Err(E) }
fn main() {
  match in[string]("") {
    Result::Ok(line) => out(line),
    Result::Err(message) => out(message),
  }
}
"""
        self.assertEqual(self.invoke(["run", "-"], source).stdout, b"end of input\n")
        (self.root / "reader.prime").write_text(source)
        self.assertEqual(self.invoke(["run", "reader.prime"], "data\n").stdout, b"data\n")
        self.assertEqual(self.invoke(["run", "reader.prime"], "\n").stdout, b"\n")
        self.invoke(["build", "-", "--name", "reader"], source)
        for raw, expected in [(b"data\n", b"data\n"), (b"", b"end of input\n"), (b"\n", b"\n")]:
            native = subprocess.run([self.root / ".build.prime/reader/reader"], cwd=self.root,
                                    input=raw, capture_output=True, timeout=15)
            self.assertEqual((native.returncode, native.stdout, native.stderr), (0, expected, b""))
