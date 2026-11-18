import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import time
import unittest


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/check_examples.sh"


class LintCLI(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.source = self.root / "demo.prime"
        self.manifest = self.root / "prime.toml"
        self.manifest.write_text('[module]\nname = "demo"\npath = "demo.prime"\n')
        self.cli = os.environ.get("PRIME_BIN", str(ROOT / "target/debug/prime-lang"))
        if os.sep in self.cli:
            self.cli = str(Path(self.cli).resolve())

    def check_lint(self, code, diagnostic):
        result = subprocess.run([self.cli, "lint", self.source.name], cwd=self.root,
                                capture_output=True, text=True, timeout=15)
        self.assertEqual(result.returncode, code, result.stderr)
        self.assertIn(diagnostic, result.stdout if diagnostic == "no diagnostics" else result.stderr)

    def test_one_shot_status_and_diagnostics(self):
        for source, code, message in [
            ("module demo;", 0, "no diagnostics"),
            ("module other;", 0, "manifest maps this file"),
            ("module demo; import demo; import demo;", 0, "Duplicate import"),
            ("module demo; §", 1, "Lexing error"),
            ("module demo", 1, "failed to parse module"),
            ("module demo; import absent;", 1, "Cannot resolve import"),
            ("library demo;", 1, "Header declares"),
        ]:
            with self.subTest(source=source):
                self.source.write_text(source)
                self.check_lint(code, message)

    def test_invalid_manifest_and_unreadable_input(self):
        self.source.write_text("module demo;")
        self.manifest.write_text("[module")
        self.check_lint(1, "prime.toml")
        self.source.unlink()
        self.check_lint(1, "demo.prime")

    def test_watch_recovers_across_replacement(self):
        self.source.write_text("module demo; §")
        log = self.root / "watch.log"
        with log.open("w") as output:
            proc = subprocess.Popen([self.cli, "lint", self.source.name, "--watch"],
                                    cwd=self.root, stdout=output, stderr=subprocess.STDOUT)
            try:
                def wait_for(message):
                    deadline = time.monotonic() + 10
                    while time.monotonic() < deadline:
                        text = log.read_text()
                        self.assertIsNone(proc.poll(), text)
                        if message in text:
                            return
                        time.sleep(0.05)
                    self.fail(f"Timed out waiting for {message!r}: {log.read_text()}")

                wait_for("Watching ")
                self.assertIn("Lexing error", log.read_text())
                replacement = self.root / "replacement"
                replacement.write_text("module demo; import absent;")
                replacement.replace(self.source)
                wait_for("Cannot resolve import")
                replacement.write_text("module demo;")
                replacement.replace(self.source)
                wait_for("no diagnostics")
            finally:
                proc.kill()
                proc.wait(timeout=5)


class ExampleSweep(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.script = self.root / "scripts/check_examples.sh"
        self.script.parent.mkdir()
        shutil.copy2(SCRIPT, self.script)
        self.package = self.root / "workspace" / "demo"
        self.package.mkdir(parents=True)
        self.manifest = self.package / "prime.toml"
        self.source = self.package / "source with spaces.prime"
        self.source.touch()
        self.trace = self.root / "calls"
        self.cli = self.root / "target/debug/prime-lang"
        self.cli.parent.mkdir(parents=True)
        self.cli.write_text(
            '#!/bin/sh\nprintf "%s\\0" "$@" >> "$TRACE"\n'
            '[ "$1" != "$FAIL_COMMAND" ] || exit 19\n'
        )
        self.cli.chmod(0o755)

    def sweep(self, **overrides):
        env = os.environ.copy()
        for key in ("PRIME_BIN", "CARGO_TARGET_DIR", "PRIME_RUN_EXAMPLES", "FAIL_COMMAND"):
            env.pop(key, None)
        env.update(TRACE=str(self.trace), **overrides)
        return subprocess.run([self.script], env=env, capture_output=True, text=True,
                              cwd=self.package, timeout=15)

    def test_toml_paths_are_resolved_and_deduplicated(self):
        self.manifest.write_text("""
[module]
name = "entry"
path = 'source with spaces.prime' # path = "misleading.prime"
[modules]
alias = './source with spaces.prime'
[libraries]
items = [{name = "library", path = "library.prime"}]
[dependencies]
dep = {path = "not-a-source"}
[[tests]]
name = "checks"
path = 'checks.prime'
""")
        library = self.package / "library.prime"
        tests = self.package / "checks.prime"
        library.touch()
        tests.touch()
        sibling = self.package.parent / "sibling"
        sibling.mkdir()
        (sibling / "prime.toml").write_text("[libraries.shared]\npath='../demo/library.prime'\n")
        result = self.sweep(PRIME_RUN_EXAMPLES="1")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("3 example modules from 2 manifests", result.stdout)
        args = self.trace.read_bytes().split(b"\0")[:-1]
        calls = list(zip(args[::2], args[1::2], strict=True))
        self.assertCountEqual(calls, [(command, os.fsencode(file)) for command in (b"lint", b"check", b"run")
                                      for file in (self.source, library, tests)])

    def test_empty_invalid_and_missing_inputs_fail_before_lint(self):
        for manifest in (None, "[package]\nname='empty'", "[broken", "[module]\npath=42",
                         "[tests]\nitems='bad'", "[modules]\nx='missing.prime'"):
            with self.subTest(manifest=manifest):
                if manifest is not None:
                    self.manifest.write_text(manifest)
                result = self.sweep()
                self.assertNotEqual(result.returncode, 0)
                self.assertTrue(result.stderr)
                self.assertFalse(self.trace.exists())
                self.assertNotIn("successfully", result.stdout)

    def test_cli_selection_and_failure_propagation(self):
        self.manifest.write_text("[modules]\ndemo='source with spaces.prime'")
        for command in ("lint", "check", "run"):
            result = self.sweep(FAIL_COMMAND=command, PRIME_RUN_EXAMPLES="1")
            self.assertEqual(result.returncode, 19, result.stderr)
            self.assertNotIn("successfully", result.stdout)
        self.assertNotEqual(self.sweep(PRIME_BIN="./missing").returncode, 0)
        moved = self.package / "explicit cli"
        self.cli.rename(moved)
        result = self.sweep(PRIME_BIN="./explicit cli")
        self.assertEqual(result.returncode, 0, result.stderr)
        release = self.root / "custom-target/release/prime-lang"
        release.parent.mkdir(parents=True)
        moved.rename(release)
        result = self.sweep(CARGO_TARGET_DIR="custom-target")
        self.assertEqual(result.returncode, 0, result.stderr)
        result = self.sweep(PATH=str(release.parent) + os.pathsep + os.environ["PATH"])
        self.assertEqual(result.returncode, 0, result.stderr)


if __name__ == "__main__":
    unittest.main()
