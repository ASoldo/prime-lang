"""Execute the guide's first Prime program in a fresh consumer package."""
import os
from pathlib import Path
import shlex
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]


class GettingStarted(unittest.TestCase):
    def setUp(self):
        cli = os.environ.get("PRIME_BIN", str(ROOT / "target/debug/prime-lang"))
        if os.sep in cli:
            cli = str(Path(cli).resolve())
        self.cli = cli
        self.env = os.environ.copy()
        target = Path(self.env.get("CARGO_TARGET_DIR", ROOT / "target")).resolve()
        self.env["CARGO_TARGET_DIR"] = str(target)
        scratch = tempfile.TemporaryDirectory()
        self.addCleanup(scratch.cleanup)
        self.scratch = Path(scratch.name)

    def execute(self, *args, cwd):
        return subprocess.run(args, cwd=cwd, env=self.env, check=True,
                              stdout=subprocess.PIPE, text=True, timeout=300).stdout

    def test_builtin_quick_start(self):
        # Run the three commands as printed by the installed CLI.
        for name in ("workspace", "prime.toml"):
            (self.scratch / name).symlink_to(ROOT / name)
        topic = self.execute(self.cli, "docs", "--query", "quick-start", cwd=ROOT)
        section = topic.split("-- Run and build the sample --", 1)[1]
        commands = section.split("```prime\n", 1)[1].split("\n```", 1)[0].splitlines()
        outputs = []
        for command in commands:
            argv = shlex.split(command)
            if argv[0] == "prime-lang":
                argv[0] = self.cli
            outputs.append(self.execute(*argv, cwd=self.scratch))
        self.assertEqual(len(outputs), 3)
        self.assertEqual(outputs[0], "6\n")
        self.assertEqual(outputs[2], "6\n")

    def test_first_program(self):
        guide = (ROOT / "docs/GettingStarted.md").read_text()
        example = guide.split("```prime\n", 1)[1].split("\n```", 1)[0]
        package = self.scratch / "hello"
        self.execute(self.cli, "new", "hello", "--bin", cwd=self.scratch)
        self.execute(self.cli, "check", "main.prime", cwd=package)
        self.assertEqual(self.execute(self.cli, "run", "main.prime", cwd=package), "Hello from Prime!\n")
        (package / "main.prime").write_text(example + "\n")
        self.assertEqual(self.execute(self.cli, "run", "main.prime", cwd=package), "score = 30\n")
        self.execute(self.cli, "build", "main.prime", "--name", "score", cwd=package)
        self.assertEqual(self.execute("./.build.prime/score/score", cwd=package), "score = 30\n")


if __name__ == "__main__":
    unittest.main()
