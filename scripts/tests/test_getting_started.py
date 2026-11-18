"""Execute the guide's first Prime program in a fresh consumer package."""
import os
from pathlib import Path
import re
import shlex
import shutil
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

    def test_heredoc_and_saved_source(self):
        cli = shutil.which(self.cli)
        self.assertIsNotNone(cli, self.cli)
        (self.scratch / "prime-lang").symlink_to(cli)
        self.env["PATH"] = str(self.scratch) + os.pathsep + self.env["PATH"]
        for guide in (ROOT / "README.md", ROOT / "docs/GettingStarted.md"):
            with self.subTest(guide=guide.name):
                work = self.scratch / guide.stem
                work.mkdir()
                section = guide.read_text().split("## First run\n", 1)[1].split("\n## ", 1)[0]
                interpret, build = re.findall(r"^```sh\n([\s\S]*?)\n```$", section, re.MULTILINE)
                self.assertEqual(self.execute("sh", "-ec", interpret, cwd=work), "42\n")
                self.assertEqual(self.execute("sh", "-ec", build, cwd=work).splitlines()[-1], "42")
                lines = interpret.splitlines()
                self.assertEqual((work / "answer.prime").read_text(), "\n".join(lines[1:-1]) + "\n")

    def test_offline_text_program(self):
        (self.scratch / "prime-lang").symlink_to(shutil.which(self.cli))
        self.env["PATH"] = str(self.scratch) + os.pathsep + self.env["PATH"]
        text = (ROOT / "docs/GettingStarted.md").read_text()
        section = text.split("## Build a text program offline\n", 1)[1].split("\n## ", 1)[0]
        command, = re.findall(r"^```sh\n([\s\S]*?)\n```$", section, re.MULTILINE)
        self.assertEqual(self.execute("sh", "-ec", command, cwd=self.scratch).splitlines()[-1], "hello")

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
