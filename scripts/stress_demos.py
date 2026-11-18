#!/usr/bin/env python3
"""Build each host demo once, then compare isolated interpreter and native runs."""
import argparse
import difflib
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]
FS_FILE = Path("workspace/demos/fs_demo/tmp_demo.txt")

# Each marker tests an observable property in addition to comparing the full output.
DEMOS = {
    "borrow": ("demos/borrow/borrow_demo.prime", ["loop_merging => 14", "after block gauge 13"]),
    "borrow_scopes": ("demos/borrow_checker_errors/borrow_error_demo.prime", ["gauge stabilized at 24"]),
    "channel": ("demos/channel_demo/channel_demo.prime", ["6"]),
    "patterns": ("demos/patterns/pattern_demo.prime", ["snapshot sums -> 140", "struct mutation total 105"]),
    "generic": ("demos/generic/generic_demo.prime", ["hp identity => 120", "pair => (18, bleed)"]),
    "heap": ("demos/heap_features/heap_features.prime", ["moved box value -> 72", "- Northwatch"]),
    "drop": ("demos/drop_demo/drop_demo.prime", ["  0: second", "  1: defer", "  2: first"]),
    "closures": ("demos/closures/closure_demo.prime", ["higher-order closure on 4 = 9", "nested closure result = 16", "defer inside closure ran:"]),
    "fs": ("demos/fs_demo/fs_demo.prime", ["write ok", "exists? true", "read: demo file contents"]),
    "input": ("tests/build_input_demo/build_input_demo.prime", []),
}


def execute(command, cwd, env, data=b"", timeout=30):
    proc = subprocess.run(command, input=data, cwd=cwd, env=env, capture_output=True, timeout=timeout)
    if proc.returncode:
        raise ValueError(f"{command} exited {proc.returncode}\n" + (proc.stdout + proc.stderr).decode(errors="replace"))
    return proc.stdout


def compare(expected, actual, label):
    if expected != actual:
        diff = difflib.unified_diff(expected.decode(errors="replace").splitlines(True),
                                    actual.decode(errors="replace").splitlines(True),
                                    fromfile="expected", tofile=label)
        raise ValueError("".join(diff))


def sample_input(index):
    age = ("bad", str(19 * index), "-2147483648")[index % 3]
    temp = (str(-7 * index), "2147483647", "invalid")[index % 3]

    def response(name, number):
        try:
            return f"{name} recorded: {int(number)}"
        except ValueError:
            return f"{name} error: invalid integer input"

    expected = f"Enter age: {response('age', age)}\nTemperature for Soldo: {response('temp', temp)}\n"
    return f"{age}\n{temp}\n".encode(), expected.encode()


def exercise(name, cli, scratch, env, rounds):
    source, markers = DEMOS[name]
    entry = ROOT / "workspace" / source
    build = scratch / name
    (build / FS_FILE.parent).mkdir(parents=True)
    execute([cli, "build", entry, "--name", name], build, env, timeout=600)
    if (build / FS_FILE).exists():
        raise ValueError("compiling the demo wrote its application data")
    binary = build / ".build.prime" / name / name
    for index in range(rounds):
        data, expected = sample_input(index) if name == "input" else (b"", None)
        for mode, cmd in (("interpreter", [cli, "run", entry]), ("native", [binary])):
            with tempfile.TemporaryDirectory(dir=build) as directory:
                work = Path(directory)
                if name == "fs":
                    (work / FS_FILE.parent).mkdir(parents=True)
                actual = execute(cmd, work, env, data)
                lines = actual.decode().splitlines()
                for marker in markers:
                    if marker not in lines:
                        raise ValueError(f"{mode} missing {marker!r}\n{actual!r}")
                if name == "fs":
                    compare(b"demo file contents", (work / FS_FILE).read_bytes(), mode)
                if expected is None:
                    expected = actual
                else:
                    compare(expected, actual, mode)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rounds", type=int, default=3)
    parser.add_argument("--demo", action="append", choices=DEMOS, help="repeat to select demos")
    args = parser.parse_args()
    if args.rounds < 1:
        parser.error("--rounds must be positive")
    env = os.environ.copy()
    for setting in ("PRIME_TEST_INPUTS", "PRIME_TEST_INPUTS_FILE"):
        env.pop(setting, None)
    target = Path(env.get("CARGO_TARGET_DIR", ROOT / "target")).resolve()
    env["CARGO_TARGET_DIR"] = str(target)
    cli = shutil.which(env.get("PRIME_BIN", str(target / "debug/prime-lang")))
    if not cli:
        parser.error("build prime-lang first or set PRIME_BIN")
    cli = Path(cli).resolve()
    if "RUSTUP_TOOLCHAIN" not in env and shutil.which("rustup"):
        env["RUSTUP_TOOLCHAIN"] = subprocess.check_output(
            ["rustup", "show", "active-toolchain"], cwd=ROOT, text=True).split()[0]
    demos = args.demo or DEMOS
    with tempfile.TemporaryDirectory(prefix="prime-demos-") as directory:
        for name in demos:
            try:
                exercise(name, cli, Path(directory), env, args.rounds)
            except (ValueError, OSError, subprocess.TimeoutExpired) as exc:
                sys.exit(f"{name} failed: {exc}")
            print(f"{name}: {args.rounds} interpreter/native pairs", flush=True)


if __name__ == "__main__":
    main()
