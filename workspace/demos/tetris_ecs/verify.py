"""Verify every state in a two-seat replay using the interpreter and host binary."""
import hashlib
import os
from pathlib import Path
import subprocess
import sys

DEMO = Path(__file__).resolve().parent
REPLAY = (DEMO / "clear.replay").read_bytes()
EXPECTED = "c6d9b0f32dfad10175226684027f9907f3c16eddb0d4c4c0cd97ce3b71188f51"


def run(command, replay):
    env = os.environ.copy()
    env.pop("PRIME_TEST_INPUTS", None)
    env.pop("PRIME_TEST_INPUTS_FILE", None)
    return subprocess.run(command, input=replay, stdout=subprocess.PIPE,
                          check=True, timeout=60, env=env).stdout


def verify(command):
    states = run(command, REPLAY)
    lines = states.splitlines()
    if len(lines) != 24 or hashlib.sha256(states).hexdigest() != EXPECTED:
        raise ValueError(f"unexpected replay from {command}: {lines[-2:]!r}")

    # Tick 1 was accepted. A duplicate, gap and bad second-seat action
    # must each preserve both boards and allow exactly the same continuation.
    rejected = REPLAY.replace(b"113\n", b"113\n113\n300\n219\n")
    errors = b"E1 frame 1\n" * 3
    prefix = b"\n".join(lines[:4]) + b"\n"
    if run(command, rejected) != prefix + errors + states[len(prefix):]:
        raise ValueError(f"frame validation changed state in {command}")
    return states


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit("usage: python3 verify.py PRIME_LANG NATIVE_TETRIS_ECS")
    interpreted = verify([sys.argv[1], "run", DEMO / "main.prime"])
    native = verify([sys.argv[2]])
    if interpreted != native:
        sys.exit("interpreter/native divergence")
    print(f"tick 11, 24 identical state records, 3 rejected frames; sha256 {EXPECTED}")
