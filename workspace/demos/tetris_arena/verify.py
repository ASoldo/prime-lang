"""Run from any directory with paths to prime-lang and the compiled arena."""
import hashlib
import os
from pathlib import Path
import subprocess
import sys
import time

DEMO = Path(__file__).resolve().parent
GOLDEN = {
    "chain": "342a6835b8b67836857de631dee88c6a1cf38489cdf64df679b42a0bb4904b63",
    "99 bots": "1eddcd39b88787568943c4dcb0684e1696138d4ed27ac2970ebf4652aac650e6",
}


def move(tick, seat, action):
    return 10000 * tick + (seat + 1) * 10 + action


def chain():
    words = [137, 2]
    actions = [1, 1, 1, 1, 4, 4, 2, 2, 2, 2, 4]
    for tick in range(1, 74):
        if tick >= 63:
            words.append(move(tick, 0, actions[tick - 63]))
        words.append(10000 * tick)
        if tick == 72:
            words.append(-2)
    return words + [-2, -1]


def bots(reverse=False):
    # Every seat acts and locks a piece. Seat zero keeps dropping in the
    # same column while the other bots explore movement and rotation.
    words = [137, 99, -2]
    order = range(98, -1, -1) if reverse else range(99)
    for tick in range(1, 16):
        for seat in order:
            action = (seat + tick) % 4
            if seat == 0 or tick == 3:
                action = 4
            words.append(move(tick, seat, action))
        words.append(10000 * tick)
    return words + [-2, -1]


def run(command, words):
    env = os.environ.copy()
    for key in ("PRIME_TEST_INPUTS", "PRIME_TEST_INPUTS_FILE"):
        env.pop(key, None)
    replay = "".join(f"{word}\n" for word in words)
    started = time.monotonic()
    result = subprocess.run(command, input=replay, stdout=subprocess.PIPE,
                            text=True, check=True, timeout=300, env=env)
    return result.stdout, time.monotonic() - started


def states(text, tick):
    return [line.split() for line in text.splitlines() if line.startswith(f"V2 {tick} ")]


def verify_chain(text):
    assert "G2 73 0 1 1\nX2 73 1 2\nT2 73 1 0\n" in text
    winner, loser = states(text, 73)
    assert winner[7:10] == ["1", "100", "1"]
    assert winner[13:] == ["0" * 190 + "6000000007", "1", "14", "15", "24", "25"]
    assert loser[13:] == ["0" * 190 + "8088888888", "2", "-1", "-1", "-1", "-1"]


def verify_bots(text):
    initial, final = states(text, 0), states(text, 15)
    assert [int(s[2]) for s in initial] == list(range(99))
    assert [int(s[2]) for s in final] == list(range(99))
    assert all(s[9] == "1" and s[13] == "0" * 200 for s in initial)
    assert all(len(s[13]) == 200 and s[13] != "0" * 200 for s in final)
    assert len({(s[3], s[4], s[5], s[13]) for s in final}) >= 4
    assert final[0][9] == "0" and final[0][14] == "99"
    assert "T2 15 98 -1\n" in text


def main():
    if len(sys.argv) == 2 and sys.argv[1] in ("--chain", "--bots"):
        print(*({"--chain": chain, "--bots": bots}[sys.argv[1]])(), sep="\n")
        return
    if len(sys.argv) != 3:
        sys.exit("usage: verify.py PRIME_LANG NATIVE_ARENA | --chain | --bots")
    commands = [[sys.argv[1], "run", DEMO / "main.prime"], [sys.argv[2]]]
    for label, words, verify in [("chain", chain(), verify_chain), ("99 bots", bots(), verify_bots)]:
        (interp, itime), (native, ntime) = [run(cmd, words) for cmd in commands]
        assert interp == native, f"{label}: backend divergence"
        verify(native)
        digest = hashlib.sha256(native.encode()).hexdigest()
        assert digest == GOLDEN[label], f"{label}: replay digest {digest}"
        print(f"{label}: sha256 {digest}, "
              f"interpreter {itime:.3f}s, native {ntime:.3f}s", flush=True)
    reordered, _ = run(commands[1], bots(reverse=True))
    assert states(native, 15) == states(reordered, 15)
    # Duplicate, future tick, invalid id/action and stale advance leave state intact.
    for cmd in commands:
        clean, _ = run(cmd, [137, 2, 10013, 10000, -2, -1])
        noisy, _ = run(cmd, [137, 2, 10013, 10014, 20000, 10030, 10025, 10000, 10000, -2, -1])
        assert noisy.splitlines().count("E2 0") == 4 and noisy.splitlines().count("E2 1") == 1
        assert states(clean, 1) == states(noisy, 1)


if __name__ == "__main__":
    main()
