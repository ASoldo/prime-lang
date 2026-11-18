#!/usr/bin/env python3
"""Validate one classic ESP32 no_std_parity run on UART or in a raw capture."""

import argparse
from contextlib import contextmanager
import math
import os
import re
import select
import stat
import sys
import termios
import time


EXPECTED = (
    "no_std parity demo start",
    "defer probe start guard=0",
    "defer ran guard=1",
    "async result parity joined",
    "async try parity ok 7",
    "match some 3",
    "timeout parity join 11",
    "await_timeout parity err timeout",
    "await_cancel parity err cancelled",
    "await_cancel_timeout parity err timeout",
    "no_std parity demo done",
)
PROBE_PREFIXES = ("no_std parity", "defer ", "async ", "match ", "timeout parity", "await_")
FAULT = re.compile(
    r"guru meditation|\bpanic(?:ked|ed|['’]d)?\b|\brebooting\b|brownout|"
    r"\bwdt reset\b|\bwatchdog (?:got triggered|timeout|reset)|"
    r"\bassert(?:ion)? failed|\babort\(\) was called", re.I
)
BOOT_MARKERS = (
    re.compile(r"\bets\s+[A-Z][a-z]{2}\s+\d"),  # Classic ESP32 ROM banner
    re.compile(r"\brst:\s*(0x[0-9a-f]+)", re.I),
    re.compile(r"\[rt\] reset_reason=(0|[1-9]\d{0,9})\b"),
)
# Raw RTC reset codes for classic ESP32 watchdogs, intrusion and brownout.
FAULT_REASONS = {4, 7, 8, 9, 10, 11, 13, 15, 16}
RT_STATS = re.compile(r"\[rt\] pc=\d+ sc=\d+ ec=\d+ ca=\d+ cf=\d+ ta=\d+ tf=\d+ sw=\d+ shw=\d+ off=\d+")


class InvalidRun(ValueError):
    pass


class Transcript:
    def __init__(self):
        self.position = 0
        self.boot_stage = 0
        self.partial = b""

    def feed(self, chunk):
        *lines, self.partial = (self.partial + chunk).split(b"\n")
        if any(len(raw) > 4096 for raw in lines) or len(self.partial) > 4096:
            raise InvalidRun("over 4096 UART bytes without a newline")
        for raw in lines:
            self.line(raw.removesuffix(b"\r").decode("ascii", "backslashreplace"))

    def line(self, line):
        if FAULT.search(line):
            raise InvalidRun(f"board fault: {line!r}")
        for stage, marker in enumerate(BOOT_MARKERS, 1):
            match = marker.search(line)
            if match:
                reason = int(match[1], 0) if match.lastindex else None
                if self.position or stage <= self.boot_stage or reason in FAULT_REASONS:
                    raise InvalidRun(f"unexpected reset: {line!r}")
                self.boot_stage = stage
                return
        if not line or RT_STATS.fullmatch(line):
            return
        if not self.position and not line.startswith(PROBE_PREFIXES):
            return  # Initial ROM and bootloader chatter varies with the board.
        if self.position == len(EXPECTED):
            raise InvalidRun(f"unexpected output after done: {line!r}")
        expected = EXPECTED[self.position]
        if line != expected:
            raise InvalidRun(f"expected {expected!r}, received {line!r}")
        self.position += 1

    def finish(self, reason):
        if self.partial:
            raise InvalidRun(f"{reason} in an unterminated UART line: {self.partial[:120]!r}")
        if self.position < len(EXPECTED):
            raise InvalidRun(f"{reason} waiting for {EXPECTED[self.position]!r}")


@contextmanager
def input_fd(path, live):
    fd = os.open(path, os.O_RDONLY | os.O_NOCTTY | os.O_NONBLOCK)
    try:
        if not live:
            if not stat.S_ISREG(os.fstat(fd).st_mode):
                raise InvalidRun("--log requires a regular file")
            yield fd
            return
        saved = termios.tcgetattr(fd)
        cc = saved[6].copy()
        cc[termios.VMIN], cc[termios.VTIME] = 1, 0
        # Raw 115200 8N1, local receiver, no flow control or modem-line commands.
        attrs = [0, 0, termios.CLOCAL | termios.CREAD | termios.CS8, 0,
                 termios.B115200, termios.B115200, cc]
        termios.tcsetattr(fd, termios.TCSAFLUSH, attrs)
        try:
            yield fd
        finally:
            termios.tcsetattr(fd, termios.TCSANOW, saved)
    finally:
        os.close(fd)


def check(fd, live, timeout):
    run = Transcript()
    deadline = time.monotonic() + timeout
    if live:
        print("UART ready at 115200 8N1. Reset the board once now.", file=sys.stderr, flush=True)
    while True:
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            if not live:
                raise InvalidRun("deadline before end of log")
            run.finish("deadline")
            return
        if live and not select.select([fd], [], [], min(remaining, 60))[0]:
            continue
        chunk = os.read(fd, 4096)
        if not chunk:
            if live:
                raise InvalidRun("UART disconnected before the observation window ended")
            run.finish("end of log")
            return
        run.feed(chunk)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    source = parser.add_mutually_exclusive_group(required=True)
    source.add_argument("--port", metavar="TTY", help="listen at 115200 baud until the deadline")
    source.add_argument("--log", metavar="FILE", help="replay a saved raw UART log through EOF")
    parser.add_argument("--timeout", type=float, default=15.0, metavar="SECONDS",
                        help="total observation or replay deadline in seconds (default 15)")
    args = parser.parse_args()
    if not math.isfinite(args.timeout) or args.timeout <= 0:
        parser.error("--timeout must be a positive finite number")
    live = args.port is not None
    try:
        with input_fd(args.port if live else args.log, live) as fd:
            check(fd, live, args.timeout)
    except (InvalidRun, OSError, termios.error) as error:
        print(f"FAIL: {error}", file=sys.stderr)
        return 1
    print(f"PASS: {'UART' if live else 'log'} matched all {len(EXPECTED)} parity lines")
    return 0


if __name__ == "__main__":
    sys.exit(main())
