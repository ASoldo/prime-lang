import os
from pathlib import Path
import pty
import select
import subprocess
import sys
import tempfile
import unittest


HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))
from check_esp32_parity import InvalidRun, Transcript


CLI = [sys.executable, str(HERE.parent / "check_esp32_parity.py")]
GOOD = (HERE / "fixtures/no_std_parity_expected.uart").read_bytes()
LINES = GOOD.splitlines(keepends=True)
BOOT = b"ets Jun  8 2016 00:22:57\r\n\r\nrst:0x1 (POWERON_RESET),boot:0x13 (SPI_FAST_FLASH_BOOT)\r\n"


class BoardAcceptance(unittest.TestCase):
    def replay(self, contents):
        with tempfile.NamedTemporaryFile() as log:
            log.write(contents)
            log.flush()
            return subprocess.run([*CLI, "--log", log.name, "--timeout", "3"],
                                  capture_output=True, timeout=5)

    def rejected(self, contents, diagnostic):
        run = Transcript()
        with self.assertRaises(InvalidRun) as failure:
            for i in range(0, len(contents), 9):
                run.feed(contents[i:i+9])
            run.finish("end of log")
        self.assertIn(diagnostic.decode(), str(failure.exception))

    def test_log(self):
        for log in (GOOD, BOOT + b"entry 0x40080664\r\n" + GOOD.replace(b"\n", b"\r\n")):
            result = self.replay(log)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn(b"PASS: log", result.stdout)

    def test_c825d5f_qemu_log(self):
        log = (HERE / "fixtures/no_std_parity_c825d5f_qemu.uart").read_bytes()
        result = self.replay(log)
        self.assertEqual(result.returncode, 1)
        self.assertIn(b"expected 'await_timeout parity err timeout'", result.stderr)
        self.assertIn(b"received 'await_timeout parity err 2000505063154167820296574927073'", result.stderr)
        self.assertNotIn(b"PASS", result.stdout)

    def test_each_line_required_once_in_order(self):
        for index, line in enumerate(LINES):
            with self.subTest(line=line):
                self.rejected(GOOD.replace(line, b""), line.rstrip())
                self.rejected(GOOD.replace(line, line * 2), line.rstrip())
        self.rejected(GOOD.replace(LINES[5] + LINES[6], LINES[6] + LINES[5]), LINES[5].rstrip())

    def test_payloads_and_branches_are_exact(self):
        for index, wrong in [(1, b"defer probe start guard=1"), (2, b"defer ran guard=0"),
                             (4, b"async try parity err empty"), (4, b"async try parity ok 70"),
                             (5, b"match some 30"), (6, b"timeout parity join 110"),
                             (7, b"await_timeout parity err 0"), (8, b"await_cancel parity err timeout"),
                             (9, b"await_cancel_timeout parity unexpected")]:
            with self.subTest(wrong=wrong):
                self.rejected(GOOD.replace(LINES[index], wrong + b"\n" + LINES[index]), wrong)

    def test_resets_panics_and_truncation(self):
        self.rejected(BOOT * 2 + GOOD, b"unexpected reset")
        self.rejected(b"rst:0x8 (TG1WDT_SYS_RESET)\n" + GOOD, b"unexpected reset")
        self.rejected(b"[rt] reset_reason=15\n" + GOOD, b"unexpected reset")
        self.rejected(b"Guru Meditation Error: Core 0 panic'ed\n" + GOOD, b"board fault")
        self.rejected(LINES[0] + b"[rt] reset_reason=1\n" + GOOD, b"unexpected reset")
        self.rejected(GOOD + b"Rebooting...\n", b"board fault")
        self.rejected(GOOD + BOOT, b"unexpected reset")
        self.rejected(GOOD + b"Guru Meditation Error", b"unterminated")
        self.rejected(GOOD.rstrip(b"\n"), b"unterminated")
        self.rejected(b"", b"end of log")

    def serial_result(self, chunks, timeout="0.4", late=b""):
        master, slave = pty.openpty()
        try:
            with subprocess.Popen([*CLI, "--port", os.ttyname(slave), "--timeout", timeout],
                                  stdout=subprocess.PIPE, stderr=subprocess.PIPE) as proc:
                try:
                    self.assertTrue(select.select([proc.stderr], [], [], 3)[0], "UART setup stalled")
                    ready = proc.stderr.readline()
                    self.assertIn(b"UART ready", ready)
                    for chunk in chunks:
                        os.write(master, chunk)
                    if late:
                        with self.assertRaises(subprocess.TimeoutExpired):
                            proc.wait(timeout=0.1)
                        os.write(master, late)
                    out, err = proc.communicate(timeout=3)
                    return proc.returncode, out, err
                finally:
                    proc.kill()
        finally:
            os.close(slave)
            os.close(master)

    def test_uart(self):
        for trace, code, diagnostic in [(GOOD, 0, b"PASS: UART"), (b"", 1, b"deadline"),
                                        (GOOD[:-10], 1, b"unterminated"),
                                        (GOOD.replace(b"join 11", b"join 12"), 1, b"join 12")]:
            with self.subTest(expected=diagnostic):
                status, out, err = self.serial_result([trace[:9], trace[9:]])
                self.assertEqual(status, code, err)
                self.assertIn(diagnostic, out + err)

    def test_uart_reset_after_done(self):
        status, _, err = self.serial_result([GOOD], timeout="1.5", late=BOOT)
        self.assertEqual(status, 1)
        self.assertIn(b"unexpected reset", err)

    def test_bad_deadlines(self):
        for timeout in ["0", "-1", "inf", "nan"]:
            with self.subTest(timeout=timeout):
                result = subprocess.run([*CLI, "--log", "/unused", "--timeout", timeout],
                                        capture_output=True, timeout=3)
                self.assertEqual(result.returncode, 2)
                self.assertIn(b"positive finite", result.stderr)


if __name__ == "__main__":
    unittest.main()
