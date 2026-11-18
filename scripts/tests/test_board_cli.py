"""Run the installed CLI and its embedded adapter with a synthetic Espressif transport."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
CLI = os.environ.get("PRIME_BIN", str(ROOT / "target/debug/prime-lang"))
FLASH = bytes(range(256)) * 4

# Substitute the SDK at the adapter boundary, including its progress on stdout.
SDK = '''
import os, sys, types
from pathlib import Path

events = Path(os.environ["BOARD_EVENTS"])
fault = os.environ.get("BOARD_FAULT", "")
flash = bytes(range(256)) * 4

def event(name):
    with events.open("a") as log:
        print(name, file=log)
    print(name)

class Device:
    CHIP_NAME = os.environ.get("BOARD_CHIP", "ESP32")
    def __enter__(self):
        return self
    def __exit__(self, *args):
        event("close")
    def get_chip_description(self):
        return "Test board revision 3"
    def read_mac(self):
        return bytes(range(6))
    def change_baud(self, baud):
        event(f"baud {baud}")

def detect_chip(*, port):
    event(f"connect {port}")
    return Device()

def read_flash(esp, address, size, *, output, no_progress):
    assert address == 0 and size == len(flash)
    event("read")
    Path(output).write_bytes(flash[:-1] if fault == "short" else flash)

def verify_flash(esp, chunks):
    event("verify")
    (offset, image), = chunks
    assert offset == 0
    if fault == "verify":
        raise ValueError("verification failed")
    if fault == "race":
        Path(os.environ["BOARD_DEST"]).write_bytes(b"other process")
    image.read()

esp = types.ModuleType("esptool")
esp.__version__ = "5.5.0"
esp.cmds = types.SimpleNamespace(
    detect_chip=detect_chip,
    run_stub=lambda device: device,
    attach_flash=lambda device: event("attach"),
    detect_flash_size=lambda device: "1KB",
    read_flash=read_flash, verify_flash=verify_flash,
    reset_chip=lambda device, mode: event(mode),
)
util = types.ModuleType("esptool.util")
util.flash_size_bytes = lambda size: len(flash)
sys.modules.update({"esptool": esp, "esptool.util": util})
isolated, mode, adapter, *arguments = sys.argv[1:]
assert (isolated, mode) == ("-I", "-c")
sys.argv = ["-c", *arguments]
exec(compile(adapter, "<embedded>", "exec"), {"__name__": "__main__"})
'''


class BoardCli(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.work = Path(temp.name)
        self.events = self.work / "events"
        self.out = self.work / "saved flash.bin"
        python = self.work / "sdk-python"
        python.write_text(f"#!{sys.executable}\n" + SDK)
        python.chmod(0o700)
        self.env = dict(os.environ, PRIME_ESP_PYTHON=str(python), BOARD_EVENTS=str(self.events),
                        BOARD_DEST=str(self.out), BOARD_FAULT="", BOARD_CHIP="ESP32")

    def command(self, verb, *arguments, success=True):
        result = subprocess.run([CLI, "board", verb, "--port", "named-port", *arguments],
                                env=self.env, cwd=self.work, capture_output=True, timeout=10)
        self.assertEqual(result.returncode, 0 if success else 1, result)
        if success:
            return json.loads(result.stdout)
        self.assertEqual(result.stdout, b"")
        return result.stderr.decode()

    def test_identification_and_target_mapping(self):
        for chip, target in (("ESP32", "xtensa-esp32-espidf"), ("ESP32-C3", "riscv32imc-unknown-none-elf"),
                             ("ESP32-S3", None)):
            with self.subTest(chip=chip):
                self.env["BOARD_CHIP"] = chip
                info = self.command("inspect")
                self.assertEqual(info["chip"], chip)
                self.assertEqual(info["prime_target"], target)
                self.assertEqual(info["flash_bytes"], len(FLASH))
                self.assertEqual(info["schema_version"], 1)
                self.assertNotIn("sha256", info)
                self.assertNotIn("image", info)
        self.assertNotIn("read", self.events.read_text().splitlines())

    def test_verified_backup_and_existing_destination(self):
        info = self.command("backup", "--baud", "115200", "--out", self.out)
        self.assertEqual(Path(info["image"]).read_bytes(), FLASH)
        self.assertEqual(info["sha256"], hashlib.sha256(FLASH).hexdigest())
        events = self.events.read_text()
        self.assertIn("baud 115200", events)
        self.assertIn("read\nverify\nhard-reset\nclose\n", events)
        self.assertIn("already exists", self.command("backup", "--out", self.out, success=False))
        self.assertEqual(self.events.read_text(), events)

    def test_failures_do_not_publish_or_replace_images(self):
        for fault in ("verify", "short", "race"):
            with self.subTest(fault=fault):
                self.env["BOARD_FAULT"] = fault
                before = set(self.work.iterdir())
                self.command("backup", "--out", self.out, success=False)
                added = set(self.work.iterdir()) - before - {self.events}
                if fault == "race":
                    self.assertEqual(added, {self.out})
                    self.assertEqual(self.out.read_bytes(), b"other process")
                else:
                    self.assertEqual(added, set())
                self.assertTrue(self.events.read_text().endswith("hard-reset\nclose\n"))


if __name__ == "__main__":
    unittest.main()
