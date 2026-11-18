"""Run a Prime CLI-built ESP32-C3 string fixture and probe its native ABI."""

import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

from unicorn import Uc, UC_ARCH_RISCV, UC_HOOK_CODE, UC_MODE_RISCV32
from unicorn.riscv_const import (
    UC_RISCV_REG_A0, UC_RISCV_REG_GP, UC_RISCV_REG_PC, UC_RISCV_REG_RA, UC_RISCV_REG_SP,
)


FIXTURE = Path(__file__).resolve().parent / "tests/fixtures/c3_strings"
TEXT, DATA, RAM_SIZE = 0x40380000, 0x3FC90000, 96 * 1024
SCRATCH = 0x10000


class C3Strings(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        temp = tempfile.TemporaryDirectory()
        cls.addClassCleanup(temp.cleanup)
        work = Path(temp.name)
        shutil.copytree(FIXTURE, work, dirs_exist_ok=True)
        cli = os.environ.get("PRIME_BIN", "prime-lang")

        def run(*cmd):
            return subprocess.check_output(cmd, cwd=work)

        ref = run(cli, "run", "main.prime")
        if ref != "result=empty\nčaj\n-13\n".encode():
            raise AssertionError(ref)
        cls.reference = ref
        run("riscv32-esp-elf-gcc", "-march=rv32imc", "-mabi=ilp32", "-c", "startup.S", "-o", "startup.o")
        run(cli, "build", "main.prime", "--name", "c3", "--no-flash")
        elf = work / ".build.prime/c3-riscv32imc-unknown-none-elf/c3"
        run("llvm-objcopy", "--dump-section", ".text=text.bin", "--dump-section", ".data=data.bin", elf)
        cls.code = (work / "text.bin").read_bytes()
        cls.data = (work / "data.bin").read_bytes()
        cls.symbols = {}
        for line in run("llvm-nm", "--defined-only", elf).decode().splitlines():
            addr, _, symbol = line.split(maxsplit=2)
            cls.symbols[symbol] = int(addr, 16)

    def setUp(self):
        self.cpu = Uc(UC_ARCH_RISCV, UC_MODE_RISCV32)
        for address in (TEXT, DATA, SCRATCH):
            self.cpu.mem_map(address, RAM_SIZE)
        self.cpu.mem_write(TEXT, self.code)
        self.cpu.mem_write(DATA, self.data)
        self.cpu.reg_write(UC_RISCV_REG_GP, self.symbols["__global_pointer$"])

    def execute(self, symbol):
        stop = self.symbols["probe_done"]
        self.cpu.emu_start(self.symbols[symbol], stop, count=2_000_000)
        self.assertEqual(self.cpu.reg_read(UC_RISCV_REG_PC), stop, symbol)

    def call(self, symbol, *args):
        for idx, arg in enumerate(args):
            self.cpu.reg_write(UC_RISCV_REG_A0 + idx, arg)
        self.cpu.reg_write(UC_RISCV_REG_RA, self.symbols["probe_done"])
        self.cpu.reg_write(UC_RISCV_REG_SP, DATA + RAM_SIZE)
        self.execute("prime_" + symbol)
        return self.cpu.reg_read(UC_RISCV_REG_A0)

    def new(self, value):
        if value:
            self.cpu.mem_write(SCRATCH + 16, value)
        return self.call("string_new", SCRATCH + 16, len(value))

    def read_string(self, handle):
        self.assertEqual(self.call("value_tag", handle), 4)
        self.assertEqual(self.call("value_as_string", handle, SCRATCH, SCRATCH + 4), 0)
        ptr = int.from_bytes(self.cpu.mem_read(SCRATCH, 4), "little")
        length = int.from_bytes(self.cpu.mem_read(SCRATCH + 4, 4), "little")
        return bytes(self.cpu.mem_read(ptr, length))

    def test_prime_result(self):
        # Observe the handles actually passed to print, then ask the linked ABI
        # for their bytes. Every constructor, match and print executes on RV32.
        handles = []
        address = self.symbols["prime_print"]
        self.cpu.hook_add(UC_HOOK_CODE, lambda cpu, *_: handles.append(cpu.reg_read(UC_RISCV_REG_A0)),
                          begin=address, end=address)
        for _ in range(2):
            handles.clear()
            self.execute("_start")
            self.assertEqual(b"".join(map(self.read_string, handles)), self.reference)

    def test_byte_capacity(self):
        value = bytes(range(256)) * 32
        self.assertEqual(self.new(value + b"!"), 0)
        handle = self.new(value)
        self.assertEqual(self.new(b"more"), 0)
        self.assertEqual(self.new(value), handle)
        self.assertEqual(self.read_string(handle), value)
        self.assertEqual(self.read_string(self.new(b"")), b"")

    def test_handle_capacity(self):
        handles = [self.new(bytes([n])) for n in range(64)]
        self.assertEqual(self.new(b"overflow"), 0)
        for n, handle in enumerate(handles * 2):
            value = bytes([n % 64])
            self.assertEqual(self.new(value), handle)
            self.assertEqual(self.read_string(handle), value)
        for invalid in [0, 1, handles[0] + 1]:
            self.assertEqual(self.call("value_as_string", invalid, SCRATCH, SCRATCH + 4), 1)
            self.assertEqual(bytes(self.cpu.mem_read(SCRATCH, 8)), bytes(8))

    def test_signed_formatting(self):
        for value in [0, -13, -(1 << 127), (1 << 127) - 1]:
            self.cpu.mem_write(SCRATCH + 16, value.to_bytes(16, "little", signed=True))
            handle = self.call("int_to_string", SCRATCH + 16)
            self.assertEqual(self.read_string(handle), str(value).encode())


if __name__ == "__main__":
    unittest.main()
