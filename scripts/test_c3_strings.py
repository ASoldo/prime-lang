#!/usr/bin/env python3
"""Execute CLI-built ESP32-C3 string, integer and token ownership probes on Unicorn."""

import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest

from unicorn import Uc, UcError, UC_ARCH_RISCV, UC_HOOK_CODE, UC_MODE_RISCV32
from unicorn.riscv_const import (
    UC_RISCV_REG_A0, UC_RISCV_REG_A1, UC_RISCV_REG_A2, UC_RISCV_REG_GP, UC_RISCV_REG_PC, UC_RISCV_REG_RA, UC_RISCV_REG_SP,
)


FIXTURE = Path(__file__).resolve().parent / "tests/fixtures/c3_strings"
TEXT, DATA, RAM_SIZE = 0x40380000, 0x3FC90000, 96 * 1024
SCRATCH = 0x10000


class C3Program(unittest.TestCase):
    source = None
    expected_error = None
    build_env = {}
    module = "c3_strings"

    @classmethod
    def setUpClass(cls):
        temp = tempfile.TemporaryDirectory()
        cls.addClassCleanup(temp.cleanup)
        work = Path(temp.name)
        shutil.copytree(FIXTURE, work, dirs_exist_ok=True)
        cli = os.environ.get("PRIME_BIN", "prime-lang")

        # Cargo runs from the temporary fixture, outside the pinned toolchain's directory.
        toolchain = subprocess.check_output(
            ["rustup", "show", "active-toolchain"], cwd=FIXTURE, text=True
        ).split()[0]
        env = dict(os.environ, RUSTUP_TOOLCHAIN=toolchain, **cls.build_env)
        manifest = work / "prime.toml"
        manifest.write_text(manifest.read_text().replace('name = "c3_strings"', f'name = "{cls.module}"'))

        def run(*cmd):
            return subprocess.check_output(cmd, cwd=work, env=env)

        if cls.source:
            (work / "main.prime").write_text(cls.source)
        run(cli, "check", "main.prime", "--format", "json")
        result = subprocess.run([cli, "run", "main.prime"], cwd=work, env=env, capture_output=True)
        if cls.expected_error:
            if result.returncode == 0 or cls.expected_error not in result.stderr:
                raise AssertionError(result)
        elif result.returncode or result.stdout != cls.expected_output:
            raise AssertionError(result)
        cc = shutil.which("riscv32-esp-elf-gcc")
        asm = [cc] if cc else ["clang", "--target=riscv32-unknown-elf"]
        if not cc:
            with (work / "prime.toml").open("a") as manifest:
                manifest.write('cc = "clang"\n')
                manifest.write('ld_flags = "--target=riscv32-unknown-elf -fuse-ld=lld -nodefaultlibs"\n')
        run(*asm, "-march=rv32imc", "-mabi=ilp32", "-c", "startup.S", "-o", "startup.o")
        run(cli, "build", "main.prime", "--name", "c3", "--no-flash")
        elf = work / ".build.prime/c3-riscv32imc-unknown-none-elf/c3"
        cls.elf = elf
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

    def observe_print(self):
        handles = []
        address = self.symbols["prime_print"]
        self.cpu.hook_add(UC_HOOK_CODE, lambda cpu, *_: handles.append(cpu.reg_read(UC_RISCV_REG_A0)),
                          begin=address, end=address)
        return handles

    def assert_output(self, handles):
        # Observe the handles passed to print and ask the linked ABI for their bytes.
        handles.clear()
        self.execute("_start")
        self.assertEqual(b"".join(map(self.read_string, handles)), self.expected_output)


class C3Strings(C3Program):
    expected_output = "result=empty\nčaj\n-13\n".encode()

    def test_prime_result(self):
        handles = self.observe_print()
        for _ in range(2):
            self.assert_output(handles)

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


class C3RecordBytes(C3Program):
    module = "record_bytes"
    source = (FIXTURE.parents[3] / "workspace/demos/no_std_parity/record_bytes.prime").read_text()
    expected_output = b"record u16 4660\nrecord u32 4278194740\nrecord short rejected\nrecord crc 10673\n"

    def test_portable_core(self):
        self.assert_output(self.observe_print())


class C3RecordBounds(C3Program):
    source = """module c3_strings;
        import core::{Option, read_u16_le, read_u32_le, crc16_ccitt_false};
        fn probe(data: &[]uint8, offset: int32) {
            let mut uint16 word = 25;
            let mut uint32 wide = 26;
            if read_u16_le(data, offset, &mut word) { out(word); } else { out("short"); }
            if read_u32_le(data, offset, &mut wide) { out(wide); } else { out("short"); }
        }
        fn main() {
            let mut int32 offset = 0;
            let mut uint8 byte = 255;
            let []uint8 bytes = [byte, 255, byte, 255];
            probe(&bytes, offset);
            probe(&bytes, offset + 1);
            probe(&bytes, offset - 1);
            probe(&bytes, offset + 2147483647);
            match bytes[offset - 1] { Some(_) => out("unexpected"), None => out("negative"), }
            match bytes[offset + 4] { Some(_) => out("unexpected"), None => out("past end"), }
            let mut uint32 saved = 0;
            out(read_u32_le(&bytes, offset, &mut saved));
            let []uint8 payload = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20];
            out(crc16_ccitt_false(&payload));
            out(read_u32_le(&bytes, offset + 1, &mut saved));
            out(saved);
            let []uint8 empty = [];
            probe(&empty, offset);
            out(crc16_ccitt_false(&empty));
        }"""
    expected_output = (
        b"65535\n4294967295\n65535\nshort\nshort\nshort\nshort\nshort\n"
        b"negative\npast end\ntrue\n7275\nfalse\n4294967295\nshort\nshort\n65535\n"
    )

    def test_dynamic_bytes_and_offsets(self):
        self.assert_output(self.observe_print())


class C3TokenOwners(C3Program):
    module = "task_ownership"
    source = (FIXTURE.parent / "task_ownership/main.prime").read_text()
    build_env = {"PRIME_RT_CANCEL_TOKEN_SLOTS": "2"}
    expected_output = (
        b"serial ok\nold alias ok\nloop exits ok\nescaping capture ok\ncapture ok\n"
        b"pool available\nguard ran\nindependent ok\nreuse ok\n"
    )

    def test_linked_two_slot_runtime(self):
        verifier = Path(__file__).with_name("test_embedded_timers.py")
        subprocess.run(
            [sys.executable, verifier, "EmbeddedTimers.test_compiled_token_owners"],
            env=dict(os.environ, PRIME_TOKEN_PROBE_ELF=str(self.elf)),
            check=True,
        )


class C3Integers(C3Program):
    source = """module c3_strings;
        fn add_word(x: int64) -> int64 { x + 4294967296 }
        fn main() {
            let mut int64 x = 0;
            let int64 hi = 9007199254740993;
            let int64 lo = 9007199254740992;
            out(hi + x > lo);
            out(add_word(x) > x);
            out(add_word(x) < x);
            out(cast[usize](x - 1));
            out(cast[isize](x + 4294967295));
            out(cast[uint8](256));
        }"""
    expected_output = b"true\ntrue\nfalse\n4294967295\n-1\n0\n"

    def test_integer_widths(self):
        self.assert_output(self.observe_print())


class C3IntegerOverflow(C3Program):
    source = """module c3_strings;
        fn byte(n: uint8) -> uint8 { n }
        fn main() { let mut int64 x = 256; out(byte(x)); }"""
    expected_error = b"out of range for `uint8`"

    def test_dynamic_overflow_reports_then_traps(self):
        diagnostics = []
        def capture_error(cpu, *_):
            if cpu.reg_read(UC_RISCV_REG_A0) == 0:
                ptr, size = cpu.reg_read(UC_RISCV_REG_A1), cpu.reg_read(UC_RISCV_REG_A2)
                diagnostics.append(bytes(cpu.mem_read(ptr, size)))

        check = self.symbols["prime_integer_check"]
        self.cpu.hook_add(UC_HOOK_CODE, capture_error, begin=check, end=check)
        # Unicorn versions report different exceptions for the guest breakpoint.
        # Check the faulting RV32 instruction (c.ebreak or ebreak) directly.
        with self.assertRaises(UcError):
            self.execute("_start")
        pc = self.cpu.reg_read(UC_RISCV_REG_PC)
        instruction = bytes(self.cpu.mem_read(pc, 4))
        self.assertTrue(instruction.startswith(b"\x02\x90") or instruction == b"\x73\x00\x10\x00", instruction.hex())
        self.assertEqual(diagnostics, [b"integer result is out of range for `uint8`"])


if __name__ == "__main__":
    unittest.main()
