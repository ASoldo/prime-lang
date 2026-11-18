"""Execute the no_std RV32IMC ABI with a controlled ESP32-C3 SYSTIMER.

Requires the riscv32imc-unknown-none-elf rustup target, LLD, LLVM tools and
Python's unicorn package (2.1+). Run with python3 scripts/test_embedded_timers.py.
"""

from pathlib import Path
import os
import struct
import subprocess
import tempfile
import unittest

from unicorn import Uc, UC_ARCH_RISCV, UC_MODE_RISCV32, UC_HOOK_CODE
from unicorn.riscv_const import (
    UC_RISCV_REG_A0, UC_RISCV_REG_GP, UC_RISCV_REG_MSTATUS, UC_RISCV_REG_PC,
    UC_RISCV_REG_RA, UC_RISCV_REG_SP,
)


RAM = 0x10000
STOP = RAM + 0x1F000
OUT = RAM + 0x1E000
ARG = OUT + 16
TICKS_PER_MS = 16_000
PERIOD = 1 << 52
SYMBOLS = """
now_ms delay_ms sleep_task task_poll task_wake task_cancel join unit_new int_new
channel_new send recv recv_task recv_timeout close enum_tag enum_get
cancel_token_new cancel_token_cancel cancel_token_is_cancelled value_retain value_release
""".split()


class EmbeddedTimers(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        root = Path(__file__).resolve().parents[1]
        temp = tempfile.TemporaryDirectory()
        cls.addClassCleanup(temp.cleanup)
        work = Path(temp.name)

        def run(*args, **kw):
            return subprocess.run(args, cwd=work, check=True, **kw)

        # rustup selects the pinned project toolchain from the working directory.
        subprocess.run(("rustc", "--edition=2021", "--crate-type=staticlib", "-Copt-level=s",
            "-Cpanic=abort", "-Dstatic_mut_refs", "--target=riscv32imc-unknown-none-elf",
            root / "src/runtime/abi.rs", "-o", work / "runtime.a"), cwd=root, check=True,
            env=dict(os.environ, PRIME_RT_CANCEL_TOKEN_SLOTS="2", PRIME_RT_TASK_SLOTS="3",
                     PRIME_RT_CHANNEL_SLOTS="2", PRIME_RT_CHANNEL_CAP="3", PRIME_RT_CHANNEL_WAITERS="3",
                     PRIME_RT_RECV_POLL_MS="1"))
        (work / "map.ld").write_text(
            "SECTIONS { . = 0x10000; .text : { *(.text*) } .rodata : { *(.rodata*) } "
            ".data : { *(.data* .sdata*) } .bss : { *(.bss* .sbss*) } }"
        )
        run("ld.lld", "--gc-sections", "--no-relax", "-T", "map.ld", "-e", "prime_now_ms",
            *["--undefined=prime_" + name for name in SYMBOLS], "runtime.a", "-o", "probe")
        run("llvm-objcopy", "-O", "binary", "probe", "probe.bin")
        cls.code = (work / "probe.bin").read_bytes()
        symbols = run("llvm-nm", "--defined-only", "probe", capture_output=True, text=True)
        cls.symbols = {
            name: int(address, 16) for address, _, name in
            (line.split() for line in symbols.stdout.splitlines())
        }

    def setUp(self):
        self.cpu = Uc(UC_ARCH_RISCV, UC_MODE_RISCV32)
        self.cpu.mem_map(RAM, 0x20000)
        self.cpu.mem_write(RAM, self.code)
        self.cpu.mem_map(0x600C0000, 0x1000)
        self.cpu.mem_write(0x600C0018, (1 << 29).to_bytes(4, "little"))
        self.ticks = self.step = self.conf = self.snapshot = 0
        self.valid_reads = 0
        self.cpu.mmio_map(0x60023000, 0x1000, self.read_timer, None, self.write_timer, None)

    def read_timer(self, cpu, offset, size, data):
        if offset == 0:
            return self.conf
        if offset == 4:
            self.valid_reads += 1
            return int(self.valid_reads >= 2) << 29
        self.assertGreaterEqual(self.valid_reads, 2)
        return {0x40: self.snapshot >> 32, 0x44: self.snapshot & 0xFFFFFFFF}[offset]

    def write_timer(self, cpu, offset, size, value, data):
        if offset == 0:
            self.conf = value
            return
        self.assertEqual((offset, size, value), (4, 4, 1 << 30))
        self.assertTrue(self.conf & (1 << 30))
        self.assertFalse(self.read_word(0x600C0018) & (1 << 29))
        self.assertTrue(self.read_word(0x600C0010) & (1 << 29))
        self.snapshot = self.ticks % PERIOD
        self.ticks += self.step
        self.valid_reads = 0

    def read_word(self, addr, size=4):
        return int.from_bytes(self.cpu.mem_read(addr, size), "little")

    def call(self, name, *args):
        cpu = self.cpu
        for n, value in enumerate(args):
            cpu.reg_write(UC_RISCV_REG_A0 + n, value)
        cpu.reg_write(UC_RISCV_REG_RA, STOP)
        cpu.reg_write(UC_RISCV_REG_SP, OUT - 16)
        before = cpu.reg_read(UC_RISCV_REG_MSTATUS)
        cpu.emu_start(self.symbols["prime_" + name], STOP, count=100_000)
        self.assertEqual(cpu.reg_read(UC_RISCV_REG_PC), STOP, name + " failed to return")
        self.assertEqual(cpu.reg_read(UC_RISCV_REG_MSTATUS) & 8, before & 8)
        return cpu.reg_read(UC_RISCV_REG_A0)

    def arg128(self, value):
        # RV32 passes an LLVM i128 indirectly, including for returned timestamps.
        self.cpu.mem_write(ARG, value.to_bytes(16, "little", signed=True))
        return ARG

    def sleep_task(self, millis):
        return self.call("sleep_task", self.arg128(millis))

    def now(self):
        self.call("now_ms", OUT)
        return self.read_word(OUT, 16)

    def test_sleep_creation_poll_join_and_pool_reuse(self):
        self.cpu.reg_write(UC_RISCV_REG_MSTATUS, 8)
        self.ticks = 7 * TICKS_PER_MS
        slow, fast = self.sleep_task(100), self.sleep_task(3)
        self.assertTrue(slow and fast and slow != fast)
        self.assertEqual(self.call("task_poll", slow, OUT), 1)
        self.ticks += 2 * TICKS_PER_MS
        self.assertEqual(self.call("task_poll", fast, OUT), 1)
        self.ticks += TICKS_PER_MS
        self.assertEqual(self.call("task_poll", fast, OUT), 0)
        self.assertEqual(self.read_word(OUT), self.call("unit_new"))
        self.assertEqual(self.call("task_cancel", slow), 0)
        # Reusing a cancelled slot must not inherit its old deadline.
        pending = self.sleep_task(101)
        self.ticks += 98 * TICKS_PER_MS
        self.assertEqual(self.call("task_poll", pending, OUT), 1)
        self.step = TICKS_PER_MS
        self.assertEqual(self.call("join", pending, OUT), 0)

    def test_sleep_retains_fractional_start(self):
        self.ticks = TICKS_PER_MS - 1
        task = self.sleep_task(1)
        self.ticks = 2 * TICKS_PER_MS - 2
        self.assertEqual(self.call("task_poll", task, OUT), 1)
        self.ticks += 1
        self.assertEqual(self.call("task_poll", task, OUT), 0)

    def test_immediate_and_overflowing_durations(self):
        self.ticks = TICKS_PER_MS
        for millis in [0, -1, -(1 << 127)]:
            self.assertEqual(self.call("task_poll", self.sleep_task(millis), OUT), 0)
        for millis in [(1 << 31) + 1, (1 << 63) - 1, (1 << 127) - 1]:
            task = self.sleep_task(millis)
            self.assertEqual(self.call("task_poll", task, OUT), 1)
            self.assertEqual(self.call("task_cancel", task), 0)

    def test_counter_rollovers_deadlines_and_overshoot(self):
        for boundary in [1 << 32, TICKS_PER_MS << 32, PERIOD]:
            self.ticks = boundary - 2 * TICKS_PER_MS
            self.assertEqual(self.now(), self.ticks // TICKS_PER_MS)
            task = self.sleep_task(5)
            self.ticks += 4 * TICKS_PER_MS
            self.assertEqual(self.now(), self.ticks // TICKS_PER_MS)
            self.assertEqual(self.call("task_poll", task, OUT), 1)
            self.ticks += 9 * TICKS_PER_MS
            self.assertEqual(self.call("task_poll", task, OUT), 0)

    def test_channel_deadlines_waiters_and_cancellation(self):
        self.assertEqual(self.call("channel_new", OUT, OUT + 4), 0)
        tx, rx = self.read_word(OUT), self.read_word(OUT + 4)
        for ms in [0, -1]:
            self.assertEqual(self.call("recv_timeout", rx, self.arg128(ms), OUT), 1)
        self.step = TICKS_PER_MS
        self.assertEqual(self.call("recv_timeout", rx, self.arg128(5), OUT), 1)
        self.assertGreaterEqual(self.now(), 5)
        self.step = 0
        waiter = self.call("recv_task", rx)
        self.assertEqual(self.call("task_poll", waiter, OUT), 1)
        self.assertEqual(self.call("task_cancel", waiter), 0)
        sleeper = self.sleep_task(10)
        self.assertEqual(self.call("send", tx, self.call("unit_new")), 0)
        self.assertEqual(self.call("task_poll", sleeper, OUT), 1)
        self.assertEqual(self.call("join", self.call("recv_task", rx), OUT), 0)
        self.assertEqual(self.call("enum_get", self.read_word(OUT), 0), self.call("unit_new"))
        self.assertEqual(self.call("task_cancel", sleeper), 0)
        self.assertEqual(self.call("close", rx), 0)

    def channel(self):
        self.assertEqual(self.call("channel_new", OUT, OUT + 4), 0)
        return self.read_word(OUT), self.read_word(OUT + 4)

    def assert_recv_result(self, task, expected):
        self.assertEqual(self.call("task_poll", task, OUT), 0)
        result = self.read_word(OUT)
        self.assertTrue(result)
        if expected is None:
            self.assertEqual(self.call("enum_tag", result), 1)
        else:
            self.assertEqual(self.call("enum_tag", result), 0)
            self.assertEqual(self.call("enum_get", result, 0), expected)

    def test_close_preserves_wrapped_fifo_for_all_receive_paths(self):
        tx, rx = self.channel()
        payloads = [self.call("int_new", self.arg128(n)) for n in [12, 16, 20, 24]]
        for payload in payloads[:3]:
            self.assertEqual(self.call("send", tx, payload), 0)
        self.assertEqual(self.call("send", tx, payloads[3]), 1)  # full
        self.assertEqual(self.call("recv", rx, OUT), 0)
        self.assertEqual(self.read_word(OUT), payloads[0])
        self.assertEqual(self.call("send", tx, payloads[3]), 0)  # wraps the tail
        self.assertEqual(self.call("close", tx), 0)
        self.assertEqual(self.call("send", tx, payloads[0]), 2)
        self.assertEqual(self.call("recv_timeout", rx, self.arg128(0), OUT), 0)
        self.assertEqual(self.read_word(OUT), payloads[1])
        self.assert_recv_result(self.call("recv_task", rx), payloads[2])
        self.assertEqual(self.call("recv", rx, OUT), 0)
        self.assertEqual(self.read_word(OUT), payloads[3])
        self.assertEqual(self.call("recv", rx, OUT), 2)
        self.assertEqual(self.call("recv_timeout", rx, self.arg128(100), OUT), 2)
        self.assert_recv_result(self.call("recv_task", rx), None)

    def test_only_drained_slots_recycle_and_old_handles_stay_closed(self):
        tx, rx = self.channel()
        other_tx, other_rx = self.channel()
        self.assertEqual(self.call("send", tx, 40), 0)
        self.assertEqual(self.call("close", tx), 0)
        self.assertEqual(self.call("channel_new", OUT, OUT + 4), 1)
        self.assertEqual((self.read_word(OUT), self.read_word(OUT + 4)), (0, 0))
        self.assert_recv_result(self.call("recv_task", rx), 40)
        for _ in range(50):
            new_tx, new_rx = self.channel()
            self.assertEqual(self.call("send", new_tx, 44), 0)
            self.assertEqual(self.call("send", tx, 48), 2)
            self.call("close", tx)
            self.assertEqual(self.call("recv", rx, OUT), 2)
            self.assertEqual(self.call("recv_timeout", rx, self.arg128(1), OUT), 2)
            self.assert_recv_result(self.call("recv_task", rx), None)
            self.assertEqual(self.call("send", new_tx, 52), 0)
            self.assertEqual(self.call("close", new_tx), 0)
            self.assert_recv_result(self.call("recv_task", new_rx), 44)
            self.assert_recv_result(self.call("recv_task", new_rx), 52)
        self.assertEqual(self.call("send", other_tx, 56), 0)
        self.assert_recv_result(self.call("recv_task", other_rx), 56)
        self.assertEqual(self.call("send", 0, 40), 1)
        self.assertEqual(self.call("recv", self.call("unit_new"), OUT), 1)

    def test_close_completes_multiple_waiters_before_slot_reuse(self):
        tx, rx = self.channel()
        tasks = [self.call("recv_task", rx) for _ in range(3)]
        for task in tasks:
            self.assertEqual(self.call("task_poll", task, OUT), 1)
        self.assertEqual(self.call("send", tx, 60), 0)
        self.assertEqual(self.call("close", tx), 0)
        new_tx, new_rx = self.channel()
        self.assertEqual(self.call("send", new_tx, 64), 0)
        for task, expected in zip(tasks, [60, None, None]):
            self.assert_recv_result(task, expected)
        self.assert_recv_result(self.call("recv_task", new_rx), 64)
        self.assert_recv_result(self.call("recv_task", rx), None)

    def test_closing_channel_cannot_complete_a_reused_task(self):
        first_tx, first_rx = self.channel()
        next_tx, next_rx = self.channel()
        task = self.call("recv_task", first_rx)
        ready_value = self.call("unit_new")
        self.assertEqual(self.call("task_wake", task, ready_value), 0)
        self.assertEqual(self.call("task_poll", task, OUT), 0)
        self.assertEqual(self.read_word(OUT), ready_value)
        next_task = self.call("recv_task", next_rx)
        self.assertEqual(self.call("close", first_tx), 0)
        self.assertEqual(self.call("task_poll", next_task, OUT), 1)
        self.assertEqual(self.call("send", next_tx, ready_value), 0)
        self.assert_recv_result(next_task, ready_value)

    def test_token_exhaustion_and_aliases(self):
        first, second = self.call("cancel_token_new"), self.call("cancel_token_new")
        self.assertTrue(first and second and first != second)
        alias = self.call("value_retain", first)
        self.assertEqual(self.call("cancel_token_cancel", first), 0)
        self.call("value_release", first)
        self.assertTrue(self.call("cancel_token_is_cancelled", alias))
        self.assertFalse(self.call("cancel_token_is_cancelled", second))
        self.assertEqual(self.call("cancel_token_new"), 0)

        self.assertTrue(self.call("cancel_token_is_cancelled", 0))
        self.assertEqual(self.call("cancel_token_cancel", 0), 1)
        self.call("value_release", alias)
        for _ in range(100):
            fresh = self.call("cancel_token_new")
            self.assertTrue(fresh)
            self.assertNotEqual(fresh, first)
            self.assertEqual(self.call("cancel_token_cancel", first), 1)
            self.call("value_release", first)
            self.assertFalse(self.call("cancel_token_is_cancelled", fresh))
            self.call("value_release", fresh)
        self.assertFalse(self.call("cancel_token_is_cancelled", second))
        self.call("value_release", second)

    def test_token_retain_overflow_and_task_result(self):
        owner = self.call("cancel_token_new")
        claims = [self.call("value_retain", owner) for _ in range(510)]
        self.assertEqual(claims, [owner] * 510)
        self.assertEqual(self.call("value_retain", owner), 0)
        for alias in claims:
            self.call("value_release", alias)
        self.assertFalse(self.call("cancel_token_is_cancelled", owner))

        task = self.sleep_task(100)
        self.assertEqual(self.call("task_wake", task, owner), 0)
        self.call("value_release", owner)
        self.assertEqual(self.call("task_poll", task, OUT), 0)
        returned = self.read_word(OUT)
        self.assertEqual(returned, owner)
        self.assertFalse(self.call("cancel_token_is_cancelled", returned))
        self.call("value_release", returned)
        self.assertEqual(self.call("cancel_token_cancel", owner), 1)

    @unittest.skipUnless(os.environ.get("PRIME_TOKEN_PROBE_ELF"), "supply two-slot Prime firmware ELF")
    def test_compiled_token_owners(self):
        # Execute the CLI-linked firmware and read the C3 PrimeString passed to
        # each print call before any later call can affect the observed output.
        path = Path(os.environ["PRIME_TOKEN_PROBE_ELF"])
        elf = path.read_bytes()
        self.assertEqual(elf[:6], b"\x7fELF\x01\x01")
        phoff = struct.unpack_from("<I", elf, 28)[0]
        phsize, phnum = struct.unpack_from("<HH", elf, 42)
        for i in range(phnum):
            kind, offset, addr, _, size, memsize, _, _ = struct.unpack_from(
                "<8I", elf, phoff + i * phsize)
            if kind == 1 and memsize:
                start = addr & ~4095
                end = (addr + memsize + 4095) & ~4095
                self.cpu.mem_map(start, end - start)
                self.cpu.mem_write(addr, elf[offset:offset + size])
        symbols = subprocess.check_output(["llvm-nm", "--defined-only", path], text=True)
        names = {name: int(address, 16) for address, _, name in
                 (line.split() for line in symbols.splitlines())}
        messages = []

        def print_string(cpu, _addr, _size, _data):
            handle = cpu.reg_read(UC_RISCV_REG_A0)
            ptr, length = struct.unpack("<II", cpu.mem_read(handle, 8))
            value = bytes(cpu.mem_read(ptr, length)).decode()
            if value.strip():
                messages.append(value)

        address = names["prime_print"]
        self.cpu.hook_add(UC_HOOK_CODE, print_string, begin=address, end=address)
        self.cpu.reg_write(UC_RISCV_REG_GP, names["__global_pointer$"])
        self.cpu.reg_write(UC_RISCV_REG_SP, OUT - 16)
        self.cpu.reg_write(UC_RISCV_REG_RA, STOP)
        self.cpu.emu_start(names["main"], STOP, count=2_000_000)
        self.assertEqual(self.cpu.reg_read(UC_RISCV_REG_PC), STOP)
        self.assertEqual(messages, [
            "serial ok", "old alias ok", "loop exits ok", "escaping capture ok",
            "capture ok", "pool full", "guard cancelled", "independent ok", "reuse ok",
        ])


if __name__ == "__main__":
    unittest.main(verbosity=2)
