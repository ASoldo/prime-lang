# Embedded timers and cancellation tokens

`sleep_task(ms)` reserves a task slot and returns immediately. The task is ready on creation for zero or negative durations. A positive duration starts at the sampled hardware tick, including its fractional millisecond. Polling returns pending up to the deadline and returns unit on or after it. `join` polls until completion. Cancelling a pending sleep frees its slot immediately. Deadlines use saturating arithmetic on the compiler ABI's 128-bit integer argument.

## Clock sources

- The classic ESP32 startup reserves timer group 0, general-purpose timer 0. It runs at 500 kHz using the APB divider selected from the bootloader's recorded CPU frequency. The runtime assumes a fixed APB frequency while the application runs. The 64-bit counter spans roughly 1.17 million years and continues counting between polls. The runtime latches both halves for each reading. [Espressif timer registers](https://github.com/espressif/esp-idf/blob/v5.3.3/components/soc/esp32/include/soc/timer_group_reg.h) and [clock setup](https://github.com/espressif/esp-idf/blob/v5.3.3/components/esp_hw_support/port/esp32/rtc_clk.c).
- The ESP32-C3 runtime enables SYSTIMER unit 0 and waits for a valid hardware snapshot. It extends the 52-bit, 16 MHz counter across rollover before converting to milliseconds. Two consecutive clock reads must be less than one full counter period apart, about 8.92 years. A reboot starts a new clock epoch. [Espressif register definitions](https://github.com/espressif/esp-idf/blob/v5.3.3/components/soc/esp32c3/include/soc/systimer_reg.h), [snapshot protocol](https://github.com/espressif/esp-idf/blob/v5.3.3/components/hal/esp32c3/include/hal/systimer_ll.h) and [clock frequency](https://github.com/espressif/esp-idf/blob/v5.3.3/components/esp_hw_support/port/esp32c3/systimer.c).

## Closing an embedded channel

Closing a channel rejects further sends with `Closed`. Values already in its FIFO remain available through `recv`, `recv_timeout` and `recv_task`. After the final value, synchronous receives report `Closed` and every waiting or subsequent receive task resolves to `None`. The slot becomes available as soon as the closed queue is empty. A closed queue that the application abandons with unread values still occupies its slot.

Each allocation issues a pointer-sized opaque channel ID. Earlier IDs continue to report a closed channel after the slot is reused. Each slot has a finite sequence of IDs and retires if its next ID would overflow. On a 32-bit target there are 2³¹ distinct channel IDs across all slots.

## Token pool lifetime

The embedded ABI reserves 16 token slots by default (`PRIME_RT_CANCEL_TOKEN_SLOTS`). Allocation returns a null handle on exhaustion. Cancelling that handle returns `Invalid` and querying it returns false. Cancelling a live token leaves it available to every task sharing the token.

The compiler passes the same raw handle for token uses. Its current scope cleanup schedules drops for structs and enums, and emits no final release when a token binding goes out of scope. The embedded retain/release operations also carry no ownership accounting. Recycling on first cancellation, await, or apparent last use could make an existing alias observe or cancel a later token. A compiler/runtime contract for the last owner is required before a slot can be safely reused.

## Host regression

With Rust's `riscv32imc-unknown-none-elf` target, `ld.lld`, `llvm-objcopy`, `llvm-nm`, and Python's `unicorn` 2.1 installed, run the probe with the following command.

```sh
python3 scripts/test_embedded_timers.py
```

The probe runs the real RV32IMC ABI in host emulation with a controlled SYSTIMER snapshot. It exercises submillisecond starts, counter rollover, distant deadlines, cancellation, task slot reuse, channel FIFO wrap and draining, simultaneous waiters, stale handles, receive timeouts and token exhaustion. Board startup, electrical clock accuracy and the Xtensa peripheral still require hardware verification.

With the corrected channel ABI, the RV32 `no_std_parity` program returned 0. Its first three receive tasks produced `Some(5)`, `Some(7)`, and `Some(11)` after their senders closed. The Option probe selected `timeout parity join 11`, and the guarded waits selected timeout, cancellation, and timeout. The probe observed the Option tags and integer payloads in emulated memory, plus the arguments passed to the C3 string and integer formatting functions. The C3 string constructor remains a stub.

In the compiler build used for this measurement, the Result probe received `Some(7)` but still selected `async try parity err empty`. The emitted IR merges both receive branches into a block that prints the literal error unconditionally. The interpreter prints `async try parity ok 7`.

Separately, the coordinator verified a normal x86-hosted `prime-lang build main.prime --name c3 --no-flash` for `delay_ms(1)`. Upstream LLVM 23, Rust 1.100 for RV32IMC, and the Espressif GCC toolchain produced an ELF with the RISC-V compressed instruction set, soft-float ABI and entry address `0x40380000`. Espressif's image inspector accepted the 736-byte, single-segment C3 image. Booting on a physical board remains unverified.
