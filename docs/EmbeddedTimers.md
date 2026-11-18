# Embedded timers and cancellation tokens

`sleep_task(ms)` reserves a task slot and returns immediately. The task is ready on creation for zero or negative durations. A positive duration starts at the sampled hardware tick, including its fractional millisecond. Polling returns pending up to the deadline and returns unit on or after it. `join` polls until completion. Cancelling a pending sleep frees its slot immediately. Deadlines use saturating arithmetic on the compiler ABI's 128-bit integer argument.

## Clock sources

- The classic ESP32 startup reserves timer group 0, general-purpose timer 0. It runs at 500 kHz using the APB divider selected from the bootloader's recorded CPU frequency. The runtime assumes a fixed APB frequency while the application runs. The 64-bit counter spans roughly 1.17 million years and continues counting between polls. The runtime latches both halves for each reading. [Espressif timer registers](https://github.com/espressif/esp-idf/blob/v5.3.3/components/soc/esp32/include/soc/timer_group_reg.h) and [clock setup](https://github.com/espressif/esp-idf/blob/v5.3.3/components/esp_hw_support/port/esp32/rtc_clk.c).
- The ESP32-C3 runtime enables SYSTIMER unit 0 and waits for a valid hardware snapshot. It extends the 52-bit, 16 MHz counter across rollover before converting to milliseconds. Two consecutive clock reads must be less than one full counter period apart, about 8.92 years. A reboot starts a new clock epoch. [Espressif register definitions](https://github.com/espressif/esp-idf/blob/v5.3.3/components/soc/esp32c3/include/soc/systimer_reg.h), [snapshot protocol](https://github.com/espressif/esp-idf/blob/v5.3.3/components/hal/esp32c3/include/hal/systimer_ll.h) and [clock frequency](https://github.com/espressif/esp-idf/blob/v5.3.3/components/esp_hw_support/port/esp32c3/systimer.c).

## Token pool lifetime

The embedded ABI reserves 16 token slots by default (`PRIME_RT_CANCEL_TOKEN_SLOTS`). Allocation returns a null handle on exhaustion. Cancelling that handle returns `Invalid` and querying it returns false. Cancelling a live token leaves it available to every task sharing the token.

The compiler passes the same raw handle for token uses. Its current scope cleanup schedules drops for structs and enums, and emits no final release when a token binding goes out of scope. The embedded retain/release operations also carry no ownership accounting. Recycling on first cancellation, await, or apparent last use could make an existing alias observe or cancel a later token. A compiler/runtime contract for the last owner is required before a slot can be safely reused.

## Host regression

With Rust's `riscv32imc-unknown-none-elf` target, `ld.lld`, `llvm-objcopy`, `llvm-nm`, and Python's `unicorn` 2.1 installed, run the probe with the following command.

```sh
python3 scripts/test_embedded_timers.py
```

The probe runs the real RV32IMC ABI in host emulation with a controlled SYSTIMER snapshot. It exercises submillisecond starts, counter rollover, distant deadlines, cancellation, task slot reuse, channel delivery, receive timeouts, and token exhaustion. Board startup, electrical clock accuracy and the Xtensa peripheral still require hardware verification.

The existing `no_std_parity` module also ran as RV32IMC code against this ABI. Its three guarded waits took the expected timeout, cancellation, and timeout branches. Two other paths still diverge from the interpreter. The emitted program sends a value, closes its sender, then creates the receive task. Embedded `prime_close` discards the queued value, so the Result probe takes its empty-channel branch and the Option probe receives None. These paths require channel close/drain semantics to preserve queued messages. The C3 string constructor is still a stub, so the probe checks which error branch ran and which string the compiler requested.

The RV32 language probe used LLVM's `-mtriple=riscv32 -mattr=+m,+c` and an emulator linker script. The installed upstream `llc` rejects the CLI's `-march=rv32imc`, and the default firmware link requests the Xtensa startup symbol. A bootable C3 image was outside this ABI test.
