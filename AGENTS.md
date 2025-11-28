• Here’s a focused plan to add “embedded-only” GPIO/delay built-ins for ESP32-C3 (RISC-V) without clogging the normal host
  pipeline:

  1. Add embedded built-ins gated by a feature/target switch

  - Define new built-ins: pin_mode(pin: int32, mode: int32), digital_write(pin: int32, level: int32), delay_ms(ms: int32).
  - Only enable them when building for an embedded target; in host/std builds they either stay hidden or error with “embedded-
    only”.
  - Mark them as std-only forbidden in no_std unless the embedded target is selected.

  2. Add a target flag to prime-lang build

  - New CLI flag --target <triple>; default stays host.
  - If target is riscv32imc-unknown-none-elf (ESP32-C3), we:
      - Emit LLVM IR for that triple.
      - Invoke the RISC-V cross toolchain (riscv32-esp-elf or riscv64-unknown-elf with correct multilib) instead of host gcc.
      - Link against a small runtime staticlib compiled for that target.

  3. Provide an embedded Platform + FFI shim

  - Add Esp32Platform implementing Platform with FFI stubs to the ESP-IDF HAL for: GPIO set mode/output, delay_ms (using esp_timer
    or a busy wait), and a monotonic now_ms.
  - Wire a --platform esp32 or environment flag to install this platform at startup when the target is the ESP32 triple.
  - Keep std platform as default so host builds remain unchanged.

  4. Runtime FFI surface for embedded built-ins

  - In runtime/abi.rs, add FFI functions for pin_mode, digital_write, delay_ms, and expose them to the interpreter’s built-in
    dispatch under the embedded-target gate.
  - In build-mode (language/build.rs) add effect types for these ops so build snapshots remain deterministic (or simply error in
    build-mode for embedded, if we only care about emitted binaries).

  5. Typechecker/LSP visibility

  - Extend the typechecker’s built-in set to include the new GPIO/delay ops only when compiling with --target set to the embedded
    triple (or a new feature flag).
  - In no_std modules on an embedded target, the GPIO/delay built-ins are allowed; on host/no-std they error with “embedded-only
    built-in”.
  - Import checking is already in place for no_std, so LSP will surface unsupported built-ins and non-no-std dependencies.

  6. Demo sketch

  - Add workspace/demos/esp32_blink with no_std = true and a simple main:

    module demos::esp32_blink;

    fn main() {
      let int32 led = 2;      // adjust to board pin
      pin_mode(led, 1);       // 1 = output
      loop {
        digital_write(led, 1);
        delay_ms(500);
        digital_write(led, 0);
        delay_ms(500);
      }
    }
  - Document expected pin numbers for your devboard.

  7. Tooling prerequisites (outside this repo, but needed to test)

  - Install Espressif RISC-V toolchain (ESP-IDF or riscv32-esp-elf-*) and point the CLI to it via env (e.g., PRIME_RISCV_CC,
    PRIME_RISCV_AR, PRIME_RISCV_OBJCOPY).
  - Provide a linker script/startup object (from ESP-IDF) for the chosen chip.
  - For AVR/Arduino later: analogous --target avr-unknown-unknown, AVR GCC toolchain, and an ArduinoPlatform.



## Resume Session
```sh
codex resume 019acad0-f9eb-7c82-aab8-c0590145f59f --full-auto
```
