# Embedded Build & Flash Pipeline

End-to-end view for the ESP32 blink demo, including toolchains, scripts, and flashing.

```mermaid
flowchart TD
  SRC["workspace/demos/esp32_blink/esp32_blink.prime"] --> PARSE["Parse/typecheck"]
  PARSE --> IR["LLVM IR (-mtriple=xtensa-esp32-elf)"]
  IR --> LLC["llc -mcpu=esp32 -mattr=+windowed"]
  LLC --> OBJ["esp32_blink.o"]

  subgraph Scripts & Toolchain
    SECTIONS["app sections.ld"] --> LINK
    MEMORY["memory.ld"] --> LINK
    ROM["esp32.rom*.ld"] --> LINK
    TOOL["xtensa-esp32-elf-gcc"] --> LINK
  end

  subgraph Runtime
    ABI["libruntime_abi.a (Xtensa)"] --> LINK
  end

  OBJ --> LINK["Link"]
  LINK --> ELF["esp32_blink (ELF)"]
  ELF --> OBJCOPY["xtensa-esp32-elf-objcopy -> .bin"]
  OBJCOPY --> FLASH["esptool flash 0x10000"]
  FLASH --> DEVICE["ESP32 UART @115200"]
```

Notes:
- Manifest supplies toolchain/env and linker scripts; defaults fall back to `~/.espressif`.
- Build uses `-relocation-model=static` for llc plus `-mcpu=esp32 -mattr=+windowed`; linking pulls `libruntime_abi.a` (Xtensa) plus libc/libgcc.
- Flashing prefers `esptool elf2image` to produce headers; falls back to objcopy when elf2image is unavailable.
- Runtime disables watchdogs once, uses ring-buffered prints, and drives `recv_task`/`recv_timeout` with a tiny waiter queue + poll (configurable via `[build.runtime]` or `PRIME_RT_*`). The default poll is 1–2ms; expect ~2–3ms wake latency on Xtensa.
- Default LED strap uses GPIO2 (active-low on many dev boards); override pin/level in the demo if your board differs.
- `out(...)`, channels, async `sleep_task`/`recv_task`, `prime_reference_read`, and GPIO built-ins are supported in no_std for ESP32.
