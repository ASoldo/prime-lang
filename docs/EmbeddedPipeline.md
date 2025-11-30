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
- Runtime disables watchdogs once, uses a calibrated busy-loop delay for sleep, and ring-buffered prints.
- `out(...)`, channels, async `sleep_task`/`recv_task`, and GPIO built-ins are supported in no_std for ESP32.
