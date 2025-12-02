# Troubleshooting Cheatsheet

Quick tips to unblock common issues when hacking on prime-lang or the ESP32 demo.

## Host builds
- Wrong target triple/object format: ensure `prime-lang` is up to date (`cargo install --path . --force`) so `llc` uses the host triple (defaults to `llvm-config --host-target`). Check `llc --version` and `file .build.prime/output/output.o`.
- Stale artifacts: remove `.build.prime/output` or set a fresh `CARGO_TARGET_DIR` to rebuild cleanly.

## ESP32 builds
- Toolchain/env: confirm `$PATH` includes `~/.espressif/tools/xtensa-esp-elf/.../bin` and `LLVM_SYS_201_PREFIX` points to esp-clang. The demo manifest already sets these; override via `[build.toolchain.env]` if your layout differs.
- Linker scripts: verify `sections.ld`/`memory.ld` paths in `workspace/demos/esp32_blink/prime.toml` match your IDF build output. ROM ld files must match the Xtensa toolchain (little-endian).
- `windowed` warnings: suppressed via `-Aunstable-features`; safe to ignore with current esp-clang/xtensa setup.
- Resets/boot spam: watchdogs are disabled once in the demo runtime. If you re-enable them, add feeds or widen timeouts; frequent resets print the bootloader header repeatedly.
- Missing UART prints: `out(...)` on Xtensa supports strings/format strings/ints/bools only; each call prints with a newline. Ring-buffered storage prevents drops in tight loops.
- Host vs no_std feature set: `fs_exists`/`fs_read`/`fs_write` are host-only. Embedded/no_std keeps `out`, channels, async timers, GPIO, mutable scalars, and now `spawn`/`join` (mapped to the task runtime, not OS threads); set `no_std = true` in the manifest (see `workspace/demos/esp32_blink/prime.toml` or `workspace/demos/bare_metal_embedded/prime.toml`).

## Logging & diagnostics
- Add `PRIME_DEBUG_MEM=1` to print memory map/debug from the CLI when building embedded targets.
- Use `prime-lang expand <file> --line L --column C` to inspect macro expansions before typecheck/compile.

## Cleaning & caches
- Host: remove `.build.prime/output` or rerun `cargo install --path . --force`.
- Embedded: wipe `.build.prime/esp32_blink-*` and ensure `CARGO_TARGET_DIR=~/.cache/prime-xtensa` (set in the manifest) is writable; remove that dir to force a clean runtime rebuild.

## When in doubt
- Re-source IDF env: `. ~/esp/esp-idf/export.sh`
- Reinstall CLI: `cargo install --path . --force`
- Verify toolchains: `which xtensa-esp32-elf-gcc`, `llc --version` (should list Xtensa for esp builds)
