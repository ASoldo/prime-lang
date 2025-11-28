• Context

  - Goal: Build ESP32 (classic, Xtensa) blink demo with no_std runtime.
  - Current state: Runtime refactored to minimal no_std stubs; builds succeed. Build reaches link step but fails due to linker/
    script/toolchain mismatch.
  - Toolchains/scripts tried:
      - ESP32-specific toolchain (xtensa-esp32-elf, app.elf.ld/memory.elf.ld) → DRAM overflow/undefined entry, warnings about
        regions.
      - IDF toolchain (xtensa-esp-elf) with ESP32-specific llvm → earlier BE/LE mismatches, but runtime now tiny; BE crt0 also
        failed.
  - Current prime.toml uses xtensa-esp32-elf toolchain and toolchain scripts; ld_flags still contained IDF ROM scripts at one
    point, then switched back, still failing.

  What failed last

  - With xtensa-esp32-elf toolchain and app.elf.ld + memory.elf.ld: iram_seg warnings, DRAM overflow, missing _start.
  - A manual link with these scripts also warned about regions.
  - When using IDF toolchain + IDF scripts, big-endian crt0 caused mismatch; BE/LE conflict.

  Plan to switch back to IDF toolchain/scripts (next agent)

  1. Toolchain selection
      - Use IDF’s xtensa-esp-elf toolchain (the one under ~/.espressif/tools/xtensa-esp-elf/esp-15.2.0_...).
      - PATH: include only the IDF toolchain bin for linking: ~/.espressif/tools/xtensa-esp-elf/.../xtensa-esp-elf/bin.
      - CARGO_TARGET_XTENSA_ESP32_ESPIDF_LINKER = xtensa-esp-elf-gcc.
      - LLVM_SYS_201_PREFIX = IDF esp-clang with Xtensa backend (the one installed via idf_tools under ~/.espressif/tools/esp-
        clang/esp-clang). Ensure llc --version shows Xtensa.
  2. prime.toml
      - [build.toolchain]
          - cc = "xtensa-esp-elf-gcc"
          - ar = "xtensa-esp-elf-ar"
          - objcopy = "xtensa-esp-elf-objcopy"
          - ld_script = "/home/rootster/esp/esp-idf/examples/get-started/blink/build/esp-idf/esp_system/ld/sections.ld"
          - ld_flags = "-Wl,--gc-sections -T/home/rootster/esp/esp-idf/examples/get-started/blink/build/esp-idf/esp_system/ld/
            memory.ld"
            (keep it minimal; drop ROM/newlib/periph scripts unless needed)
          - startup_obj → remove/omit; let IDF scripts/toolchain supply startup.
      - Ensure no residual toolchain script paths from xtensa-esp32-elf remain.
  3. Linker adjustments
      - In link_esp32, avoid adding -L paths for the ESP32 toolchain; rely on IDF toolchain default search paths.
      - Keep -lc -lgcc.
      - Ensure -Wl,-EL/-mlongcalls are appropriate for Xtensa.
  4. Runtime
      - Already no_std with type constants and a panic handler; builds with -Zbuild-std=core,alloc for embedded.
      - prime_now_ms stub returns 0 (satisfies compiler).
      - Keep this minimal runtime for now.
  5. Build command (example)

     . ~/esp/esp-idf/export.sh
     env LLVM_SYS_201_PREFIX="$HOME/.espressif/tools/esp-clang/esp-clang" \
         LD_LIBRARY_PATH="/usr/lib64:/usr/lib:/lib:$HOME/.espressif/tools/esp-clang/esp-clang/lib" \
         PATH="$HOME/.espressif/tools/esp-clang/esp-clang/bin:$HOME/.espressif/tools/xtensa-esp-elf/esp-15.2.0_20250929/xtensa-esp-
  elf/bin:$PATH" \
         RUSTUP_TOOLCHAIN=esp \
         CARGO_TARGET_DIR="$HOME/.cache/prime-xtensa" \
         CARGO_TARGET_XTENSA_ESP32_ESPIDF_LINKER=xtensa-esp-elf-gcc \
         PRIME_DEBUG_MEM=1 \
         ./target/debug/prime-lang build workspace/demos/esp32_blink/esp32_blink.prime --name esp32_blink --no-flash
  6. If link still fails
      - Check for big-endian hints: ensure xtensa-esp-elf-gcc -v target is LE; if BE, locate an LE IDF toolchain or use ESP32-
        specific toolchain with its own scripts and custom memory.ld.
      - If _heap_low_start or region errors persist, simplify ld_flags to only memory.ld and sections.ld, no ROM scripts.
      - If scripts still mismatch, consider using IDF build outputs’ esp32_out.ld or preprocess sections.ld with ldgen from IDF to
        ensure region definitions exist.

  With these changes, the linker should use a consistent LE toolchain and matching scripts, eliminating region/endianness
  conflicts.
