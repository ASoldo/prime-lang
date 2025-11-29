# Prime Architecture Guide

This guide gives maintainers a visual map of how the CLI, compiler, runtime, and embedded toolchain fit together. Diagrams use Mermaid so you can preview them directly in viewers that support it.

## Top-Level Components

```mermaid
graph TD
  A["CLI (src/main.rs)"] -->|Commands| B["Project Loader<br/>Manifest handling"]
  A -->|Subcommands| C["Formatter/Lint/Docs<br/>(src/tools)"]
  A -->|Run| D["Interpreter<br/>(src/runtime/interpreter.rs)"]
  A -->|Build| E["Compiler & LLVM<br/>(src/language)"]
  E -->|ABI Decls| F["Runtime ABI<br/>(src/language/runtime_abi.rs)"]
  E -->|IR-&gt;obj| G[LLC]
  G -->|Link| H["Binary/ELF + runtime lib"]
  H -->|Flash (optional)| I["ESP32 Device"]
  B -->|Manifests| J["workspace/*/prime.toml"]
  D -->|Builtin FFI| K["Runtime ABI<br/>(src/runtime/abi.rs)"]
```

## CLI Command Flow

```mermaid
flowchart TD
  subgraph CLI
    R[run] --> PARSE1[Parse + expand macros]
    BLD[build] --> PARSE2[Parse + expand macros]
    EXP[expand] --> PARSE3[Parse + expand macros]
  end

  PARSE1 --> TC1[Typecheck]
  PARSE2 --> TC2[Typecheck]
  PARSE3 --> OUT3[Print expanded module]

  TC1 --> INT["Interpreter (runtime/interpreter.rs)"]
  TC2 --> COMP["Compiler (language/compiler.rs)"]

  COMP --> IR[LLVM IR]
  IR --> LLC["llc -&gt; object"]
  LLC --> LINK["Link with runtime staticlib"]
  LINK --> BIN[".build.prime/&lt;name&gt;/output"]
  BIN --> RUN["Run binary / flash"]
```

## Build Pipeline (Host vs ESP32)

```mermaid
flowchart LR
  SRC[".prime source"] --> PARSE[parser/AST]
  PARSE --> TYPE[typecheck]
  TYPE --> EFFECTS["build effects (channels/out/defer)"]
  EFFECTS --> LLVM["LLVM IR"]
  LLVM --> LLC_HOST["llc -mtriple=host"]
  LLVM --> LLC_ESP["llc -mtriple=xtensa-esp32-elf -mcpu=esp32 -mattr=+windowed"]
  LLC_HOST --> OBJ_HOST["host .o"]
  LLC_ESP --> OBJ_ESP["xtensa .o"]

  subgraph Runtime Staticlib
    ABI["build src/runtime/abi.rs"]
    ABI --> LIB_HOST["libruntime_abi.a (host)"]
    ABI --> LIB_ESP["libruntime_abi.a (xtensa)"]
  end

  OBJ_HOST --> LINK_HOST["gcc link"]
  LIB_HOST --> LINK_HOST
  LINK_HOST --> BIN_HOST[".build.prime/output/output"]

  OBJ_ESP --> LINK_ESP["xtensa-esp32-elf-gcc link"]
  LIB_ESP --> LINK_ESP
  LINK_ESP --> ELF[esp32_blink]
  ELF --> FLASH["esptool flash (optional)"]
```

## Interpreter vs Build (Semantics Parity)

```mermaid
graph TD
  P[Parsed + typed program] --> INT[Interpreter]
  P --> COMP[Compiler]
  INT -->|executes| RES_RUN[Runtime values/prints]
  COMP --> LLVM[LLVM IR] --> BIN[Binary]
  BIN -->|executes| RES_BUILD[Runtime values/prints]
  RES_RUN -. parity .-> RES_BUILD
```

Both modes share the same AST/type system; build mode records effects (e.g., `out`, channels) and emits equivalent code. Concurrency (spawn/join/channel) is deterministic in build snapshots and mapped to OS threads in the emitted binary.

## Embedded Runtime Highlights (src/runtime/abi.rs)

- `no_std` Xtensa stubs: entry (`call_user_start_cpu0`), BSS/data init, GPIO2 setup, ROM delay/printf bindings.
- `out(...)` support: strings, format strings, ints (constants), bools; each call newline-terminated.
- Tiny ring buffers for string storage to avoid print loss in tight loops.
- Watchdogs disabled once at boot for the demo (RTC + TIMG WDTs); remove if you need watchdog coverage.

## Manifest and Toolchain (workspace/demos/esp32_blink/prime.toml)

```mermaid
graph TD
  M["prime.toml (demo)"] --> TGT["target=xtensa-esp32-espidf"]
  M --> PLAT["platform=esp32"]
  M --> TC["build.toolchain (xtensa-esp-elf)"]
  M --> ENV["build.toolchain.env (esp-clang paths, RUSTUP_TOOLCHAIN=esp, CARGO_TARGET_DIR)"]
  M --> FLASH["build.flash (port/baud/address)"]
  TC --> SCRIPTS["sections.ld + memory.ld + ROM ld"]
  ENV --> PATHS["PATH/LLVM_SYS_201_PREFIX/LD_LIBRARY_PATH"]
```

Defaults: if env vars are absent, the CLI auto-detects esp-clang/xtensa toolchains under `~/.espressif`, sets `RUSTUP_TOOLCHAIN=esp`, and caches artifacts in `~/.cache/prime-xtensa`.

## Repo Pointers

- Compiler: `src/language/compiler.rs` (LLVM emission), `src/language/runtime_abi.rs` (decls), `src/language/typecheck.rs`
- Interpreter: `src/runtime/interpreter.rs`
- Embedded ABI: `src/runtime/abi.rs` (Xtensa `no_std`, GPIO, prints, watchdog handling)
- CLI/wrappers: `src/main.rs` (subcommands, runtime build/link), `src/project/*` (manifests)
- Docs: `src/docs/topics.rs` (CLI `prime-lang docs`)

Feel free to expand these diagrams as the architecture evolves.
