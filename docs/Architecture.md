# Prime Architecture Guide

This guide gives maintainers a visual map of how the CLI, compiler, runtime, and embedded toolchain fit together. Diagrams use Mermaid so you can preview them directly in viewers that support it.

## Top-Level Components

```mermaid
graph TD
  A["CLI (src/main.rs)"] --> PL["Project loader<br/>manifests"]
  A --> TOOLS["Formatter / Lint / Docs<br/>(src/tools)"]
  A --> RUN["Run -> Interpreter<br/>(src/runtime/interpreter.rs)"]
  A --> BUILD["Build -> Compiler/LLVM<br/>(src/language)"]
  A --> LSP["LSP server<br/>(src/lsp/server.rs)"]

  PL --> MAN["workspace/*/prime.toml"]

  RUN --> RT_HOST["Runtime ABI (host interp)<br/>(src/runtime/abi.rs)"]

  BUILD --> ABI_DECL["ABI decls in IR<br/>(src/language/runtime_abi.rs)"]
  ABI_DECL --> IR["IR/object"]
  ABI_SRC["Runtime staticlib source<br/>(src/runtime/abi.rs)"] --> RT_HOST_LIB["libruntime_abi.a (host)"]
  ABI_SRC --> RT_ESP_LIB["libruntime_abi.a (xtensa)"]

  IR --> LINK["Link"]
  RT_HOST_LIB --> LINK
  RT_ESP_LIB --> LINK
  LINK --> BIN_HOST["Host binary"]
  LINK --> BIN_ESP["ESP32 ELF/bin"]
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

Notes:
- Host builds default to PIC; Xtensa builds force `-relocation-model=static` and pass
  `-mcpu=esp32 -mattr=+windowed -mtriple=xtensa-esp32-elf` to match the ESP toolchain.
- Linking the embedded binary pulls `libruntime_abi.a` (Xtensa) plus libc/libgcc; `esptool` uses `elf2image` when available, with objcopy as a fallback.

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
Async/await + channels always attach runtime handles when async code is present, so
`recv_task`/`sleep_task` block correctly in build mode, the host binary, and Xtensa.
Non-constant comparisons now lower to LLVM `icmp`/`fcmp` with SSA-backed booleans, keeping `if`/`while`/pattern conditions working even when values are only known at runtime.

## Embedded Runtime Highlights (src/runtime/abi.rs)

- `no_std` Xtensa runtime: entry (`call_user_start_cpu0`), BSS/data init, GPIO mux for common LED pins (2/4/5), calibrated delays for `sleep_task`, ROM printf bindings.
- `out(...)` plus channels and async tasks (`sleep_task`/`recv_task`) work in no_std; configurable static pools (`[build.runtime]` or `PRIME_RT_*`) back channels/tasks with a waiter queue + tiny poll for `recv_task`/`recv_timeout`.
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

## Memory Model & ABI Stability

- Channels are FIFO with at-most-once delivery; sends/recvs honor program order on host and embedded. Xtensa async waits wake within the poll interval (default 1–2ms), so plan for ~2–3ms latency when waiting on channels.
- Build snapshots preserve channel inboxes and join handles to keep build/run parity; non-determinism is limited to I/O or explicit parallel builds (`PRIME_BUILD_PARALLEL=1`).
- Runtime ABI symbols are shared between host and embedded binaries via `libruntime_abi.a`; the frozen Xtensa toolchain and linker recipe lives in `docs/esp32_toolchain_template.toml` (-relocation-model=static + libc/libgcc).

## Repo Pointers

- Compiler: `src/language/compiler.rs` (LLVM emission), `src/language/runtime_abi.rs` (decls), `src/language/typecheck.rs`
- Interpreter: `src/runtime/interpreter.rs`
- Embedded ABI: `src/runtime/abi.rs` (Xtensa `no_std`, GPIO, prints, watchdog handling)
- CLI/wrappers: `src/main.rs` (subcommands, runtime build/link), `src/project/*` (manifests)
- Docs: `src/docs/topics.rs` (CLI `prime-lang docs`)

Feel free to expand these diagrams as the architecture evolves.
