# Prime Compiler Flow (Detailed)

This diagram traces a build from parsed AST through LLVM IR to the linked binary, highlighting where type info, effects, and ABI calls are applied.

```mermaid
flowchart TD
  SRC[".prime source"] --> PARSE["Parser + AST (src/language/ast.rs)"]
  PARSE --> MACRO["Macro expand (src/language/macro_expander.rs)"]
  MACRO --> TYPE["Typecheck (src/language/typecheck.rs)"]
  TYPE --> EFFECTS["Build effects recorded (out/channel/defer)"]
  EFFECTS --> LOWER["Lowering to LLVM (src/language/compiler.rs)"]
  LOWER --> ABI["Runtime ABI decls (src/language/runtime_abi.rs)"]
  ABI --> IR["LLVM IR module"]
  IR --> LLC["llc (target-aware)"]
  LLC --> OBJ["Object file (.o)"]

  subgraph Runtime Lib
    ABI_SRC["src/runtime/abi.rs"] --> RT_LIB_HOST["libruntime_abi.a (host)"]
    ABI_SRC --> RT_LIB_ESP["libruntime_abi.a (xtensa)"]
  end

  OBJ --> LINK_HOST["Linker (host gcc/clang)"]
  RT_LIB_HOST --> LINK_HOST
  LINK_HOST --> BIN_HOST["Host binary"]
  BIN_HOST --> RUN_HOST["Run on host"]

  OBJ --> LINK_ESP["Linker (xtensa-esp32-elf-gcc)"]
  RT_LIB_ESP --> LINK_ESP
  LINK_ESP --> BIN_ESP["ESP32 ELF/bin"]
  BIN_ESP --> RUN_ESP["Flash / run on device"]
```

Key notes:
- The compiler reuses type info and build effects so `out(...)`, channels, and defers behave the same in build snapshots and the emitted binary.
- ABI declarations are inserted per module; the linked staticlib provides the concrete implementations (host or Xtensa).
- Target selection: host builds use the host triple; ESP32 uses `xtensa-esp32-elf` with `+windowed` attr and `-relocation-model=static` so llc stays compatible with the Xtensa toolchain.
- Async/await + channels always attach runtime handles when async appears, so `recv_task`/`sleep_task` block through the runtime ABI in build mode and in emitted binaries (host/ESP32).
- Scalar comparisons are lowered even when operands are non-constant: ints/floats emit `icmp`/`fcmp` and produce SSA-backed `BoolValue`s so control flow remains valid in build mode while still folding constants where possible.
