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
    ABI_SRC["src/runtime/abi.rs"] --> RT_LIB["libruntime_abi.a"]
  end

  OBJ --> LINK["Linker (gcc/xtensa-esp32-elf-gcc)"]
  RT_LIB --> LINK
  LINK --> BIN["Binary/ELF"]
  BIN --> RUN["Run / Flash"]
```

Key notes:
- The compiler reuses type info and build effects so `out(...)`, channels, and defers behave the same in build snapshots and the emitted binary.
- ABI declarations are inserted per module; the linked staticlib provides the concrete implementations (host or Xtensa).
- Target selection: host builds use the host triple; ESP32 uses `xtensa-esp32-elf` with `+windowed` attr.
