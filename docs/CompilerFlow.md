# Prime Compiler Flow (Detailed)

This diagram traces a build from parsed AST through LLVM IR to the linked binary, highlighting where type info, effects, and ABI calls are applied.

```mermaid
flowchart TD
  SRC[".prime source"] --> PARSE["Parser + AST (src/language/ast.rs)"]
  PARSE --> MACRO["Macro expand (src/language/macro_expander.rs)"]
  MACRO --> TYPE["Typecheck (src/language/typecheck/checker.rs)"]
  TYPE --> EFFECTS["Build effects recorded (out/channel/defer)"]
  EFFECTS --> LOWER["Lowering to LLVM (src/language/compiler/mod.rs)"]
  LOWER --> ABI["Runtime ABI decls (src/language/runtime_abi.rs)"]
  ABI --> IR["LLVM IR module"]
  IR --> LLC["llc (target-aware)"]
  LLC --> OBJ["Object file (.o)"]

  subgraph Runtime Lib
    ABI_SRC["src/runtime/abi.rs"] --> RT_LIB_HOST["libruntime_abi.a (host)"]
    ABI_SRC --> RT_LIB_ESP["libruntime_abi.a (embedded target)"]
  end

  OBJ --> LINK_HOST["Linker (host gcc/clang)"]
  RT_LIB_HOST --> LINK_HOST
  LINK_HOST --> BIN_HOST["Host binary"]
  BIN_HOST --> RUN_HOST["Run on host"]

  OBJ --> LINK_ESP["Espressif target linker"]
  RT_LIB_ESP --> LINK_ESP
  LINK_ESP --> BIN_ESP["ESP32 or ESP32-C3 ELF and image"]
  BIN_ESP --> RUN_ESP["Flash / run on device"]
```

Host builds link the bundled Rust runtime for graphics, audio, channels and tasks. Embedded builds link a `no_std` runtime with fixed-capacity storage. Classic ESP32 uses Espressif LLVM and Xtensa tools. ESP32-C3 uses RV32IMC LLVM code and the ILP32 ABI. The link step uses the selected target's startup and memory layout. [Embedded pipeline](EmbeddedPipeline.md)

Host async blocks run on worker threads. Embedded async blocks use deferred evaluation. On both targets, timer and receive tasks can be polled for completion. [Task semantics](AsyncAwait.md)

Nonconstant integer and float comparisons emit LLVM `icmp` and `fcmp` instructions. Their boolean results can drive native control flow.
