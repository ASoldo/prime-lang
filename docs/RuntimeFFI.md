# Runtime & FFI Surfaces

How the interpreter, compiler, and runtime ABI interact across host and Xtensa targets.

```mermaid
graph LR
  COMP["Compiler (build mode)"] --> ABI_DECL["ABI decls in IR"]
  ABI_DECL --> BIN["Binary links libruntime_abi.a"]
  INT["Interpreter (run mode)"] --> CALLS["Builtin calls (out, channel, sleep)"]
  CALLS --> HOST_RT["Host runtime (interpreter structures)"]
  BIN --> DEVICE["Host or ESP32"]

  subgraph "Staticlib (per target)"
    ABI_SRC["src/runtime/abi.rs"] --> HOST_LIB["libruntime_abi.a (host)"]
    ABI_SRC --> XTENSA_LIB["libruntime_abi.a (xtensa)"]
  end

  HOST_LIB -. exports .-> ABI_DECL
  XTENSA_LIB -. exports .-> ABI_DECL
  DEVICE --> UART["UART/ROM bindings"]
```

Highlights:
- Interpreter uses its own in-process runtime; build mode invokes the ABI for async/channels when async appears (runtime handles are auto-attached).
- The same ABI signatures are declared in IR; linking picks the correct staticlib for the target. Xtensa builds request `-relocation-model=static` and link libc/libgcc alongside `libruntime_abi.a`.
- Xtensa runtime (no_std) includes async tasks, channels, calibrated delays (busy loop), GPIO mux for common LED pins, ring-buffered strings, watchdog disable, and basic reference helpers (`prime_reference_read` passthrough).
