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
- Interpreter uses its own in-process runtime; build mode only needs ABI shims plus values.
- The same ABI signatures are declared in IR; linking picks the correct staticlib for the target.
- Xtensa runtime uses ROM `ets_delay_us`/`ets_printf`, GPIO2 toggles, ring-buffered strings, and disables watchdogs once for the demo.
