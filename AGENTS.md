Current core features

- Modules/libraries/tests with manifest-based workspace loading (prime.toml), preludes, visibility checks.
- Strong static type system: structs/enums (variants), tuples, ranges, pointers, references, slices, maps, arrays; numeric/bool/string/rune primitives; literal type inference.
- Pattern matching across tuples/maps/structs/slices; destructuring in loops/ifs/matches; Option/Result with try {} / ?.
- Ownership/borrowing: mutable/immutable refs, Box, Drop (RAII), defers; move semantics enforced by the checker.
- Functions + macros: expression/item macros with hygiene controls, tokens/blocks/repeats, param quantifiers, expansion tracing, CLI expand.
- Concurrency: spawn/join, channels (send/recv/close/recv_timeout), deterministic in build snapshots; threads in emitted binaries. Async/await + channels auto-attach runtime handles when async is present so `recv_task`/`sleep_task` block consistently across build/run/embedded.
- Built-ins: out/in, fs (read/write/exists), time (sleep/now), iterators (slice/map), formatting (out with strings/format strings/ints/bools on embedded).
- Tooling: interpreter and LLVM build with parity; formatter/lint/docs; LSP (hover/completion/diagnostics); CLI subcommands for run/build/lint/fmt/docs/init/add/test/expand.
- Embedded support: ESP32 Xtensa no_std runtime (GPIO mux for 2/4/5, calibrated busy-loop delay, watchdog disable for demo), async/await + channels with small static pools, ring-buffered prints, manifest-driven toolchains/scripts/flash.

V1.0 suggestions (gap check)

1. Error handling & diagnostics:
   - Stabilize error codes/messages; ensure consistent, actionable diagnostics (type errors, borrow errors, macro errors).
   - Consider panic semantics and unwind/abort strategy in build/runtime; define a clear contract.
2. Standard library surface:
   - Minimal, well-scoped core modules (collections, math, string utilities, option/result helpers).
   - File/path utilities and time/timers defined in-language (beyond host-specific stubs).
3. Formatting & printing:
   - Unified formatting story (host and embedded) with a small, consistent subset; ensure out supports the same value shapes across targets (or documented deltas).
4. Concurrency & async:
   - Clarify memory model and ordering guarantees for channels/threads; document determinism expectations.
   - Async/await now emits in build mode and links on host/ESP32; runtime handles are attached automatically when async is present so `recv_task`/`sleep_task` block in build/run/embedded. Embedded runtime still uses calibrated delays and small pools; consider configurable sizing and documented latency expectations.
5. FFI & embedding:
   - Formalize the runtime ABI surface (host and embedded) with versioning; consider a small FFI for host builds.
   - Document stability guarantees for ABI symbols and target triples.
6. Toolchain stability:
   - Lock host llc/linker behavior; ensure default triples are reliable across platforms.
   - For embedded, freeze known-good toolchain/script versions and document required layout (or ship templates); Xtensa builds now force `-relocation-model=static` and link libc/libgcc with `libruntime_abi.a`.
7. Testing/story for users:
   - First-class test runner semantics (fixtures, scripted input already present, but consider assertions/helpers).
   - Golden-output snapshots for key demos; CI smoke on both host and embedded (emulated where possible).
8. Language spec:
   - A concise reference of syntax/semantics (expressions, patterns, ownership rules, type inference edges, macro hygiene rules).
9. Stability/interop:
   - Stabilize module/import rules and visibility; ensure formatter is idempotent and LSP matches parser/lint.

Next steps for v1.0.0

- Embedded async/channel parity: current `recv_task` busy-waits with calibrated delay; add proper wait queues or configurable polling/backoff plus pool sizing knobs.
- Embedded delay accuracy: provide a timer-backed delay or IDF shim and auto-detect CPU frequency; keep busy-loop as fallback.
- GPIO mux coverage: extend mux setup beyond pins 2/4/5 or expose config; document active-low defaults and strap-pin caveats (GPIO2/0/12/15).
- Formatter polish: add regression coverage for async initializers and multiline lets; keep indentation stable.
- Docs/diagnostics: document embedded async/channel limits, runtime-handle defaults for async, and supported built-ins; update strap-pin and LED-pin guidance in blink demo docs.
- CI/tests: add smoke for async/await + channels on host and embedded (or emulated), and formatter golden outputs.
