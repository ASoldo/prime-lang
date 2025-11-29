  Current core features

  - Modules/libraries/tests with manifest-based workspace loading (prime.toml), preludes, visibility checks.
  - Strong static type system: structs/enums (with variants), tuples, ranges, pointers, references, slices, maps, arrays; numeric/
    bool/string/rune primitives; type inference for literals.
  - Pattern matching across tuples/maps/structs/slices; destructuring in loops/ifs/matches; Option/Result with try {} / ?.
  - Ownership/borrowing: mutable/immutable refs, Box, Drop (RAII), defers; move semantics enforced by the checker.
  - Functions + macros: expression/item macros with hygiene controls, tokens/blocks/repeats, param quantifiers, expansion tracing,
    CLI expand.
  - Concurrency: spawn/join, channels (send/recv/close/recv_timeout), deterministic in build snapshots; threads in emitted
    binaries.
  - Built-ins: out/in, fs (read/write/exists), time (sleep/now), iterators (slice/map), formatting (out with strings/format
    strings/ints/bools on embedded).
  - Tooling: interpreter and LLVM build with parity; formatter/lint/docs; LSP (hover/completion/diagnostics); CLI subcommands for
    run/build/lint/fmt/docs/init/add/test/expand.
  - Embedded support: ESP32 Xtensa no_std runtime (GPIO2 blink demo), ROM delay/printf, ring-buffered prints, watchdog disable for
    the demo, manifest-driven toolchains/scripts/flash.

  V1.0 suggestions (gap check)

  1. Error handling & diagnostics:
      - Stabilize error codes/messages; ensure consistent, actionable diagnostics (type errors, borrow errors, macro errors).
      - Consider panic semantics and unwind/abort strategy in build/runtime; define a clear contract.
  2. Standard library surface:
      - Minimal, well-scoped core modules (collections, math, string utilities, option/result helpers).
      - File/path utilities and time/timers defined in-language (beyond host-specific stubs).
  3. Formatting & printing:
      - Unified formatting story (host and embedded) with a small, consistent subset; ensure out supports the same value shapes
        across targets (or documented deltas).
  4. Concurrency & async:
      - Clarify memory model and ordering guarantees for channels/threads; document determinism expectations.
      - Decide whether async/await is in/out of scope for 1.0; if out, make channels/spawn fully stable.
  5. FFI & embedding:
      - Formalize the runtime ABI surface (host and embedded) with versioning; consider a small FFI for host builds.
      - Document stability guarantees for ABI symbols and target triples.
  6. Toolchain stability:
      - Lock host llc/linker behavior; ensure default triples are reliable across platforms.
      - For embedded, freeze known-good toolchain/script versions and document required layout (or ship templates).
  7. Testing/story for users:
      - First-class test runner semantics (fixtures, scripted input already present, but consider assertions/helpers).
      - Golden-output snapshots for key demos; CI smoke on both host and embedded (emulated where possible).
  8. Language spec:
      - A concise reference of syntax/semantics (expressions, patterns, ownership rules, type inference edges, macro hygiene
        rules).
  9. Stability/interop:
      - Stabilize module/import rules and visibility; ensure formatter is idempotent and LSP matches parser/lint.
