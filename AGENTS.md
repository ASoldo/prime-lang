# prime-lang: Current State & Roadmap

• Current Capabilities

  - Language core: Modules/imports, structs/enums (sum types), interfaces/impls, generics, pattern matching (tuples, maps,
    structs, slices, enums with guards), references/&mut, defer, range/collection loops, tuples/multiple returns, Result/Option
    with try {} and ?.
  - Tooling: Single CLI binary (run, build, lint, fmt, docs, init, add, lsp); LSP with hover/defs/refs/rename/format; formatter;
    docs topics tied to runnable demos; manifest-driven module graph (prime.toml).
  - Runtime/build: Interpreter and LLVM build mode aligned; lint/typecheck before run/build; examples all lint/run clean; tree-
    sitter grammar + Neovim queries maintained.
  - Diagnostics: Parser/typechecker errors with spans, unused-variable warnings (ignores _), manifest drift warnings; lint mirrors
    LSP diagnostics.

  Gaps & Must-Have Next

  1. Type system depth: No explicit ownership/borrowing model beyond simple checks; no lifetimes or move semantics. Need a clear
     ownership story or stronger borrow checker to avoid silent heap aliasing issues.
  2. Stdlib completeness: Slices/maps are minimally ergonomic; missing richer collections (iterators, sets, strings utilities,
     math). Add core stdlib to avoid hand-rolling basics.
  3. Error handling ergonomics: Result/Option exist, but no panic!/assert, no error types beyond strings. Add richer error types
     and backtraces for runtime failures.
  4. Module/package management: Manifest exists but no versioning/deps; consider dependency resolution or at least local path deps
     and prime-lang add that handles external modules.
  5. Build tooling: Tests are limited to Rust side; no first-class prime-lang test to run .prime test files or golden outputs. Add
     a native test runner and fixtures to lock behavior.
  6. IDE/LSP coverage: Go-to-type/implementation, document symbols on interfaces/impls, diagnostics for duplicate symbols across
     modules, and hover that shows parameter vs function clarity. Strengthen cross-file symbol index (current preload is best-
     effort).
  7. Formatter consistency: Nested literals just improved; ensure formatter round-trips all constructs (interfaces with typed
     self, map/array inline/expanded, blocks with tails). Add formatter golden tests.
  8. Runtime safety: No story for overflow, NIL/None/default; consider option unwrap diagnostics, guarded division, or a safety
     mode with traps.
  9. Concurrency/asynchrony: Absent; if planned, need syntax + runtime model (futures, async/await, channels) or explicitly defer.
  10. Docs & discoverability: Great demos, but missing a concise language reference/spec. Add a “Prime by example” plus a minimal
     spec (grammar, typing, runtime model) and a troubleshooting section for common errors.

  Actionable Next Steps

  1. Add a prime-lang test command + fixtures; wire into CI.
  2. Strengthen ownership/borrowing rules or document the current heap/aliasing semantics; add diagnostics for common footguns
     (re-borrows, dangling refs).
  3. Expand stdlib (collections, string/math helpers) and document API via prime-lang docs.
  4. Enhance LSP: distinguish params vs funcs uniformly, cross-file symbols, go-to-impl/type, better hover formatting; add
     regression tests for lookups.
  5. Formatter golden tests to lock inline/expanded literal rules and interface self: T.
  6. Manifest/deps: add local path deps and prepare for versioned modules or at least detect duplicates/conflicts.
  7. Publish a concise language reference/spec and a “getting started” guide that maps each feature to a runnable demo.
