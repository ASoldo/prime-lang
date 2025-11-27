  - Lifetime & cleanup: verify all build-time closure envs are freed (done) and align run-time drop semantics; add stress test for
    many closures.
  - Capture semantics: ensure reference captures (immut/mut) match runtime borrow rules; support pointer/pattern captures if
    runtime allows.
  - Builtin parity: route slice/map method calls on captured handles through runtime for all ops (get/push/insert/len already
    wired; check remove, iter, etc., if present in run-mode).
  - Value coverage: allow captured Box/Reference/Sender/Receiver/JoinHandle to behave like runtime (reads/writes via handles,
    printing/formatting); block what runtime blocks.
  - Closure calls: allow closures to be stored/passed/returned uniformly; ensure tuple/multi-return and unit-return behavior
    matches runtime.
  - Borrow/escape checks: mirror runtime’s borrow checker behavior for captured references (no use-after-move, no borrow
    conflicts); add diagnostics coverage.
  - Defer/control flow: ensure deferred actions and control-flow restrictions inside closures align with runtime expectations (or
    document differences).
  - Docs/demos/tests: expand closure demo/tests to cover reference captures, handle mutation, nested/higher-order cases; add
    build-mode parity notes to README/AGENTS.
  - Build/run closure parity implemented: build-mode closures now support reference captures, runtime handle routing, and tuple/multi returns; see README “Build-Mode Closures”.
