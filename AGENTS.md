  1. Closure metadata fidelity (highest priority)

  - Always use typechecker-populated captures; remove or gate opaque capture synthesis. If captures are empty, derive them
    structurally from free-vars and annotate with real types (using typechecker info), not opaque placeholders.
  - When reconstructing closure values from runtime structs (env, fn, id), preserve the original signature/arity. Reject or patch
    up mismatches rather than defaulting to empty signatures.

  2. Env allocation & lifetime

  - Keep envs heap-allocated (already done) but centralize allocation (helper) to avoid mistakes; document lifetime as process-
    long for now.

  3. Type coverage parity

  - Extend closure value_to/from_llvm to support:
      - bool and string values properly (no placeholder bool-as-int; string shouldn’t be opaque).
      - Structs/tuples of structs, slices/maps/boxes if allowed; otherwise emit clear errors.
  - Verify multi-return tuple lowering works for closures and calls.

  4. Call correctness

  - Ensure fn pointer bitcasts use the correct function type derived from the closure’s signature. Validate arity before calls and
    error clearly on mismatch.

  5. Diagnostics and safety nets

  - Emit clear errors when captures are missing types after typecheck.
  - Add a debug flag to dump closure capture sets/signatures during codegen for tricky cases.
  - Remove unreachable patterns/warnings (formatter unreachable arm, dead CaptureMode::Reference, unused ClosureValue::ret).

  6. Tests

  - Unit/integration:
      - Higher-order closures returning closures (correct return values).
      - Nested closures capturing outer vars and returning closures.
      - Multi-capture, tuple return closure.
      - Closure passed through a tuple/struct and invoked later.
  - Demo: keep the enriched closure demo; add an assertion or output check in the test harness.

  7. Docs/CLI

  - README/CLI docs: describe build-mode closure support, env layout {env_ptr, fn_ptr}, calling convention (fn(env, args…)),
    capture semantics (move-only), current limitations (no destructors, process-lifetime env), and examples (higher-order, tuple
    returns, nested captures).

  Execution order: 1 → 4 → 3 → 5 → 6 → 7, verifying prime-lang build workspace/demos/closures/closure_demo.prime --name
  closures_demo after each major fix.
