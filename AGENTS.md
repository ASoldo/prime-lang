• Detailed plan to close the remaining closure gaps

  1. Env lifetime management (destructors/cleanup)

  - Compiler: introduce a closure-env drop path in build mode. Track allocated env handles and emit runtime free calls at end of
    build execution (or per-function scope if safe).
  - Runtime: add an env-free entry point (e.g., prime_env_free(ptr)) that frees malloc’d env structs; ensure handle-owned heap
    values are reference-counted or explicitly freed.
  - Tests: add a build-mode test that allocates many closures and asserts no double-free/crash and bounded malloc usage (if
    measurable).
  - Docs: document lifetime change and any caveats (still no user-defined destructors).

  2. Reference captures in build mode

  - Typecheck: allow CaptureMode::Reference { mutable } in build mode when the referenced value outlives the closure; reject
    otherwise with clear diagnostics.
  - Codegen: store a pointer to the original value in the env, adjust value_to_llvm_for_type/value_from_llvm to wrap/unwarp
    references. Update call sites to respect mutability and borrow rules.
  - Safety: enforce borrow checks so reference captures don’t outlive scopes; error on illegal captures.
  - Tests: add nested/higher-order closures that capture refs (mutable and immutable) and verify correct results and borrow
    errors.

  3. Heap handle mutation/read on captured handles

  - Runtime: add handle-friendly ops: prime_slice_get_handle, prime_slice_push_handle, prime_map_get_handle,
    prime_map_insert_handle that operate on captured handles.
  - Compiler: wire builtins (get/push/insert for slices/maps) to call the handle variants when a handle is present; keep current
    in-memory path when locally available.
  - Tests: add integration tests where a closure captures a slice/map, then get/push/insert is called on the captured handle and
    results are observable via debug_show/len.

  4. Docs and demos

  - README/AGENTS: update build-mode closures section to describe env lifetime changes, reference capture support, and handle-
    aware heap ops.
  - Demo: extend heap section to mutate captured slice/map (push, insert, get) and print results; show reference-capture example
    if enabled.
  - LSP: ensure method completions include new handle-aware ops (no change if names stay the same).

  Milestones/testing

  - After each major step, run: cargo run --quiet -- build workspace/demos/closures/closure_demo.prime --name closures_demo and
    targeted tests.
  - Add new integration tests for ref captures and handle mutations; consider a stress test for env freeing if feasible.

  This sequence keeps dependencies sane: 1 and 2 are most intrusive (safety/lifetime), 3 builds on runtime bridges, 4 is cleanup/
  docs.
