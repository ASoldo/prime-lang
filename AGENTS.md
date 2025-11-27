  1. Language surface

  - Allow impl Drop for Type { fn drop(&mut self) { ... } } or fn drop(&mut self) inside impl Type.
  - Restrict to no return values; allow &mut self and no captures.
  - Disallow panics during drop or treat them as runtime errors.

  2. Compiler/lowering

  - For stack-bound values, enqueue a deferred drop when the binding is created; run drops on scope exit (reuse defer stack).
  - For moves, transfer ownership and drop only once; moved-from bindings no longer schedule the drop.
  - For references/pointers/handles, either skip drop or allow a separate DropHandle trait for handle wrappers.
  - Run drops in reverse insertion order per scope (LIFO), mirroring typical RAII.

  3. Build mode specifics

  - Track scheduled drops in both interpreter and build modes; drops should execute in build snapshots and be replayed (effects-
    only) in parallel builds.
  - Ensure drops are side-effect ordered with defers; likely run defers after drops or vice versa, but document the order.

  4. Runtime ABI

  - Expose a stable “drop call” ABI for types that hold runtime handles (boxes/slices/maps) to release retained runtime handles
    safely.
  - For pure types, drop bodies stay in compiled code; no runtime shim needed.

  5. Testing

  - Add regression tests for scopes with multiple bindings, control-flow exits (return/break/continue), moves, captured closures,
    and build snapshot replay with drop side effects.
  - Add a test that drop runs exactly once and after the last use.

  What it’s useful for

  - Resource cleanup (files/sockets once you add FFI), releasing runtime handles early, and symmetry with Rust-like RAII so users
    don’t hand-roll defers.
  - Simplifies patterns like “ensure channel close” or “release borrow guard” by putting the logic in drop.
