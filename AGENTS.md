1. Minimal stdlib expansion via built-ins (fs/time/iter):

  - File I/O built-ins: fs_read(path) -> Result[string, string], fs_write(path, string) -> Result[(), string], fs_exists(path)
    -> bool.
  - Time built-ins: now_ms() -> int64, sleep_ms(int64) -> () (rename/alias from sleep).
  - Iter helpers as built-ins: slice/map .iter() returning a lightweight iterator with next() -> Option[T]; map_keys/map_values
    for Map. Keep pure and build-mode friendly.
  - Docs/samples: README/docs topic plus demos (fs_demo, time_demo, iter_demo) showing usage.

  2. Borrow/move diagnostics improvements:

  - Borrow errors: include binding origin, active borrower, and hints to clone/reorder; show spans for borrower and move site.
  - Moved-value errors: cite the last binding and move origin (move, capture, box_take); suggest references when suitable.
  - Alignment: identical wording/spans in run vs build.
  - Tests: regression cases for borrow conflicts, moves during borrow, and drop-after-move with clear messages.

  3. Scope/compatibility:

  - No language surface changes; only built-ins and diagnostics.
  - Build-mode determinism: fs/time built-ins record effects; snapshots replay deterministically.

  4. Deliverables/checklist:

  - Built-in fs/time/iter support.
  - README + prime-lang docs updates with code snippets.
  - New demos under workspace/demos for fs/time/iter.
  - Tests under workspace/tests covering fs/time and iterator helpers.
  - Improved borrow/move diagnostics with unit tests for run/build parity.

  5. No-Std friendly
  Want to be no-std friendly later, design now so that:

  1. Built-ins are routed through a thin platform trait layer that can be stubbed or HAL-backed.
  2. There’s a feature flag to disable std-only built-ins (fs/time/spawn/channel) and fail fast at compile time in no-std.
  3. Allocator use is explicit, with a way to swap in a custom allocator or a “no heap” profile.
  4. Diagnostics can degrade gracefully (no color, minimal formatting) without std.
