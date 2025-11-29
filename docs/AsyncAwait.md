# Async/Await Design (v1.0)

This note captures the intended shape of async/await for Prime. It is a checkpoint for implementation and diagnostics work; update it as pieces land.

## Goals
- No function coloring: existing `fn` signatures stay `-> T`; suspension is an internal transform.
- Go-like ergonomics: `spawn` remains for concurrent fire-and-forget; `async {}` creates a first-class task.
- Parity: interpreter, build snapshots, and emitted binaries share semantics; build stays deterministic.
- Ownership safety: borrows across suspension are rejected unless proven local; values crossing `await` must satisfy sendability rules.
- Tooling-ready: parser/formatter/LSP understand the syntax; diagnostics surface precise spans and hints.

## Syntax and Surface Types
- `async { expr }` yields `Task[T]` when the block produces `T`.
- `await expr` suspends the current fiber until `expr` is ready; desugars over `Task` and select-ready primitives (channels/timers/fs).
- `async fn foo(args) -> T { ... }` desugars to `fn foo(args) -> Task[T] { async { ... } }` for authoring convenience; callers are not forced to use it.
- `spawn expr` accepts sync or async bodies; async bodies run on the fiber scheduler, sync bodies stay on OS threads (unchanged).
- `Task[T]` gains `join() -> Result[T, Panic]` and is directly awaitable; `Result` propagation with `?` works inside async the same as sync.

## Typing and Borrowing Rules
- Any `await` introduces a suspension point; the checker ensures no `&mut` (or unique ownership that implies exclusivity) lives across it unless the lifetime is proven disjoint.
- Captured values that cross suspension must be `Send` (and `'static` if moved into detached tasks). Non-sendable values must be scoped before the `await` or cloned.
- Drop/RAII and `defer` run in scope order on task completion or panic; suspension does not trigger drops.
- `select` will later allow `await` arms; same borrow rules apply per arm.

## Runtime Model
- Add a cooperative, stackful fiber scheduler in `runtime`: ready queue + timer wheel + channel wakeups. Fibers yield on `await`.
- Builtins gain non-blocking variants for timers/channels/fs; blocking host ops fall back to a small thread pool to avoid starving the event loop.
- Build mode records wakeups deterministically; emitted binaries map fibers to an event loop plus OS threads for blocking work.
- Panics inside fibers surface through `Task::join()`; unjoined tasks propagate panic to the parent runtime on shutdown.

## Compiler/Interpreter Integration
- Parser/AST: tokens for `async`/`await`; expression node for async block; marker for suspension sites.
- Typechecker: rewrites `fn` bodies containing `await` into state-machine lowering; ensures borrow/send rules above.
- Interpreter: executes fibers cooperatively; shares the same drop/defer ordering as sync execution.
- LLVM lowering: emit stackful coroutines or equivalent state machines; preserve build/run parity.

## Tooling
- Formatter/LSP: recognize `async`/`await`, provide hover/diagnostics for illegal borrows across `await`, and signature help for `Task`.
- Docs/Demos: add `workspace/demos/async_demo` showing `async {}`, `await`, channel/timer integration, and `spawn` interop.

## Open Items
- Decide on `select` syntax (`select { await a => ..., await b => ... }`) and fairness policy.
- Finalize panic contract: abort vs unwind inside fibers; current leaning is panic-as-error surfaced via `Task::join()`.
- ABI hooks for embedded: define the minimal async surface (timers/channels) for Xtensa.

## Current Status (2026-04 prototype)
- Syntax + formatter/LSP/typechecker paths are in place; `async {}` returns `Task[T]`, `await` unwraps.
- Runtime uses `AsyncRuntime` with a task state + condvar handle; the cooperative scheduler now polls timers and channel inboxes instead of relying on blocking helper threads.
- Build mode lowers `async`/`await` into the same runtime calls; `sleep_task`/`recv_task` now compile for LLVM output while keeping deterministic snapshots.
- Borrow/send checks across suspension points are not enforced yet; add these once the scheduler is wired.
