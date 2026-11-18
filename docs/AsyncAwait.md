# Async tasks, cancellation and timeouts

## Creating and awaiting tasks

`async { ... }` creates a `Task[T]`. The block's tail expression supplies `T`. A regular function can return a task, and a caller can use `await task` to take its value. `sleep_task(millis)` produces a `Task[()]` and `recv_task(rx)` produces a task containing the next channel item as an `Option`.

```prime
fn delayed_value() -> Task[int32] {
  async {
    await sleep_task(150);
    7
  }
}
```

`spawn expr` produces a `JoinHandle`. If `expr` produces a task, `join(handle)` returns that task, and `await` takes its result. The [async demo](../workspace/demos/async_demo/async_demo.prime) shows timer and channel tasks together.

## Guarded waits on the host

`cancel_token()` creates a shared `CancelToken` whose flag is initially clear. `cancel(token)` sets the flag, and `is_cancelled(token)` reads it. Code observing any copy of the token sees the same flag.

| Helper | Result |
| --- | --- |
| `await_timeout(task, millis)` | `Result[T, string]` |
| `await_cancel(task, token)` | `Result[T, string]` |
| `await_cancel_timeout(task, token, millis)` | `Result[T, string]` |

These helpers accept a `Task[T]`. The timeout argument has type `int64` and measures milliseconds from the start of the wait. Both the interpreter and native host binaries check the following conditions in order.

1. If the task has finished, return its result. Success yields `Ok(value)`. A task failure yields `Err(message)`. A task whose **value** is `Err(e)` therefore yields `Ok(Err(e))`.
2. If the task is pending and its token is cancelled, mark the task cancelled and return `Err("cancelled")`.
3. If the task is pending and the deadline has expired, mark the task cancelled and return `Err("timeout")`.

A zero or negative duration requests an immediate check. With `await_cancel_timeout`, an already-complete task wins even when the token is set and the deadline is zero. Cancellation wins a tie with the deadline for a pending task.

```prime
let Result[int32, string] result = await_timeout(delayed_value(), 2);
match result {
  Ok(value) => out(value),
  Err(message) => out(message),
}
```

This prints `timeout` while the 150 ms timer is pending. With enough time, the task returns `7`. In native output, use a `match` for a guarded result whose variant is determined at runtime. The compiler diagnoses runtime-selected `?` propagation until it can emit both paths. A task supplies one consumable result. A second guarded wait after a successful join returns `Err("task missing result")`.

## Execution and ownership

The host interpreter starts async blocks on worker threads. The native host compiler emits an entry function for each block and transfers captured values to the runtime at task creation. A waiter sleeps on a condition variable and checks an optional token periodically. Timer and receive tasks observe cancellation. Prime code already executing on a worker can still produce side effects after its caller stops waiting. Cancellation does not unwind the worker's stack.

At an `await` expression, the type checker reports an active mutable borrow (`E0ASYNCBORROW`) or a task whose immediate result type is a reference or pointer (`E0ASYNCSEND`). Those checks apply to the await expression. Native host code transfers immutable bindings and preserves the identity of runtime-backed tokens and channel handles. It diagnoses mutable captures because it cannot yet share their binding cells with the worker. Transitive sendability of captures and values inside aggregates remains open.

Embedded LLVM targets use a fixed pool of tasks, channel waiters and a target-specific timer and polling ABI. Embedded async blocks still use deferred evaluation. Device timing depends on the board runtime. The [no_std parity probe](../workspace/demos/no_std_parity/main.prime) covers a subset of these APIs under the host interpreter.

## Proposed extensions

`async fn` syntax, `select` arms, a `Task::join()` method, transitive `Send` checks and a cooperative fiber scheduler are design proposals. The current host runtime uses OS threads and explicit task results.
