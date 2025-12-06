# Prime Language Outline (v1.0)

- Expressions: expression-oriented (`if`/`match` yield values), blocks return last expression, arrays/maps/slices/struct literals follow formatter style. `async {}` produces `Task[T]`; `await` unwraps.
- Ownership: bindings are move-by-default; `move`/`copy` explicit when transferring. `&T`/`&mut T` references are non-null; borrows cannot cross `await` until send/borrow checks land. `defer` runs on scope exit in declaration order.
- Concurrency: `channel[T]()` yields `(Sender[T], Receiver[T])`; `recv_timeout(rx, ms)` returns `Option[T]`; `recv_task(rx)`/`sleep_task(ms)` are awaitable. Channel ordering is FIFO; embedded waits poll at 1–2ms by default.
- Macros: hygienic, pattern-based with named or `repeat+` params. Macros expand before typecheck; hygiene preserves bindings while allowing deliberate escapes via `@` where needed.
- Comments & docs: `//` is a line comment; `///` attaches documentation to the following item; `//!` is file/module-level documentation. Doc comments flow into hover, outline, and the HTML docs generator.
- Tooling: formatter and LSP share the parser; formatter regression cases live in `workspace/tests/golden`. Runtime handles must be available for async/await on non-host targets.
