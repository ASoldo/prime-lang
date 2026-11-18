# Prime language outline

This document describes the current implementation. Use `prime-lang check TARGET` to check an example and its imports. Use `prime-lang run TARGET` to interpret an entry module, or `prime-lang build TARGET --name NAME` to emit a native executable.

## Source and modules

A file can declare a `module`, `library` or `test` header. Package manifests (`prime.toml`) provide names, paths and visibility. A module can supply `fn main()`. Libraries expose items to importing modules. Test files contain functions that the test runner discovers. Namespace paths support `::` and `/`. For example, `import core::types::prelude::{*};` imports the exported prelude. Items support private, `pub(package)` and `pub` visibility. The Rust parser defines the accepted syntax. The Tree-sitter grammar supplies an independent editor parser.

## Expressions, types and control flow

Blocks return their final expression. `if` and `match` can produce values. A match tries its arms in source order. After a pattern matches, its bindings are in scope for the boolean guard and the arm body. The guard runs once. A false result continues with the next arm, which may match the same variant. Missing enum variants are reported in declaration order. Prime includes tuples, arrays, slices, maps, structs, enums, interfaces and generic functions. `Option[T]` and `Result[T, E]` are defined in the workspace core library. `?` propagates a failure and `try { ... }` establishes a local propagation boundary.

An integer literal must fit its inferred or declared integer type. For example, a variant field declared as `uint8` accepts `255` and rejects `256`. An `int8` field accepts `-128`. The width of `isize` and `usize` follows the selected target. These checks cover literals. The current interpreter can still carry an out-of-range value produced by arithmetic or an explicit cast in a narrow field.

An exhaustive enum match covers every variant and its possible payloads. A guard may fail, so a guarded arm cannot establish coverage by itself. For example, this function covers both states of a packet.

```prime
enum Packet { Data(int32), Empty }

fn value(packet: Packet) -> int32 {
    match packet {
        Packet::Data(n) => n,
        Packet::Empty => 0,
    }
}
```

Replacing `Data(n) => n` with `Data(1) => 1` leaves other integers uncovered. For nested patterns, the checker considers combinations of fields together. A qualified pattern must belong to the enum being matched.

The checker tracks moves and mutable borrows. Direct `await` rejects an active mutable borrow and a task with a reference or pointer as its direct result type. Raw pointers have separate rules. The current checker does not prove arbitrary nested values safe to transfer between threads.

## Host filesystem

`fs_exists(path)` tests whether a path exists. `fs_read(path)` returns UTF-8 text in `Result[string, string]`. `fs_write(path, contents)` writes UTF-8 text and `fs_write_bytes(path, bytes)` writes a `[]uint8`. Both return `Result[(), string]`. Failures carry the error message. The interpreter and native executable resolve relative paths in their process working directory at execution time. Building an application emits these calls. Filesystem calls require a host target with std support and are rejected in `no_std` modules.

The installed CLI includes the runtime sources and builds its matching archive under `.build.prime/runtime` or `$CARGO_TARGET_DIR/prime-runtime`.

Use `match` or `if let` to handle the native result. Native code generation currently rejects `?` on a runtime-selected result and early returns inside runtime match arms. When compiling a spawned filesystem call, leave the experimental `PRIME_BUILD_PARALLEL` snapshot evaluator disabled.

## Scope exit

Each scope registers cleanups as bindings and `defer` statements are evaluated. On normal scope exit, including early return, it runs the most recently registered cleanup first. A deferred action registered between two values with `Drop` runs after the second value drops and before the first. The executable example is in [the drop suite](../workspace/tests/drop).

## Tasks and target profiles

`async { expr }` creates a `Task[T]`. `await task` consumes its result. `sleep_task` and `recv_task` return tasks. Channels preserve FIFO order for queued messages. Guarded waits can observe a timeout or explicit cancellation. The [async specification](AsyncAwait.md) gives the signatures, precedence and restrictions.

The host supports operating-system threads, files, graphics, audio and bounded loopback TCP. The [TCP built-ins and HTTP example](LoopbackWebServer.md) describe the live interpreter and native calls. ESP32 uses fixed-capacity runtime storage and architecture-specific timing. Host conformance results and an ESP32 firmware build establish different properties. The [embedded timer tests](EmbeddedTimers.md) exercise the RV32IMC ABI against a controlled clock. Token reclamation and timing on physical boards remain open.

## Macros, source positions and tools

Pattern macros expand before type checking. `//` introduces a comment, `///` documents an item, and `//!` documents its module. The formatter and language server use the Rust parser. LSP positions count UTF-16 code units. The [check JSON format](Checking.md) uses half-open UTF-8 byte spans suitable for slicing the original source. For executable examples see [workspace tests](../workspace/tests), [Tetris](../workspace/demos/tetris) and the [host checks](../scripts/ci.sh).
