# Prime core library

A Prime package can use the same portable types on the host and in a module marked `no_std = true`. Add a path dependency on the core package. Adjust the path relative to the consumer's `prime.toml`.

```toml
[dependencies]
core = { path = "../prime-lang/workspace/core" }
```

```prime
module example;
import core;

fn first(values: []int32) -> Option[int32] {
  let Iterator[int32] it = values.iter();
  it.next()
}
```

The `core` library exports four declarations. `Option[T]` provides `Some(T)` and `None`. `Result[T, E]` provides `Ok(T)` and `Err(E)`. The `Iterator[T]` interface declares `next(self: Self) -> Option[T]`. The `Iterable[T]` interface declares `iter(self: Self) -> []T`. For explicit selections use, for example, `import core::prelude::{Option, Result};`.

The gameplay app owns `Vec2`, `Transform`, `Stats`, `Damage`, `MissionEvent`, `Player`, `Enemy` and `Quest` in `app::model`. The historical `core::types` and `core::types::prelude::{*}` imports expose both sets of declarations. Its manifest maps both source libraries, and the compatibility module selects public types directly from each declaration site. A consumer importing only `core` loads only the portable declarations.

## Host services and embedded limits

The host runtime provides the filesystem built-ins `fs_exists(path)`, `fs_read(path)`, `fs_write(path, contents)` and `fs_write_bytes(path, bytes)` with the default `std-builtins` feature. Reads return `Result[string, string]` and writes return `Result[(), string]`. The host also provides bounded TCP operations on IPv4 loopback. The web server accepts requests with these built-ins in both interpreted and native execution. [Loopback server and TCP calls](LoopbackWebServer.md)

A `no_std` module can import libraries marked `no_std`. The checker rejects host filesystem calls there. Embedded channels, tasks and timers use target-specific runtime support and fixed-capacity storage. Checking or interpreting a module on the host exercises a different execution path from building firmware or running it on a board. [Embedded pipeline](EmbeddedPipeline.md)
