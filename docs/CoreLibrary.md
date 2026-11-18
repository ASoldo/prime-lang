# Prime core library

The installed `prime-lang` executable includes the portable core. A standalone file, piped source, or package (including a `no_std = true` module) can import it directly.

```prime
module example;
import core;

fn first(values: []int32) -> Option[int32] {
  let Iterator[int32] it = values.iter();
  it.next()
}

fn main() {
  let []int32 values = [42];
  match first(values) {
    Some(value) => out(value),
    None => out("empty"),
  }
}
```

Save the example as `main.prime` and run `prime-lang run main.prime`. It prints `42`.

The `core` library exports four declarations. `Option[T]` provides `Some(T)` and `None`. `Result[T, E]` provides `Ok(T)` and `Err(E)`. The `Iterator[T]` interface declares `next(self: Self) -> Option[T]`. The `Iterable[T]` interface declares `iter(self: Self) -> []T`. For explicit selections use, for example, `import core::prelude::{Option, Result};`.

The gameplay app owns `Vec2`, `Transform`, `Stats`, `Damage`, `MissionEvent`, `Player`, `Enemy` and `Quest` in `app::model`. The historical `core::types` and `core::types::prelude::{*}` imports expose both sets of declarations. These legacy paths require the workspace core package or equivalent project-provided libraries. The installed fallback supplies only `core` and its prelude. Older projects whose `core::types` defines its own `Option` and `Result` can keep that import alone. Importing the bundled prelude alongside those independent definitions causes name conflicts.

A core library selected by the project manifest, its dependencies, or a relative source file takes precedence. To use the workspace version explicitly, declare a path dependency on `workspace/core`. When the bundled source is selected, `prime-lang check --format=json` lists `<bundled>/core.prime` as its source identity. Imports require no downloads or generated library files.

## Host services and embedded limits

The host runtime provides the filesystem built-ins `fs_exists(path)`, `fs_read(path)`, `fs_write(path, contents)` and `fs_write_bytes(path, bytes)` with the default `std-builtins` feature. Reads return `Result[string, string]` and writes return `Result[(), string]`. The host also provides bounded IPv4 loopback TCP built-ins for sequential connections and concurrent reads of delimiter-terminated frames on one listener. [Loopback server limits](LoopbackWebServer.md)

A `no_std` module can import libraries marked `no_std`. The checker rejects host filesystem calls there. Embedded channels, tasks and timers use target-specific runtime support and fixed-capacity storage. Checking or interpreting a module on the host exercises a different execution path from building firmware or running it on a board. [Embedded pipeline](EmbeddedPipeline.md)
