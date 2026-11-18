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

The `core` prelude exports four declarations. `Option[T]` provides `Some(T)` and `None`. `Result[T, E]` provides `Ok(T)` and `Err(E)`. The `Iterator[T]` interface declares `next(self: Self) -> Option[T]`. The `Iterable[T]` interface declares `iter(self: Self) -> []T`. For explicit selections use, for example, `import core::prelude::{Option, Result};`.

The gameplay app owns `Vec2`, `Transform`, `Stats`, `Damage`, `MissionEvent`, `Player`, `Enemy` and `Quest` in `app::model`. The historical `core::types` and `core::types::prelude::{*}` imports expose both sets of declarations. These legacy paths require the workspace core package or equivalent project-provided libraries. The installed fallback supplies only `core` and its prelude. Older projects whose `core::types` defines its own `Option` and `Result` can keep that import alone. Importing the bundled prelude alongside those independent definitions causes name conflicts.

A core library selected by the project manifest, its dependencies, or a relative source file takes precedence. To use the workspace version explicitly, declare a path dependency on `workspace/core`. When the bundled source is selected, `prime-lang check --format=json` lists `<bundled>/core.prime` as its source identity. Imports require no downloads or generated library files.

## Reading binary records

Select the byte functions directly from `core`. The installed CLI includes their Prime source. This example can be saved as `main.prime` anywhere and run with `prime-lang run main.prime`. To run the host binary, use `prime-lang build main.prime --name record` followed by `./.build.prime/record/record`.

```prime
module reader;
import core::{Option, read_u16_le, read_u32_le, crc16_ccitt_false};

fn main() {
  let []uint8 data = [52, 18, 0, 255];
  match read_u16_le(&data, 0) { Some(n) => out(n), None => out("short"), }
  match read_u32_le(&data, 0) { Some(n) => out(n), None => out("short"), }
  out(crc16_ccitt_false(&data));
}
```

| Function | Result |
| --- | --- |
| `read_u16_le(data: &[]uint8, offset: int32) -> Option[uint16]` | Returns the decoded two-byte field |
| `read_u32_le(data: &[]uint8, offset: int32) -> Option[uint32]` | Returns the decoded four-byte field |
| `read_u16_le_into(data: &[]uint8, offset: int32, field: &mut uint16) -> bool` | Writes an unsigned two-byte field and returns `true` on success |
| `read_u32_le_into(data: &[]uint8, offset: int32, field: &mut uint32) -> bool` | Writes an unsigned four-byte field and returns `true` on success |
| `crc16_ccitt_false(data: &[]uint8) -> uint16` | Computes the checksum over the entire slice in byte order |

Offsets count bytes from zero and may be unaligned. The readers return `None` for negative offsets or whenever the complete field falls outside the slice. The `_into` forms return `false` on failure and the destination keeps its previous value. For a record with a stored checksum, compare it with the CRC of the bytes covered by the record format. A checksum mismatch indicates corruption. CRC-16/CCITT-FALSE uses polynomial `0x1021`, initial value `0xFFFF`, no input or output reflection, and final XOR `0x0000`. The ASCII bytes of `123456789` give `0x29B1`. Empty input gives `0xFFFF`.

The host supports runtime-sized byte slices. On the embedded targets, use a slice whose element count is known when compiling. Its byte values and read offsets can be supplied at runtime. The diagnostic example uses the classic ESP32 manifest and prints the following application lines on host or UART. Build the firmware with `prime-lang build workspace/demos/no_std_parity/record_bytes.prime --name record_bytes --no-flash`. [Diagnostic source](../workspace/demos/no_std_parity/record_bytes.prime) · [ESP32 toolchain setup](EmbeddedPipeline.md)

```text
record u16 4660
record u32 4278194740
record short rejected
record crc 10673
```

## Host services and embedded limits

The host runtime provides the filesystem built-ins `fs_exists(path)`, `fs_read(path)`, `fs_write(path, contents)` and `fs_write_bytes(path, bytes)` with the default `std-builtins` feature. Reads return `Result[string, string]` and writes return `Result[(), string]`. The host provides explicit TCP and UDP socket handles with numeric IPv4 and IPv6 endpoints, byte slices, and per-call timeouts. [Host socket API](HostSockets.md)

The host also provides bounded IPv4 loopback TCP built-ins for sequential connections and concurrent reads of delimiter-terminated frames on one listener. [Loopback server limits](LoopbackWebServer.md)

A `no_std` module can import libraries marked `no_std`. The checker rejects host filesystem calls there. Embedded channels, tasks and timers use target-specific runtime support and fixed-capacity storage. Checking or interpreting a module on the host exercises a different execution path from building firmware or running it on a board. [Embedded pipeline](EmbeddedPipeline.md)
