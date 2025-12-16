<img width="1924" height="1053" alt="image" src="https://github.com/user-attachments/assets/80256d3b-018a-46f4-95a9-6134645e3817" />

# Prime Language

Prime is a learning-oriented language and toolchain for experimenting with
parsing, interpretation, LLVM code generation, and Neovim integrations. This
repository contains the compiler/interpreter (`prime-lang`), the LSP server, and
the CLI utilities you use while editing `.prime` files.

### Project Layout

```
src/
 ├─ language/   # parser + AST + compiler lowering
 ├─ runtime/    # interpreter + value/runtime support
 ├─ project/    # manifest + package loader shared by CLI/LSP
 ├─ lsp/        # standalone LSP crate (backend, completion, hover, diagnostics)
 └─ tools/      # CLI helpers: formatter, lint, diagnostics bridge
```

The `tools` module now owns the previously huge `formatter.rs`, `lint.rs`,
`diagnostics.rs`, and `lsp.rs` files, which keeps the main binary lean and makes
each tool easier to navigate.

---

## Quick Start

```bash
git clone https://github.com/asoldo/prime-lang.git
cd prime-lang
cargo install --path .
prime-lang run workspace/app/main.prime   # interpret the packaged app::main module
prime-lang build workspace/app/main.prime # emit LLVM, link to ./.build.prime/output
./.build.prime/output/output              # run the native binary
# Try another module: prime-lang run workspace/demos/error_handling/error_handling_demo.prime
```

The repository ships with `main.prime` so you can confirm the full toolchain
works end‑to‑end immediately after building. As of the April 2026 compiler
updates the emitted native binary now exits with status code `0`, so scripting
and CI integrations no longer see spurious failures after a successful build.
Build-mode now mirrors the interpreter for control flow (`return`/`break`/`continue`)
and for the new `try {}` / `?` sugar, so you can rely on identical semantics in both
`prime run` and `prime build`.
Build-mode also executes concurrency the same way: `spawn`/`join` plus channels work
while compiling (deterministically unless you opt into `PRIME_BUILD_PARALLEL=1`), and
the emitted binary uses real OS threads. Async/await + channels automatically attach
runtime handles when async appears, so `recv_task`/`sleep_task` block correctly in
build mode, the host binary, and the ESP32 runtime (no stubs or hangs).

### Execution Parity (run vs build vs embedded no_std)

- **Core semantics (run/build)**: control flow (`if`/`match`/loops), `return`/`break`/`continue`, pattern matching across tuples/maps/structs/slices, generics/interfaces, macros, Option/Result with `try {}`/`?`, Drop + `defer`, mutable scalars (stack slots kept for ints/bools so loop mutations print correctly), channels + spawn/join, async/await (`sleep_task`/`recv_task`), and SSA-backed booleans/ints all mirror between interpreter and host builds.
- **Embedded/no_std (ESP32 Xtensa)**: `out` (strings + format strings + ints/bools), channels, async (`sleep_task`/`recv_task`), `spawn`/`join` (task runtime, not OS threads), GPIO (`pin_mode`/`digital_write`), `delay_ms`/`now_ms`, mutable scalars (stack slots), Option/Result + `?`, pattern matching, and macro-expanded code work the same as host. Panic unwinds are host-only; embedded panics abort/loop. The ESP32 demo also exercises channel/task pool recycling and async Result propagation so build/run/embedded stay aligned.
- **Host-only**: filesystem built-ins (`fs_exists`/`fs_read`/`fs_write`) and direct OS threads; embedded `spawn`/`join` run on the task runtime instead of OS threads, and other std-dependent hooks stay disabled (you’ll get clear errors when they’re unavailable).
- **Manifests**: set `no_std = true` per module in `prime.toml` for embedded targets. `workspace/demos/esp32_blink/prime.toml` and `workspace/demos/bare_metal_embedded/prime.toml` show the expected flags and toolchain entries; CLI also auto-detects esp-clang/Xtensa under `~/.espressif` when not provided.

### Example Programs

- `workspace/app/main.prime` – Feature tour: modules, structs/enums, interfaces, ownership, and UI-ish printing.
- `pattern_demo.prime` – Match/if/while/for destructuring across tuples, maps, structs, slices.
- `error_handling_demo.prime` – `Result`, `try {}`, and `?` propagation.
- `lab_demo.prime` – A syntax-rich lab scenario (range loops, map destructuring, mutable refs, generic interface) used as an internal example; not exposed via `prime-lang docs`.
- `pointer_demo.prime` – Raw pointers derived from references plus first-class ranges.
- Library modules: `types.prime` and `pkg_lib.prime` are libraries only (no `main`). Running them directly reports `Unknown symbol main`; import them from other modules instead.

Run any of them directly:

```bash
prime-lang run workspace/demos/lab_demo/lab_demo.prime                  # module demos::lab_demo
prime-lang run workspace/demos/patterns/pattern_demo.prime              # module demos::patterns
prime-lang run workspace/demos/error_handling/error_handling_demo.prime # module demos::error_handling
```

Validated outputs (fresh run, current syntax; files live under `workspace/demos/...` unless noted):

- `borrow_demo.prime` — aliasing/borrowing demos; prints loop merges and HP/MP stats.
- `error_handling_demo.prime` — shows Ok/Err propagation, span calculations.
- `generic_demo.prime` — identity/promote helpers with typed outputs.
- `heap_demo.prime` / `heap_features.prime` — box counters, roster prints, redeploy outputs.
- `interface_demo.prime` / `interface_generics_demo.prime` — method calls and announcements.
- `lab_demo.prime` – three plan runs with totals/synergy summaries and pairing.
- `main.prime` — full gameplay log (players, enemies, quests, patrols, defers).
- `ns_demo.prime` — namespace overloading demo with foo/bar labels.
- `pattern_demo.prime` – pattern matches over tuples/maps/slices plus mutation demos.
- `drop_demo.prime` — Drop/RAII cleanup with scoped trackers and defer ordering.
- `pkg_app.prime` — banner/promotion output.
- `pointer_demo.prime` — pointer-based HP tweaks and stored ranges.
- `parallel_demo.prime` — channels + spawn/join demo showing build/run parity for concurrency.
- `fs_demo.prime` / `time_demo.prime` / `iter_demo.prime` — minimal built-in samples for file I/O, timers, and iterator helpers.
- `workspace/demos/no_std_parity/main.prime` — host-run parity fixture for embedded async/Result/timeouts (channels, `recv_task`/`recv_timeout`, defers) so `no_std` behavior stays locked while developing on a desktop.
- `workspace/demos/bare_metal_embedded` — smallest `no_std` sample (sums/filters on-device) showing manifest flags plus channels + async `recv_task` + `Result` parity without GPIO.
- `workspace/demos/esp32_blink` — ESP32 Xtensa no_std blink demo with async/await, spawn/join, channel reuse probe, and calibrated delay; manifest includes toolchains and flash settings. Build uses
  `llc -relocation-model=static -mtriple=xtensa-esp32-elf -mcpu=esp32 -mattr=+windowed`
  and links `libruntime_abi.a` (Xtensa) plus libc/libgcc before flashing via
  `esptool elf2image` (falls back to objcopy when elf2image is unavailable).

### Macros & Traceability

- Call macros with `~name(args)`; add `;` to use them as statements alongside regular expression use.
- Expansion traces are carried into type errors, so diagnostics from generated code include the macro call stack (e.g., `in expansion of \`foo\` at …`).
- Inspect expansions from the CLI: `prime-lang expand file.prime --line 10 --column 5` prints the macro trace and only the expanded items that originate from that call. Add `--print-expanded` or omit the position flags to dump the whole expanded module after expansion.
- Read input safely with `in[T](prompt, ...) -> Result[T, string]`. Prompts can be plain or format strings; parsing failures return `Err(message)` so callers can `match` or use `?`. Works in both the interpreter and compiled binaries.
- Use `@ident` inside a macro body to intentionally capture an outer binding without hygiene renaming.
- Item macros are allowed: write a macro body as a block of items and invoke it at module scope with `~macro_name(...);` to splice structs/functions/consts into the module.
- Macro parameters can opt into richer shapes with `: block`, `: pattern`, or `: tokens` annotations (defaults to expression); block/tokens arguments are inlined without extra bindings, and pattern args can be dropped directly into match/let patterns.
 - `: tokens` args preserve the original token stream for re-parsing or re-emitting; this underpins repetition/fragment matching.
- `: repeat` args now split on the first top-level separator (comma or semicolon by default); comma-joined fragments become a tuple, and semicolon-joined fragments become a block with sequential expressions. You can use a custom single-token separator via `@sep = <token>` (e.g. `@sep = |`); keep it immediately after `(`, at most once per macro call (an optional comma after the prefix is accepted).
- Repeat params support quantifiers: `repeat+` (default, one-or-more) and `repeat*` (zero-or-more) for macro parameters.
- Macro visibility: `pub` exports across packages, `pub(package)` shares within the package, and private macros stay module-only.
 - LSP hover shows macro declarations (including bodies) with parameter kinds; macros participate in navigation/rename just like functions. Primitive types and keywords use markdown tooltips for quick reminders.

### Collections and Indexing

- Slices: `let []int32 values = [1, 2, 3];` and `values[1]` yields `Option[int32]`. Methods: `.len()`, `.get(index)`, `.push(value)`.
- Maps: `let Map[string, int32] scores = #{ "alpha": 10 };` and `scores["alpha"]` yields `Option[int32]`. Methods: `.len()`, `.get(key)`, `.insert(key, value)`.
- Arrays: fixed-size `[T;N]` types index to `Option[T]` too.
- Indexing works in both run/build modes; assignment through indexes is supported for slices and maps.

### File I/O, Time, and Iterator Helpers

- File built-ins: `fs_exists(path) -> bool`, `fs_read(path) -> Result[string, string]`, `fs_write(path, contents) -> Result[(), string]`.
- Time built-ins: `now_ms() -> int64` and `sleep_ms(int64)` (alias for `sleep`); build mode records clock effects for deterministic snapshots.
- Iterator helpers: `slice.iter()` / `map.iter()` return lightweight `Iterator[T]` with `next() -> Option[T]`; `map_keys` / `map_values` return slices. Works in run/build and in the emitted binary.
- See `workspace/demos/fs_demo`, `time_demo`, and `iter_demo` for runnable examples.

### Drop (RAII-style cleanup)

- Implement `Drop` on a struct/enum to run `fn drop(self: &mut Self)` at scope exit. No returns, panics become runtime errors.
- Drops are scheduled when a binding is created, run once, and honor moves (moved-out bindings stop scheduling drops).
- Cleanup order is LIFO per scope alongside `defer` (later bindings drop first; defers interleave in insertion order). Build mode replays the same drop effects in snapshots.
- Example (`workspace/demos/drop_demo/drop_demo.prime`):

```prime
struct Tracker { label: string; log: &mut []string; }

impl Drop for Tracker {
  fn drop(self: &mut Tracker) { self.log.push(self.label); }
}

fn demo(log: &mut []string) {
  let Tracker first = Tracker { label: "first", log };
  defer log.push("defer");
  let Tracker second = Tracker { label: "second", log };
}
# log becomes ["second", "defer", "first"]
```

## CLI Overview & Built-in Docs

Every command is defined in `src/main.rs` with clap, so `prime-lang --help` stays
fresh. Highlights:

| Command | Purpose |
| --- | --- |
| `prime-lang run <target> [--project pkg]` | Interpret a module by manifest name or path after loading workspace dependencies (modules only; libraries are rejected) |
| `prime-lang build <target> [--project pkg] --name demo` | Compile a module (by name or path) to LLVM IR/object code and write a runnable binary below `./.build.prime/demo` (modules only) |
| `prime-lang lint <file> [--watch]` | Single-shot or watch-mode linting with the same parser used by the LSP |
| `prime-lang fmt <file> [--write]` | Preview or apply the formatter |
| `prime-lang lsp` | Start the language server over stdio (Neovim/VS Code use this entry point) |
| `prime-lang new [path] [--bin|--lib|--wrk]` | Scaffold a package or a workspace (with a single member) using `manifest_version = "3"` |
| `prime-lang add [-p pkg] <module> [--path file.prime] [--test|--library] [--git URL|--dep-path DIR] [--features a,b]` | Append a module/test/library or register a dependency (git/path) in a package manifest (workspace-safe with `-p`) |
| `prime-lang test [target,...]` | Run test modules (header `test ...;`) by name or file; discovers tests when no target is provided |
| `prime-lang docs [--list|--query ...|--generate [--out file]|--serve [--port 7878]]` | Print curated topics, emit workspace docs as HTML, or serve them locally |
| `prime-lang install --git URL [--name tool]` | Clone a tool into `.prime/tools` and record it in the registry |
| `prime-lang update [--name tool]` | Update installed tools by refetching their git sources |
| `prime-lang uninstall <tool>` | Remove a tool checkout and prune it from the registry |
| `prime-lang sync [path]` | Resolve dependencies and write `prime.lock`; pair with `--frozen` on run/build to require the lock |
| `prime-lang expand <file> [--offset N | --line L --column C] [--print-expanded]` | Show macro expansion trace at a cursor (line/column or byte offset). With a position, prints only the expanded items from that macro; without a position or with `--print-expanded`, prints the fully expanded module. |
| `in[T](prompt, ...) -> Result[T, string]` | Built-in input helper. Reads a line from stdin, parses to `T`, and returns `Ok(value)`/`Err(message)`. Formatting rules mirror `out`; type arguments are required. |
| Concurrency | `spawn expr` → `JoinHandle[T]`; `channel[T]()` → `(Sender[T], Receiver[T])`; `send` returns `Result[(), string]`; `recv` yields `Option[T]` (run uses real threads; build evaluates deterministically with the same blocking semantics; embedded maps spawn/join to the task runtime for parity) |

Dev workflow (build/test/lint + docs server): `docs/Development.md`.

Input prompts accept format strings, so you can interpolate context into the prompt itself:

```prime
match in[int32](`Temperature for { "Soldo" }: `) {
  Result::Ok(temp) => out(`temp recorded: {temp}`),
  Result::Err(msg) => out(`temp error: {msg}`),
}

// Async + channel parity (build/run/embedded):
fn main() {
  let (tx, rx) = channel[int32]();
  let task = async {
    let Option[int32] received = await recv_task(rx);
    match received {
      Some(v) => out(`got {v}`),
      None => out("closed"),
    }
  };
  let _ = send(tx, 7);
  close(tx);
  await task;
}
```

For a compact reference on expressions, ownership, and macro hygiene, see `docs/LanguageOutline.md`.

Input supports all primitives (`string`, `bool`, signed/unsigned ints, floats, `rune`).
For non-interactive test runs, set `PRIME_TEST_INPUTS` to a delimited list of lines
(supports `|`, `,`, or `;` as separators) so `in[...]` consumes scripted input instead
of blocking on stdin. Example: `PRIME_TEST_INPUTS="21,abc,true,maybe,98.6,nope,Prime,Y,200,42,-1,500,70000,1000000,128,3.14,badf" prime-lang test`
feeds every prompt in the bundled input tests; quote the value so shells don’t split on `|`.
Alternatively point `PRIME_TEST_INPUTS_FILE` at a file containing one input per line.
When no input env is provided, `prime-lang test` auto-seeds the above defaults to avoid blocking.

### Modules, imports, and preludes

- Modules are declared with `module ...;` (entrypoints) or `library ...;` (import-only). Use `test ...;` for test modules.
- Import modules with selectors: `import core::types::{Result, Option};` or glob: `import core::types::{*};`. Plain `import core::types;` brings the module into scope and automatically applies its prelude export list.
- Libraries can export a prelude block to surface their public API without spelling selectors at every call site:

  ```prime
  library core::types;

  export prelude {
    Option,
    Result,
    Iterable,
  };

  pub enum Option[T] { Some(T), None }
  pub enum Result[T, E] { Ok(T), Err(E) }
  ```

  Consumers can:
  - `import core::types;` (prelude auto-applied),
  - `import core::types::prelude::*;` (explicit),
  - or select symbols directly: `import core::types::{Result};`.
- Qualified imports must point at modules, not symbols (`import pkg::lib::{example};` is correct; `import pkg::lib::example;` is rejected). Formatter keeps selector imports as `module::{...}`.
- Visibility is enforced on imports: non-`pub` items aren’t imported, and LSP completions/hover respect visibility.

### `prime-lang docs`

- `prime-lang docs` with no flags prints every topic in one pass (CLI, manifest,
  syntax, control flow, generics, borrowing, etc.).
- `prime-lang docs --list` shows topic keys plus their aliases so humans or AI
  agents can discover supported queries.
- `prime-lang docs --query for,match` (comma-delimited or repeated flags) filters
  to the topic that owns those aliases—`for`, `if`, and `match` map to the
  `prime-intermediate` section that demos control flow and pattern matching.
All snippets are drawn directly from the compiling demo files (`demos/*.prime`)
or the real workspace manifest, so you always see runnable examples. The CLI
exits with an error code and a hint if a query doesn’t map to any topic.

### Workspace HTML docs (comments → web app)

- Item docs use `///` directly above a declaration; module/file docs use `//!` at
  the top of the file. Regular `//` comments are preserved by the formatter but
  aren’t included in rendered docs.
- `prime-lang docs --generate --out docs.html` renders the workspace described
  by `prime.toml` into a single-page HTML app with search, collapsible
  workspace/outline, clickable import jumps, hoverable graph, and doc blocks
  sourced from your `///`/`//!` comments. Without `--out`, HTML is printed to
  stdout for piping.
- `prime-lang docs --serve --port 7878` serves the same app over HTTP
  (`http://127.0.0.1:7878` by default). Use the browser UI to search, click
  imports to hop across modules, and inspect relationships in the canvas graph.
  When served, Prime watches your workspace `.prime` files + `prime.toml` and
  auto-reloads the page as docs regenerate on save.

### `prime.toml` manifest

`prime-lang` workspaces load `prime.toml` through
`project::manifest::PackageManifest`. The manifest tells the loader which module
names map to which files and what the entry module is:

```toml
manifest_version = "3"

[package]
name = "demo-app"
version = "0.1.0"

[module]
name = "demo_app::main"
path = "main.prime"
visibility = "pub"

[dependencies]
util_logging = { name = "util::logging", git = "https://example.com/logging.git", features = ["fmt"] }

[libraries]
core_types = { name = "core::types", path = "../core/types.prime", visibility = "pub" }
```

Module names use `::` separators (no dots). Tests can be listed under `[tests]`
(inline entries or an `items` array) if you want manifest metadata; otherwise
any file that starts with `test ...;` is discovered by `prime-lang test`.
Section order is normalized by the CLI as `manifest_version`, `package`, `dependencies`, `modules`, `libraries`, `tests`, `workspace`.
`prime-lang add` respects that order and writes entries into the correct table; dependencies never float to the top or bottom.

#### Dependencies

- Local path: `prime-lang add util::logging --dep-path ../logging` writes to `[dependencies]` with `path = "../logging"`.
- Git: `prime-lang add util::logging --git https://example.com/logging.git` writes a git entry, clones into `.prime/deps/…`, and records the current HEAD in `prime.lock`. Path deps are locked too. Use `--features a,b` to record feature flags.
- Workspace-scoped edits: pass `-p pkgname` to edit that package’s `prime.toml` instead of the workspace root.

### Tests

- Write test files with a `test my::module;` header. They can import other modules
  and define any number of functions; no `main` is required.
- `prime-lang test` scans the nearest `prime.toml`; if it is a workspace manifest, it walks every member’s manifest to gather test entries. You can still point it at specific test files or module names.
- `prime-lang test` runs every zero-arg function in the test module (bool return
  is treated as pass/fail). The full return list is passed to the next function
  when arity matches (supports multi-value chaining) or to a single-arg function
  when there is exactly one return; otherwise the function is skipped with a note.
- Targets can be files or module names (`prime-lang test tests::basic,other::test`);
  no target runs all discovered test headers.
- `prime-lang run/build` refuse to execute/compile `test ...;` targets.
- Built-in helpers for tests: `assert(cond: bool)` and `expect(cond: bool, message: string)`.

`prime-lang add demos::patterns --path pattern_demo.prime` will append another
`[modules]` entry and create the stub file automatically. Keeping the manifest
in sync with each file’s `module ...;` declaration lets the CLI, interpreter,
compiler, and LSP share the same package graph.

### Embedded / ESP32 (Xtensa) Quickstart

The workspace ships an ESP32 classic (Xtensa) demo at `workspace/demos/esp32_blink/esp32_blink.prime`. The manifest carries everything needed to build and flash when you have the ESP-IDF toolchains installed under `~/.espressif`:

```toml
[build]
target = "xtensa-esp32-espidf"
platform = "esp32"

[build.toolchain]
cc = "xtensa-esp32-elf-gcc"
ar = "xtensa-esp32-elf-ar"
objcopy = "xtensa-esp32-elf-objcopy"
esptool = "esptool"
ld_script = "/home/rootster/esp/esp-idf/examples/get-started/blink/build/esp-idf/esp_system/ld/sections.ld"
ld_flags = """
  -Wl,--gc-sections
  -T/home/rootster/esp/esp-idf/examples/get-started/blink/build/esp-idf/esp_system/ld/memory.ld
  -T/home/rootster/esp/esp-idf/components/esp_rom/esp32/ld/esp32.rom.ld
  -T/home/rootster/esp/esp-idf/components/esp_rom/esp32/ld/esp32.rom.api.ld
  -T/home/rootster/esp/esp-idf/components/esp_rom/esp32/ld/esp32.rom.libgcc.ld"""

[build.toolchain.env]
RUSTUP_TOOLCHAIN = "esp"
CARGO_TARGET_DIR = "/home/rootster/.cache/prime-xtensa"
LLVM_SYS_201_PREFIX = "/home/rootster/.espressif/tools/esp-clang/esp-clang"
LD_LIBRARY_PATH = "/usr/lib64:/usr/lib:/lib:/home/rootster/.espressif/tools/esp-clang/esp-clang/lib"
PATH = "/home/rootster/.espressif/tools/esp-clang/esp-clang/bin:/home/rootster/.espressif/tools/xtensa-esp-elf/esp-15.2.0_20250929/xtensa-esp-elf/bin:$PATH"
CARGO_TARGET_XTENSA_ESP32_ESPIDF_LINKER = "xtensa-esp32-elf-gcc"

[build.flash]
enabled = true
port = "/dev/ttyUSB0"
baud = 460800
address = "0x10000"
```

What it does:
- Toolchain/ROM scripts point at the IDF-generated `sections.ld`/`memory.ld` plus ROM ld files so Xtensa ROM symbols (e.g., `ets_delay_us`, `ets_printf`) resolve correctly.
- `[build.toolchain.env]` injects the esp-clang/Xtensa toolchains, LLVM prefix, linker choice, and a dedicated cache dir. `$PATH` is expanded in-place so existing PATH is preserved.
- The CLI also autodetects the same defaults under `~/.espressif` if you omit them; keep these entries if your layout differs or you want a self-contained manifest.
 - UART logging: `out(...)` prints over UART via ROM `ets_printf`. Strings, format strings, ints (mutable/SSA-backed, so loop counters stay fresh), and bools are rendered; each `out` appends its own newline so successive calls separate cleanly. The blink demo now prints your format strings (e.g., `hello ... {my_var}`) plus per-loop state booleans—no extra boot banner is injected by the runtime.
  - For the bundled demo, the runtime disables RTC/TIMG watchdogs once at startup so the tight blink loop can run indefinitely without resets. Remove that if you want watchdog coverage in your own projects.

How to build/flash (after installing ESP-IDF tools):
1. Optional but recommended: `source ~/esp/esp-idf/export.sh` to set IDF_PATH and python deps.
2. Run the demo: `prime-lang build workspace/demos/esp32_blink/esp32_blink.prime --name esp32_blink`
   - The runtime prints your `out(...)` lines on UART (115200) with newlines per call (`hello ... {my_var}`, loop state toggles, etc.), and the on-board blue LED (GPIO2, active-low) blinks.
   - External button input: wire GPIO18 -> momentary switch -> GND (active-low) for in-loop logging; BOOT/EN buttons are reserved by auto-boot/reset on many boards.
   - Concurrency parity: the demo runs async `recv_task`, `spawn`/`join`, a Result + `?` async probe, a timeout probe, and a small channel/task pool reuse probe so embedded output matches run/build semantics.
   - The demo disables hardware watchdogs once on boot to keep the loop alive; re-enable or adjust if your project needs watchdog protection.
   - Flashing is on by default for the demo; disable with `--no-flash` or set `[build.flash].enabled = false`.
3. If your board’s user LED sits on another pin (some use GPIO4/5), edit the `led` constant in `esp32_blink.prime`.

Environment handling:
- Manifest-provided `build.toolchain.env` is applied automatically before build; `$PATH` interpolation keeps your existing PATH intact.
- If no env overrides are present, the CLI falls back to sensible defaults under `~/.espressif` (esp-clang + xtensa-esp-elf) and uses `RUSTUP_TOOLCHAIN=esp` plus `CARGO_TARGET_DIR=~/.cache/prime-xtensa`.
- The Xtensa entry point and runtime now include ROM-backed delay/printf stubs and minimal GPIO2 setup, so no extra IDF libs are required beyond the linker scripts and toolchain.

## Language Basics

Prime keeps the syntax close to systems languages you already know. Key features
in `main.prime` and `types.prime` demonstrate the core semantics:

### Modules, Libraries & Imports

```prime
module app::main;
library core::types;

import core::types;            // load structs/enums from types.prime
import math::random;           // relative paths become math/random.prime
```

Headers distinguish entrypoints (`module` with `main`), shared code (`library`
without `main`), and `test` files. Imports use the same symbolic paths, so the
formatter/LSP can offer completions and go-to-def across modules and libraries.
`prime-lang run/build` only accept module headers; libraries that define `main`
produce an error.

#### Concurrency

```prime
fn log_once() -> Option[int32] {
  let (tx, rx) = channel[int32]();
  let worker = spawn send(tx, 42);
  let Option[int32] first = recv(rx);
  join(worker);               // Result[(), string]
  first;
}
```

- `spawn expr` returns `JoinHandle[T]` where `T` is the expression type; `join(handle)` produces the value (or panic) and consumes the handle.
- `channel[T]()` returns `(Sender[T], Receiver[T])`; `send`/`recv`/`close` act on endpoints. `send` returns `Result[(), string]`; `recv` yields `Option[T]` (None if closed).
- Run mode executes spawned work on OS threads and `recv` blocks until a value arrives or the channel closes. Build mode mirrors the same blocking channel semantics while still executing deterministically during compilation.

### Constants & Mutability

```prime
const TRAINING_LIMIT: int32 = 3;

let Player scout = spawn_player("Sparrow", Vec2{ 5.0, 12.5 }, 90);
let mut Player hero = spawn_player("Prime Hero", Vec2{ 10.0, 20.0 }, 120);
hero = refill_hp(hero, 12);
```

- `const NAME: Type = value;` creates an immutable global.
- `let name =` introduces an immutable binding. Add `mut` to opt into mutation.
- Types are explicit; the compiler infers only when the annotation is obvious.

### Structs, Embedding & Enums

```prime
struct Vec2 {
  x: float32;
  y: float32;
}

struct Transform {
  Vec2;                     // embedded; adds position.x/position.y directly
  velocity: Vec2;
}

struct Player {
  Transform;
  Stats;
  name: string;
  level: int32;
}

enum Damage {
  Flat(int32);
  Percent(int32);
}
```

- Struct embedding flattens the fields of the embedded struct (`Transform` and
  `Stats`), so `player.position.x` works without inheritance or vtables.
- Enums are algebraic sum types; each variant carries its payload and is
  exhaustively matched.

### Functions & Multiple Return Values

```prime
fn divmod(a: int32, b: int32) -> (int32, int32) {
  (a / b, a % b)
}

fn move_player(p: Player, delta: Vec2) -> Player {
  Player{
    Transform{ add_vec2(p.position, delta), p.velocity },
    Stats{ p.hp, p.stamina },
    p.name,
    p.level,
  }
}
```

- Functions take explicit parameter lists with optional `mut` on the binding
  itself.
- Return types can be a single type or a tuple. Destructuring happens at call
  sites: `let (quot, rem) = divmod(22, TRAINING_LIMIT);`.

### Pattern Matching & Control Flow

```prime
match damage {
  Flat(amount) => Player{ .. },
  Percent(rate) if rate > 25 => { /* guard branch */ },
  MissionEvent:Waiting => { /* zero-field variant */ },
  Percent(rate) => { /* fallback */ },
};

if let Some(reserve) = stats.get("hp") {
  out(reserve);
}

match stats {
  #{ "hp": hp, "mp": mp } => out(hp + mp),
  _ => out("incomplete"),
};

match reading {
  ("status", code) => out(code),
  _ => out("other"),
};

match player {
  Player{ Stats{ hp, .. }, name, .. } => out(name),
};

match queue {
  [head, ..tail] => out(head),
  [] => out("empty"),
};

if hero.hp < 50 {
  out("status: fragile");
} else if hero.hp > 250 {
  out("status: unstoppable");
} else {
  out("status: ready");
}

for i in 0..steps {
  out(i);
}

while probes < 2 {
  out("probe");
  probes = probes + 1;
}

for explorer in explorers {
  out(explorer);
}

let []string probe_notes = ["ok", "stable"];
let mut int32 idx = 0;
while let Some(note) = probe_notes.get(idx) {
  out(note);
  idx = idx + 1;
}

let Telemetry{ hp: hp_stat, mp: mp_stat, .. } = snapshot;
let #{ "hp": hp_total, .. } = stats;
out(hp_stat + mp_stat + hp_total);
```

- `match` is exhaustive, supports `Enum::Variant` syntax, and each arm can carry
  an `if guard` to refine patterns.
- Loops include `for range`, `while`, `while let`, and `match` drives branching.

### References, Pointers & `defer`

```prime
fn peek_hp(target: &Player) -> int32 {
  let Player snapshot = *target;
  snapshot.hp
}

fn refill_hp(target: &mut Player, amount: int32) {
  let Player snapshot = *target;
  *target = Player{ .. };
}

fn calibrate_station(name: string) {
  out("Calibrating station:");
  out(name);
  defer out("Station teardown complete");
  // ...
}
```

- `&T` is a reference. You must dereference explicitly (`*target`) when copying.
- `&mut T` gates mutation.
- Raw pointers use `ptr(&value)` / `ptr_mut(&mut value)` and dereference with
  `*pointer`; they bypass borrow checks but share the same storage as the
  originating reference.
- `defer` schedules work to run when the current scope exits—ideal for cleanup.

### Ownership & Borrowing Rules

```prime
let []string squad = ["alpha", "bravo"];
let []string redeployed = move squad;    // `squad` is consumed

let mut int32 hp = 10;
let &mut int32 first = &mut hp;
let &mut int32 second = &mut *first;     // error: `hp` already mutably borrowed

fn measure() -> int32 { 4 }
let &int32 bad = &measure();             // error: temporary would dangle
```

- `move` transfers ownership of heap-backed values (Box, slices, maps). Reads or method calls on the moved binding surface a `was moved` diagnostic, even after branching.
- Only one active `&mut` to a binding is allowed at a time; re-borrows through aliases/dereferences are traced and produce actionable errors.
- Borrowing a temporary (call result, inline literal, block expression) is rejected so references do not outlive the value. Bind the value first, then borrow.

### Error Handling (`Result`, `try`, `?`)

```prime
fn apply_boost(value: int32) -> Result[int32, string] {
  let int32 parsed = parse_energy(value)?;
  if parsed % 2 == 0 {
    Ok(parsed * 2)
  } else {
    Err("boost requires even energy")
  }
}

fn calibrate_profile(value: int32) -> Result[int32, string] {
  try {
    let int32 boosted = apply_boost(value)?;
    boosted + 5
  }
}
```

- `try { ... }` wraps the final expression in `Result:Ok` and propagates any
  `FlowSignal:Propagate` out of the block automatically—perfect for composing
  fallible helpers.
- The postfix `?` operator unwraps `Result:Ok` (returning the payload) or
  triggers an early `Err` by propagating the current value outward. Both the
  interpreter and build-mode backend now honor this control flow.

See `demos/error_handling_demo.prime` for a full example that mixes `try {}`,
`?`, and the multiple-return `span_measurements` helper.

### Static Type Checking

Before interpreting or compiling, Prime runs a lightweight checker that
verifies:

- `let` bindings and function returns honor their declared types.
- Function calls supply the right number of arguments/type arguments.
- `match` expressions cover every enum variant unless a wildcard arm is present.
- Mutable borrows (`&mut foo`) are checked so only one active reference to a
  binding exists at a time; shadowing or leaving a scope releases the borrow.
- Interface conformance is enforced at runtime (the interpreter rejects missing
  impls); static checking for interfaces is not implemented yet.

Type errors are reported with file/line spans, so you can fix mistakes before
running code.

### Map & Slice Literals

- `["Prime Hero", "Sparrow"]` builds a dynamic slice without calling
  `slice_new`/`slice_push`.
- `#{ "Harbor sweep": 175, "Ridge scouting": 250 }` constructs a map literal.
  If the literal has at least one entry, Prime now infers `Map[string, T]`
  automatically so you can skip type annotations on the binding.
- Slices and maps expose methods for common operations:
  - `squad.len()`, `squad.get(0)`, `squad.push("name")`
  - `rewards.get("Harbor sweep")`, `rewards.insert("bonus", 42)`, `rewards.len()`
- Heap helpers now have ergonomic method aliases:
  `explorers.get(0)`, `rewards.get("Harbor sweep")`, `counter.box_set(10)`.

### Collection Iteration

- `for hero in squad { ... }` consumes slices (and references to slices) element
  by element.
- `for entry in rewards { ... }` walks a map, binding each `(string, T)` tuple so
  you can destructure it with `let (mission, payout) = entry;` before using the
  key/value pair.

### Built-in I/O & APIs

- `out(expr)` prints values (strings, numbers, tuples, structs implement a debug
  representation).
- `match Option[...]` and `Result[...]` mirror Rust semantics for optional and
  fallible flows.

```prime
import math;

const LIMIT: int32 = 4;

fn divmod(a: int32, b: int32) -> (int32, int32) {
  (a / b, a % b)
}

fn main() {
  let (quot, rem) = divmod(22, LIMIT);
  out((quot, rem));
}
```

### Methods & Calls

Interfaces/impls provide the “method” surface. A function declared inside an
`impl Interface[Type] for Type` block can be called with dot syntax, and the
compiler rewrites it to the underlying function:

```prime
interface Renderable[T] { fn render(self: T); }

impl Renderable[PlayerPanel] for PlayerPanel {
  fn render(panel: PlayerPanel) {
    out(panel.label);
  }
}

let PlayerPanel panel = build_panel();
panel.render();  // rewrites to Renderable::render(panel)
```

- The first parameter type controls ownership: plain `T`, `&T`, or `&mut T`.
- Interfaces act like traits; implementations live in `impl` blocks. You still
  write free functions as needed; dot-call is sugar over the impl methods.

### Types & Memory Model

- Numbers: `int8`…`int64`, `uint8`…`uint64`, `isize`/`usize`, `float32`,
  `float64`, plus `bool`, `rune`, and `string`. Numeric literals pick up the
  annotated type (e.g., `let uint8 x = 0;`, `let float64 y = 0.0;`) and default
  to `int32`/`float32` only when the surrounding context is untyped. Use
  `cast[T](value)` to convert explicitly between numeric widths/signedness.
- Concurrency helpers: `channel[T]()` yields `(Sender[T], Receiver[T])`; `recv_timeout(rx, ms)`
  returns `Option[T]` after waiting; `sleep(ms)` pauses the current task.
- Structs/enums are value types; assignment copies the fields. Heap helpers
  include `Box[T]`, slices (`[]T`), and maps (`Map[string, T]`) with literals,
  methods (+/- borrow checks), and iteration support.
- References (`&T`) wrap the value in an `Rc<RefCell<_>>`, so borrowing a value
  moves it to the heap until all references drop. Raw pointers (`*T` / `*mut T`)
  are available via `ptr` / `ptr_mut` over existing references and dereference
  with `*`; there is no manual `alloc/free`.

With these primitives you can already compose larger programs (see
`main.prime`/`types.prime`) while keeping the future roadmap—better memory
controls—in mind.

### ABI, FFI & Targets

- Codegen targets the host triple by default (LLVM) and assumes pointer width
  matches the host. Integer values lower than 128 bits are represented with
  their exact width; the backend lowers `int32`/`uint32` et al. directly.
- Runtime ABI uses a stable set of C-callable shims (`prime_int_new`,
  `prime_float_new`, `prime_string_new`, etc.) defined in `runtime_abi.rs`. FFI
  is not finalized yet; linking against external symbols should be treated as
  experimental until the ABI is locked for 1.0.
- Codegen limits: 128-bit integers are the widest supported scalar; larger
  widths or non-native pointer sizes are currently unsupported.


### Build-Mode Closures

- Representation: closure values are a struct `{ env_ptr: ptr, fn_ptr: ptr, id: usize }`. Calls use the convention `fn(env_ptr, args...)`.
- Environments: heap-allocated opaque structs; build-mode compilation now tracks allocations and frees them via the runtime `prime_env_free` shim once execution finishes.
- Captures: move captures remain, and immutable/mutable reference captures are allowed when the borrow is valid for the closure lifetime (mutable references respect the existing borrow checker). Capture layouts come from typechecking (no opaque synthesis).
- Calling: function pointers are bitcast to the exact closure signature; arity mismatches raise an error before codegen.
- Supported types: primitives, tuples (including multi-return), slices/maps/boxes (captured as runtime handles), references, channels/join handles, pointers, and nested closures (including higher-order and tuple-returning examples as in `workspace/demos/closures/closure_demo.prime`).
- Limitations: captured heap handles are still opaque in build-time evaluation but can be mutated/read through handle-aware builtins (`push`/`insert`/`get` route to runtime ops when a handle is present); user-defined destructors are still absent.
- Build snapshots (with `PRIME_BUILD_PARALLEL=1`) now serialize closures, channel endpoints with queued messages/closed state, join results, pointers/references, and boxed/slice/map handles so parallel build replay mirrors runtime captures. Dynamic indices/ranges route through runtime handles when present to keep build/run parity for collection access.

## CLI Usage

Build the tool once:

```bash
cargo build               # dev workflow in-place
cargo install --path .    # install `prime-lang` onto your $PATH
```

Commands:

| Command                              | Description                                  |
|--------------------------------------|----------------------------------------------|
| `prime-lang run main.prime`          | Interpret the program                         |
| `prime-lang build main.prime`        | Emit LLVM IR → object → native `./output`     |
| `prime-lang lint --file main.prime`  | One-shot diagnostics                          |
| `prime-lang lint --file main.prime --watch` | Watch for changes & re-lint          |
| `prime-lang fmt --file main.prime`   | Pretty-print to stdout                        |
| `prime-lang fmt --file main.prime --write` | Format in place                       |
| `prime-lang lsp`                     | Start the stdio Language Server               |

## Checking Examples & Regression Safety

Use the bundled script to lint every module registered in `prime.toml` (and
optionally run them) to catch parser/typecheck regressions early:

```bash
./scripts/check_examples.sh            # lint all modules
PRIME_RUN_EXAMPLES=1 ./scripts/check_examples.sh  # also run them
./scripts/ci.sh                        # fmt/clippy/tests (+ docs smoke), matches CI
```

This script keeps demos like `lab_demo.prime` in sync with the CLI and grammar
changes so editor highlights and `prime-lang docs` snippets stay reliable.


## LSP Integration (Neovim example)

`prime-lang lsp` implements diagnostics, hover, document symbols, and
`textDocument/formatting`. Configure it with `nvim-lspconfig`:

```lua
return {
  {
    "neovim/nvim-lspconfig",
    ft = { "prime" },
    config = function()
      -- teach Neovim that *.prime => filetype=prime
      vim.filetype.add {
        extension = { prime = "prime" },
      }

      local lspconfig = require "lspconfig"
      local configs = require "lspconfig.configs"

      if not configs.primelang then
        configs.primelang = {
          default_config = {
            cmd = { "prime-lang", "lsp" },
            filetypes = { "prime" },
            root_dir = function(fname)
              local git = vim.fs.find(".git", { path = fname, upward = true })[1]
              if git then return vim.fs.dirname(git) end
              return vim.fs.dirname(fname)
            end,
          },
        }
      end

      lspconfig.primelang.setup {
        on_attach = function(client, bufnr)
          local map = function(lhs, rhs, desc)
            vim.keymap.set("n", lhs, rhs, { buffer = bufnr, desc = desc })
          end
          map("gd", vim.lsp.buf.definition, "Go to definition (Prime)")
          map("gD", vim.lsp.buf.declaration, "Go to declaration (Prime)")
          map("<leader>cr", vim.lsp.buf.rename, "Rename symbol (Prime)")

          if client.server_capabilities.documentFormattingProvider then
            vim.api.nvim_create_autocmd("BufWritePre", {
              buffer = bufnr,
              callback = function() vim.lsp.buf.format { bufnr = bufnr } end,
            })
          end
        end,
      }
    end,
  },
}
```

### LSP & IDE Support

`prime-lang lsp` speaks the same protocol as rust-analyzer, so Neovim,
VS Code, Helix, and any other LSP client can plug in. Capabilities include:

- diagnostics with ranges, severity, and warnings for unused bindings
- hover, and signature help
- structured completions (locals, structs, enums, constants, keywords, macro param kinds like `tokens`/`repeat`/`block` plus hygiene escape `@`), plus built-ins used by the demos: heap helpers, `in`/`out`, channels (`recv_task`/`recv_timeout`/`sleep_task`/`join`), timers (`sleep_ms`/`now_ms`), filesystem (`fs_*`), map helpers (`map_keys`/`map_values`), and embedded-only GPIO/delay/reset suggestions when editing `no_std` targets
- `textDocument/formatting` via the built-in formatter
- document symbols
- method completions include interface impls, so `hero.label()` (from
  `impl Nameable for Hero`) pops up next to data fields just like direct methods.

Minimal `nvim-lspconfig` setup (Blink’s capabilities optional but recommended):

```lua
local lspconfig = require "lspconfig"
local configs = require "lspconfig.configs"
local capabilities = require("blink.cmp").get_lsp_capabilities()

vim.filetype.add { extension = { prime = "prime" } }

if not configs.primelang then
  configs.primelang = {
    default_config = {
      cmd = { "prime-lang", "lsp" },
      filetypes = { "prime" },
      root_dir = require("lspconfig.util").root_pattern(".git"),
    },
  }
end

lspconfig.primelang.setup {
  capabilities = capabilities,
  on_attach = function(client, bufnr)
    local map = function(lhs, rhs, desc)
      vim.keymap.set("n", lhs, rhs, { buffer = bufnr, desc = desc })
    end
    map("gd", vim.lsp.buf.definition, "Go to definition (Prime)")
    map("gD", vim.lsp.buf.declaration, "Go to declaration (Prime)")
    map("<leader>cr", vim.lsp.buf.rename, "Rename symbol (Prime)")

    if client.server_capabilities.documentFormattingProvider then
      vim.api.nvim_create_autocmd("BufWritePre", {
        buffer = bufnr,
        callback = function() vim.lsp.buf.format { bufnr = bufnr } end,
      })
    end
  end,
}
```

> **AstroNvim tip:** drop the same `on_attach` block (or use `astrocore`'s
> `opts.mappings.n["gd"] = ...`) inside your `user/plugins/*.lua` spec so that
> `gd`/`gD` invoke the LSP. Without explicit keymaps Neovim falls back to its
> built-in backward search, which only finds definitions that appear above the
> cursor.

### Treesitter Highlighting & Symbols

Use the bundled grammar under `tree-sitter/prime` (or the upstream
`prime_lang_treesitter` repo) plus a small Lazy spec to wire up syntax
highlighting, locals view, and outline panes:

```lua
return {
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",

    opts = function(_, opts)
      opts = opts or {}

      -- 1) Enable highlight globally
      opts.highlight = opts.highlight or {}
      opts.highlight.enable = true

      -- don't disable for prime
      if type(opts.highlight.disable) == "table" then
        for i = #opts.highlight.disable, 1, -1 do
          if opts.highlight.disable[i] == "prime" then table.remove(opts.highlight.disable, i) end
        end
      end

      -- 2) Filetype for *.prime
      vim.filetype.add {
        extension = { prime = "prime" },
      }

      -- 3) Register parser
      local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
      ---@diagnostic disable-next-line: inject-field
      parser_config.prime = {
        install_info = {
          -- use the vendored grammar in this repo
          url = vim.fs.normalize(vim.fn.getcwd() .. "/tree-sitter/prime"),
          files = { "src/parser.c" },
        },
        filetype = "prime",
      }

      -- 4) Map ft -> language
      vim.treesitter.language.register("prime", "prime")

      -- 5) Queries for quick preview (dev mode)
      local query_root = vim.fn.stdpath "data" .. "/site/queries/prime"
      vim.fn.mkdir(query_root, "p")

      return opts
    end,
  },
}
```

## Prime-Lang Icon Plugin:
```lua
-- lua/plugins/prime-icons.lua
return {
  {
    "nvim-mini/mini.icons",
    -- AstroNvim already loads this; we just extend its config
    opts = function(_, opts)
      opts = opts or {}

      -- ensure extension table exists
      opts.extension = opts.extension or {}

      -- icon for .prime files
      opts.extension.prime = {
        glyph = "",
        hl = "MiniIconsCyan",
      }

      -- (optional) also by filetype, in case something uses that path
      opts.filetype = opts.filetype or {}
      opts.filetype.prime = opts.extension.prime

      return opts
    end,
  },
}

```

Run `:TSInstall prime` once; afterwards Tree-sitter handles highlighting and
Aerial (or Snacks) shows symbols even before the LSP attaches.


## Development & Troubleshooting

* **Linting loop** – `prime-lang lint --file main.prime --watch` streams miette
  diagnostics to stdout; great for quickfix-based workflows.
* **Formatter** – `prime-lang fmt --write main.prime` keeps files canonical.
* **Crate versions** – `Cargo.toml` pins `tokio 1.48`, `tower-lsp-server 0.22`,
  `notify 8.2`, etc. Run `cargo update` cautiously.
* **Treesitter ABI warning** – run `tree-sitter init` inside
  `prime_lang_treesitter` so `tree-sitter.json` records ABI 15 if needed.

With the CLI, LSP, formatter, lint watch, and Tree-sitter grammar in place, you
get a complete Prime editing experience: diagnostics and formatting via LSP,
syntax highlighting via Treesitter, and a sample toolchain for learning how
language infrastructure fits together.

### Recent Updates (current)

- Build-mode now lowers non-constant integer/float comparisons to LLVM `icmp`/`fcmp`, so `if/while` conditions and pattern guards can depend on runtime values without bailing out of the host build.
- Booleans are SSA-backed (`BoolValue`), so dynamic conditions work in build mode and the emitted binaries while still folding constants for snapshots and control-flow checks.
- Mutable scalars (ints/bools) use stack slots on all targets, keeping prints/format strings in sync with in-loop mutations for both host and embedded builds.

### Recent Updates (November 2025)
