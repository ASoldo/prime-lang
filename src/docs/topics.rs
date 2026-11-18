pub struct Topic {
    pub key: &'static str,
    pub title: &'static str,
    pub summary: &'static str,
    pub category: &'static str,
    pub aliases: &'static [&'static str],
    pub sections: &'static [TopicSection],
}

impl Topic {
    pub fn all_keys(&self) -> impl Iterator<Item = &str> {
        std::iter::once(self.key).chain(self.aliases.iter().copied())
    }
}

pub struct TopicSection {
    pub title: &'static str,
    pub snippet: &'static str,
    pub explanation: &'static str,
}

pub const TOPICS: &[Topic] = &[
    Topic {
        key: "core-syntax",
        title: "Prime Core Syntax",
        category: "Core Syntax",
        summary: "Module headers, imports, bindings, and struct declarations live at the heart of every `.prime` file. These constructs are parsed into the AST in `src/language/ast.rs` before the compiler or interpreter sees them.",
        aliases: &[
            "core",
            "syntax",
            "module",
            "modules",
            "import",
            "imports",
            "const",
            "constants",
            "let",
            "binding",
            "bindings",
            "fn",
            "function",
            "functions",
            "declaration",
            "declarations",
        ],
        sections: &[
            TopicSection {
                title: "Module headers & imports",
                snippet: r#"module app::main;

import core::types;
import core::types::prelude::{*};
import demos::error_handling;"#,
                explanation: "Entry modules declare `module path::to::module;`. Libraries and test modules use `library` and `test` headers. Imports use the same canonical segments, allowing the manifest loader and the LSP to resolve dependencies unambiguously. Modules can export a `prelude` block; importing the module applies it automatically, or you can pull it explicitly via `::prelude::{*}`.",
            },
            TopicSection {
                title: "Bindings, constants, and functions",
                snippet: r#"const TRAINING_LIMIT: int32 = 3;

fn divmod(a: int32, b: int32) -> (int32, int32) {
  (a / b, a % b)
}

fn assign_example() {
  let int32 value = 5;
  let mut int32 counter = value + 1;
  counter = counter + 2;
}"#,
                explanation: "`const` introduces immutable globals, `let` bindings are immutable unless `mut` is specified, and function signatures list each parameter plus tuple returns. The typechecker enforces the declared shapes before interpretation or code generation.",
            },
            TopicSection {
                title: "Struct declarations",
                snippet: r#"struct Vec2 {
  x: float32;
  y: float32;
}

struct Player {
  name: string;
  level: int32;
  stats: Stats;
}"#,
                explanation: "Struct layouts mirror the syntax in `types.prime`. Embedded structs flatten into their parents, so code such as `player.stats.hp` works without inheritance. Patterns reuse the same syntax with `Struct{ field: binding, .. }`.",
            },
        ],
    },
    Topic {
        key: "embedded",
        title: "Embedded / ESP32 (Xtensa)",
        category: "Platforms",
        summary: "Build and flash the bundled ESP32 blink demo using the ESP-IDF toolchains, ROM linker scripts, and manifest-provided environment overrides; async/await + channels + spawn/join use runtime handles in no_std.",
        aliases: &["esp32", "xtensa", "embedded", "bare-metal", "no_std"],
        sections: &[
            TopicSection {
                title: "Blink manifest settings",
                snippet: r#"[build]
target = "xtensa-esp32-espidf"
platform = "esp32"

[build.flash]
enabled = false"#,
                explanation: "The complete example is workspace/demos/esp32_blink/prime.toml. Set IDF_PATH and IDF_BUILD_ROOT to the ESP-IDF checkout and generated linker scripts as described in the README.",
            },
            TopicSection {
                title: "Build the classic ESP32 image",
                snippet: r#"prime-lang build workspace/demos/esp32_blink/esp32_blink.prime --name esp32_blink --no-flash"#,
                explanation: "This checks Xtensa compilation, linking and offline image conversion. Observe GPIO2 and UART on a compatible board to verify execution.",
            },
            TopicSection {
                title: "Toolchain detection",
                snippet: r#"Defaults if env is unset:
- RUSTUP_TOOLCHAIN=esp
- CARGO_TARGET_DIR=~/.cache/prime-xtensa
- LLVM_SYS_201_PREFIX=~/.espressif/tools/esp-clang/esp-clang
- PATH prepends esp-clang/bin and the first xtensa-esp-elf/*/xtensa-esp-elf/bin it finds
- CARGO_TARGET_XTENSA_ESP32_ESPIDF_LINKER=xtensa-esp32-elf-gcc"#,
                explanation: "The CLI applies manifest env first; if absent it auto-detects standard ESP-IDF installs under `~/.espressif`. Override any key in `[build.toolchain.env]` to point at non-standard locations.",
            },
            TopicSection {
                title: "UART logging from Prime",
                snippet: r#"fn main() {
  let int32 my_var = 5;
  out(`hello from esp32 blink demo and value is {my_var}`);
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
}"#,
                explanation: "`out(...)` prints over UART via ROM `ets_printf`. Strings, format strings, ints (constants), and bools are rendered; each `out` ends with a newline so successive calls don’t run together. Async/await + channels attach runtime handles automatically, so `recv_task`/`sleep_task` block through the Xtensa runtime; `spawn`/`join` also map to the task runtime for parity with build/run. The demo includes a small Result+`?` async probe, a timeout probe, and a channel/task pool reuse loop to mirror build/run behavior. Use these to trace startup and loop activity alongside LED toggles. The demo uses GPIO2 for LED and an external GPIO18->GND button for input logging; BOOT/EN are left for the auto-boot/reset circuit.",
            },
        ],
    },
    Topic {
        key: "drop",
        title: "Drop & Cleanup",
        category: "Memory & Ownership",
        summary: "RAII-style cleanup for structs/enums: implement `Drop` with `fn drop(&mut self)` to release resources at scope exit. Drops run once, respect moves, and interleave with defers in LIFO order in both run and build modes.",
        aliases: &["drop", "cleanup", "raii", "destructor", "defer-drop"],
        sections: &[
            TopicSection {
                title: "Implementing Drop",
                snippet: r#"struct Tracker { label: string; log: &mut []string; }

impl Drop for Tracker {
  fn drop(self: &mut Tracker) {
    self.log.push(self.label);
  }
}"#,
                explanation: "Add an `impl Drop for Type` block with a single `fn drop(self: &mut Type)` method (no returns). The language forbids panics in drop bodies at runtime. Only structs/enums can implement Drop; references/pointers are skipped.",
            },
            TopicSection {
                title: "When drops run",
                snippet: r#"fn demo(log: &mut []string) {
  let Tracker first = Tracker { label: "first", log };
  defer log.push("defer");
  let Tracker second = Tracker { label: "second", log };
} // log => ["second", "defer", "first"]"#,
                explanation: "Drops are scheduled when a binding is created, run once on scope exit, and are skipped after a move from that binding. Cleanup is LIFO per scope: later bindings drop first, interleaving with `defer` in insertion order.",
            },
        ],
    },
    Topic {
        key: "quick-start",
        title: "Quick Start",
        category: "Basics",
        summary: "Install the CLI from source, run the interpreter, and try the bundled demos to validate the toolchain end-to-end.",
        aliases: &[
            "quick",
            "start",
            "install",
            "setup",
            "getting-started",
            "clone",
            "run-demo",
            "build-demo",
        ],
        sections: &[
            TopicSection {
                title: "Install the CLI",
                snippet: r#"git clone https://github.com/ASoldo/prime-lang.git
cd prime-lang
cargo install --locked --path ."#,
                explanation: "Use the Rust and LLVM prerequisites in docs/GettingStarted.md.",
            },
            TopicSection {
                title: "Run and build the sample",
                snippet: r#"prime-lang run workspace/demos/channel_demo/channel_demo.prime
prime-lang build workspace/demos/channel_demo/channel_demo.prime --name channel_demo
./.build.prime/channel_demo/channel_demo"#,
                explanation: "Run these commands from the checkout root. Both executions print 6, the sum of the three channel messages.",
            },
            TopicSection {
                title: "Print / generate / serve docs",
                snippet: r#"prime-lang docs
prime-lang docs --list
prime-lang docs --query for,match
prime-lang docs --generate --out docs.html
prime-lang docs --serve --port 7878"#,
                explanation: "`prime-lang docs` renders the built-in reference topics, `--list` shows topic keys and aliases, and `--query` filters to specific topics. `--generate` produces a single-page HTML app from workspace doc comments (`///` and `//!`) and `--serve` hosts it locally over HTTP for browsing. When serving, the docs app auto-regenerates and reloads as you edit `.prime` files.",
            },
        ],
    },
    Topic {
        key: "prime-basics",
        title: "Prime Basics: CLI & Workspace",
        category: "Basics",
        summary: "The `prime-lang` binary exposes subcommands for running, building, linting, formatting, initializing, adding modules, and printing these docs. Workspace metadata lives in `prime.toml`.",
        aliases: &[
            "basics",
            "cli",
            "command",
            "commands",
            "run",
            "build",
            "lint",
            "fmt",
            "format",
            "docs",
            "init",
            "add",
            "help",
            "workspace",
            "prime",
            "manifest",
            "prime.toml",
            "modules",
            "module-entries",
        ],
        sections: &[
            TopicSection {
                title: "Essential CLI commands",
                snippet: r#"prime-lang check workspace/demos/channel_demo/channel_demo.prime
prime-lang lint workspace/demos/channel_demo/channel_demo.prime --watch
prime-lang fmt workspace/demos/channel_demo/channel_demo.prime
prime-lang lsp
prime-lang docs --list"#,
                explanation: "Run these commands independently. Lint watches until interrupted. The formatter previews changes and the language server communicates over stdio.",
            },
            TopicSection {
                title: "Read user input with `in[T]`",
                snippet: r#"let age = in[int32]("Age: ")?;
match in[float64](`Temp {label}: `, label) {
  Ok(temp) => out(`got {temp}`),
  Err(msg) => out(`input error: {msg}`),
}"#,
                explanation: "`in[T](prompt, ...) -> Result[T, string]` reads a line from stdin, parses it to the requested type, and returns `Ok`/`Err` so callers stay in control. Prompts may be format strings; type arguments are required. Supported in interpreter mode and host builds; embedded/no_std targets currently reject `in` (no stdin).",
            },
            TopicSection {
                title: "Example programs",
                snippet: r#"prime-lang run workspace/demos/patterns/pattern_demo.prime        # module demos::patterns
prime-lang run workspace/demos/error_handling/error_handling_demo.prime # demos::error_handling
prime-lang run workspace/demos/lab_demo/lab_demo.prime                  # demos::lab_demo
prime-lang run workspace/demos/ns/ns_demo.prime                         # demos::ns"#,
                explanation: "Demos ship as workspace members so you can exercise patterns, error handling, namespaces, interfaces, and borrowing. Library-only modules like `workspace/core/types.prime` and `workspace/pkg/lib/pkg_lib.prime` omit `main` intentionally and are meant to be imported.",
            },
            TopicSection {
                title: "prime.toml manifest",
                snippet: r#"manifest_version = "3"

[package]
name = "demo-app"
version = "0.1.0"

[module]
name = "demo_app::main"
path = "main.prime"
visibility = "pub"

[libraries]
core_types = { name = "core::types", path = "../core/types.prime", visibility = "pub" }
"#,
                explanation: "`project::manifest::PackageManifest` reads the workspace `prime.toml`, then each member's `prime.toml` for `[module]`, `[modules]`, `[libraries]`, and `[tests]` entries. Inline tables or `items` arrays are accepted. Entries must match each file's `module ...;`, `library ...;`, or `test ...;` header so `load_package` can resolve imports across the workspace before typechecking or interpretation.",
            },
            TopicSection {
                title: "Add a library to a new package",
                snippet: r#"prime-lang new adventure --bin
cd adventure
prime-lang add adventure::items --path items.prime --library"#,
                explanation: "Add records the library and creates a source stub. Import the library from an entry module.",
            },
            TopicSection {
                title: "Interpreter concurrency (beta)",
                snippet: r#"let (tx, rx) = channel[int32]();
let handle = spawn (send(tx, 1));
let Option[int32] first = recv(rx);
join(handle);"#,
                explanation: "A sender can enqueue a value and a receiver can consume it. In the host interpreter recv waits for a value or channel closure.",
            },
            TopicSection {
                title: "Checking the examples",
                snippet: r#"./scripts/check_examples.sh"#,
                explanation: "The script lints and type-checks declared workspace sources. The inventory includes libraries and tests, as well as programs that need media or a board.",
            },
        ],
    },
    Topic {
        key: "builtins-fs-time-iter",
        title: "Built-ins: fs, time, iterators",
        category: "Built-ins",
        summary: "Minimal stdlib-style helpers for file I/O, timing, and iterator ergonomics. The host runtime supplies file and time operations.",
        aliases: &[
            "fs", "time", "iter", "iterator", "sleep_ms", "now_ms", "fs_read", "fs_write",
        ],
        sections: &[
            TopicSection {
                title: "File I/O",
                snippet: r#"let string path = "notes.txt";
match fs_write(path, "contents") {
  Result::Ok(()) => out("write ok"),
  Result::Err(msg) => out(`write failed: {msg}`),
}
out(`exists? {fs_exists(path)}`);
match fs_read(path) {
  Result::Ok(text) => out(`read: {text}`),
  Result::Err(msg) => out(`read failed: {msg}`),
}"#,
                explanation: "The interpreter and native executable access files when they run. Relative paths use the current working directory. fs_write_bytes accepts a []uint8 for binary data.",
            },
            TopicSection {
                title: "Time helpers",
                snippet: r#"let int64 start = now_ms();
sleep_ms(5);
let int64 end = now_ms();
out(`delta: {end - start}`);"#,
                explanation: "On the host, now_ms reads Unix time in milliseconds. sleep_ms takes a delay in milliseconds. Embedded timers use a target-specific clock.",
            },
            TopicSection {
                title: "Iterator helpers",
                snippet: r#"let []int32 nums = [1, 2, 3];
let Iterator[int32] iter = nums.iter();
match iter.next() {
  Option::Some(first) => out(`first: {first}`),
  Option::None => out("empty"),
}

let Map[string, int32] scores = #{"a": 1, "b": 2};
for key in scores.map_keys() { out(key); }
for value in scores.map_values() { out(value); }"#,
                explanation: "Slices and maps expose `.iter()` returning an `Iterator[T]` with `next()` yielding `Option`. `map_keys`/`map_values` return slices for quick traversal. For loops also accept `Iterator` values directly.",
            },
        ],
    },
    Topic {
        key: "examples",
        title: "Bundled Examples & Outputs",
        category: "Basics",
        summary: "The repository includes runnable `.prime` programs that double as documentation snippets for borrowing, pattern matching, interfaces, and error handling.",
        aliases: &[
            "examples",
            "demos",
            "main",
            "lab",
            "pattern",
            "error-handling",
            "interface",
            "borrow",
            "heap",
            "pkg",
            "validated",
        ],
        sections: &[
            TopicSection {
                title: "Feature tour modules",
                snippet: r#"- workspace/app/main.prime – modules, structs/enums, interfaces, ownership, UI-ish printing
- workspace/demos/patterns/pattern_demo.prime – matches/destructuring over tuples, maps, structs, slices
- workspace/demos/error_handling/error_handling_demo.prime – Result, try { }, and ? propagation
- workspace/demos/lab_demo/lab_demo.prime – range loops, map destructuring, mutable refs, generic interface
- workspace/demos/pointer_demo/pointer_demo.prime – raw pointers derived from references plus stored ranges
- workspace/demos/no_std_parity/main.prime – host-run parity fixture that targets embedded/no_std for async/Result/timeouts"#,
                explanation: "Paths are relative to the repository root. Inspect a program with prime-lang check or execute its entry module with prime-lang run.",
            },
            TopicSection {
                title: "Libraries and namespace demos",
                snippet: r#"- workspace/core/types.prime / workspace/pkg/lib/pkg_lib.prime – library-only modules (no main)
- workspace/demos/ns/ns_demo.prime – namespace overloading with foo/bar labels
- workspace/pkg/app/pkg_app.prime – banner/promotion output paired with pkg_lib"#,
                explanation: "Import libraries from an entry module. Use prime-lang check to inspect a library directly.",
            },
            TopicSection {
                title: "A demo with expected output",
                snippet: r#"prime-lang run workspace/demos/channel_demo/channel_demo.prime"#,
                explanation: "Expected output is 6. For a bounded, live TCP example on the host see docs/LoopbackWebServer.md. Interactive Tetris needs a display and media files from the checkout.",
            },
            TopicSection {
                title: "Numeric primitives",
                snippet: r#"- signed:  int8, int16, int32, int64, isize
- unsigned: uint8, uint16, uint32, uint64, usize
- floats:   float32, float64
- literals: `0`/`0.0` adopt the annotated type; default to int32/float32 only when untyped
- casts:    `cast[T](value)` explicitly converts between numeric widths/signedness"#,
                explanation: "Arithmetic, ranges, and bitwise ops accept the full Rust-like numeric set. Context drives literal types (`let uint8 f = 0;` works without casts). Mixed numeric operations require matching widths/signedness—use `cast[uint32](value)` (etc.) to opt into conversions. The typechecker surfaces clear errors when operands diverge.",
            },
            TopicSection {
                title: "Concurrency helpers",
                snippet: r#"- spawn/task: `spawn expr` returns JoinHandle[T]
- channels: channel[T]() -> (Sender[T], Receiver[T]); send/recv/recv_timeout
- timing:   sleep(ms) pauses the current task"#,
                explanation: "recv_timeout bounds a receive. For deadlines on Task values, see await_timeout in docs/AsyncAwait.md.",
            },
            TopicSection {
                title: "Runtime ABI",
                snippet: r#"src/language/runtime_abi.rs  # LLVM function declarations
src/runtime/abi.rs           # target-specific Rust exports
src/runtime/distribution/    # bundled manifests and lockfiles"#,
                explanation: "Generated code calls internal C-compatible entry points for values, tasks, channels and platform services. Host, Xtensa ESP32 and RV32IMC ESP32-C3 builds need matching runtime archives. See docs/RuntimeFFI.md for widths and target limitations.",
            },
        ],
    },
    Topic {
        key: "prime-intermediate",
        title: "Prime Intermediate: Control Flow & Patterns",
        category: "Intermediate",
        summary: "Control-flow constructs (`if`, `match`, loops, destructuring) show up throughout `pattern_demo.prime` and `error_handling_demo.prime`. They are validated by the typechecker before runtime.",
        aliases: &[
            "intermediate",
            "control-flow",
            "if",
            "else",
            "if let",
            "match",
            "pattern",
            "patterns",
            "switch",
            "for",
            "loop",
            "loops",
            "while",
            "while let",
            "destructure",
            "guard",
            "range",
        ],
        sections: &[
            TopicSection {
                title: "Branching with multiple returns",
                snippet: r#"fn span_measurements(seed: int32) -> (int32, int32) {
  if seed < 0 {
    return 0, 0;
  }
  if seed > 100 && seed < 150 {
    return seed - 10, seed + 10;
  } else if seed >= 150 {
    return seed - 5, seed + 5;
  }
  let int32 midpoint = seed / 2;
  return midpoint - 5, midpoint + 5;
}"#,
                explanation: "Standard `if`/`else if` chains allow early `return` statements. The typechecker ensures every path returns the `(int32, int32)` tuple promised in the signature.",
            },
            TopicSection {
                title: "Pattern matching",
                snippet: r#"fn describe_reading(reading: (string, int32)) {
  match reading {
    ("temp", value) => out(`temperature reading {value}`),
    ("status", code) => out(`status code {code}`),
    _ => out("unrecognized reading"),
  }
}

fn summarize_stats(stats: Map[string, int32]) {
  match stats {
    #{ "hp": hp, "mp": mp } => out(`stats ready hp {hp} mp {mp}`),
    _ => out("missing stats"),
  }
}"#,
                explanation: "`match` supports tuples, maps, enums, structs, and slice patterns. Guards (`if condition`) refine matches, and every arm must cover the declared type. Destructuring binds the needed fields directly in each arm; enum variants qualify with `Enum::Variant` inside patterns and expressions.",
            },
            TopicSection {
                title: "Loops & iteration",
                snippet: r#"fn loop_merging() -> int32 {
  let mut int32 total = 0;
  for count in 0..2 {
    let &mut int32 alias = &mut total;
    *alias = *alias + count;
  }
  let []int32 entries = [3, 4];
  for entry in entries {
    let &mut int32 alias = &mut total;
    *alias = *alias + entry;
  }
  let []string notes = ["ok", "stable"];
  let mut int32 idx = 0;
  while let Some(note) = notes.get(idx) {
    out(`note {idx}: {note}`);
    idx = idx + 1;
  }
  loop {
    break;
  }
  total
}"#,
                explanation: "Range loops (`start..end`) and slice iteration move values directly into the loop binding. Stored ranges and any type with an `iter()` method (`Iterable[T]`) work with `for` as well. `while let` keeps looping as long as a pattern binds successfully, and `loop { ... }` is an infinite loop that exits with `break`. Borrow checking (see advanced topic) guarantees each body finishes before the next borrow.",
            },
            TopicSection {
                title: "Ranges as values",
                snippet: r#"fn store_range() -> Range[int32] {
  let Range[int32] span = 2..=5;
  span
}"#,
                explanation: "Range expressions produce `Range[T]` values inferred from their bounds so you can store, pass, or iterate later. Printing yields `start..end` or `start..=end` formats.",
            },
            TopicSection {
                title: "Collection literals & map/slice iteration",
                snippet: r#"let []string squad = ["Prime Hero", "Sparrow"];
for hero in squad {
  out(hero);
}

let Map[string, int32] rewards = #{
  "Harbor sweep": 175,
  "Ridge scouting": 250,
                };
                for entry in rewards {
                  let (mission, payout) = entry;
                  out(`mission {mission} pays {payout}`);
                }"#,
                explanation: "Slice literals inline values without helper calls and can be iterated directly. Map literals use `#{ ... }`, and `for entry in rewards` binds `(key, value)` tuples so you can destructure them before use. Use `squad[idx]` or `rewards[\"Harbor sweep\"]` to get `Option` values; methods like `.get`, `.insert`, and `.len` remain available too.",
            },
            TopicSection {
                title: "Printing and utility APIs",
                snippet: r#"fn divmod(a: int32, b: int32) -> (int32, int32) {
  (a / b, a % b)
}

fn main() {
  let (quot, rem) = divmod(22, 4);
  out((quot, rem));
}"#,
                explanation: "`out(expr)` prints strings, numbers, tuples, and structs using their debug representations. Tuple returns destructure cleanly at call sites, matching the example above.",
            },
        ],
    },
    Topic {
        key: "prime-advanced",
        title: "Prime Advanced: Traits, Generics, Borrowing",
        category: "Advanced",
        summary: "Advanced features include the borrow checker, generics, interfaces, heap helpers, and `Result`-based error propagation. Examples live in `borrow_demo.prime`, `generic_demo.prime`, `interface_generics_demo.prime`, `heap_demo.prime`, and `error_handling_demo.prime`.",
        aliases: &[
            "advanced",
            "generics",
            "generic",
            "interface",
            "interfaces",
            "trait",
            "traits",
            "impl",
            "implement",
            "borrow",
            "borrowing",
            "mut",
            "refs",
            "references",
            "error",
            "errors",
            "result",
            "try",
            "heap",
            "box",
            "slice",
            "map",
            "defer",
            "pointer",
            "pointers",
            "cleanup",
            "memory",
            "type-check",
            "diagnostics",
        ],
        sections: &[
            TopicSection {
                title: "Borrow checker scenarios",
                snippet: r#"fn alias_mut() {
  let mut int32 value = 10;
  let int32 copy = value;
  let &mut int32 first_ref = &mut value;
  *first_ref = copy + 5;
  let int32 final_value = *first_ref;
  out(`alias_mut copy {copy} final {}`, final_value);
}"#,
                explanation: "Mutable borrows (`&mut`) must be unique. The typechecker (`language::typecheck`) tracks lifetimes so each reference ends before the next borrow begins, even across loops and branches.",
            },
            TopicSection {
                title: "Generics & duplication",
                snippet: r#"fn identity[T](value: T) -> T { value }

fn duplicate[T](value: T) -> (T, T) {
  (value, value)
}

let pair = duplicate[string]("Prime Hero");"#,
                explanation: "Type parameters appear inside square brackets and can be inferred or supplied explicitly.",
            },
            TopicSection {
                title: "Interfaces and implementations",
                snippet: r#"interface Nameable[T] {
  fn label(self: T) -> string;
  fn pair(self: T, other: T) -> string;
}

impl Nameable[Hero] for Hero {
  fn label(hero: Hero) -> string { hero.name }
  fn pair(_hero: Hero, other: Hero) -> string { other.name }
}

let Hero prime = Hero{ name: "Prime Hero" };
out(prime.label());"#,
                explanation: "Interfaces behave like traits. `impl` blocks attach behaviors to structs so code can call `hero.label()` or `hero.pair(partner)` directly. Generic interfaces (`Nameable[T]`) let disparate structs share the same contract.",
            },
            TopicSection {
                title: "References, deref, and defer",
                snippet: r#"fn peek_hp(target: &Player) -> int32 {
  let Player snapshot = *target;
  snapshot.hp
}

fn refill_hp(target: &mut Player, amount: int32) {
  let Player snapshot = *target;
  *target = Player{ hp: snapshot.hp + amount, ..snapshot };
}

fn calibrate_station(name: string) {
  out(`Calibrating {name}`);
  defer out("Station teardown complete");
}"#,
                explanation: "`&T` shares read-only access, `&mut T` grants unique mutation, and explicit `*` deref copies values out of heap-backed cells. `ptr` / `ptr_mut` produce raw pointers from existing references when you need to sidestep borrow checks, and `defer` schedules cleanup at scope exit.",
            },
            TopicSection {
                title: "Raw pointers from references",
                snippet: r#"fn boosted_hp(target: &Player, delta: int32) -> int32 {
  let mut int32 hp = target.hp;
  let ptr = ptr_mut(&mut hp);
  *ptr = *ptr + delta;
  *ptr
}

fn read_hp(target: &Player) -> int32 {
  let ptr = ptr(&target.hp);
  *ptr
}"#,
                explanation: "Use `ptr`/`ptr_mut` to create raw pointers from existing references when you need to reuse the same storage outside borrow-checking contexts. Dereference with `*ptr` just like references—use sparingly.",
            },
            TopicSection {
                title: "Error propagation and heap helpers",
                snippet: r#"fn apply_boost(value: int32) -> Result[int32, string] {
  let int32 parsed = parse_energy(value)?;
  if parsed % 2 == 0 {
    Ok(parsed * 2)
  } else {
    Err("boost requires even energy")
  }
}

let Box[int32] counter = box_new(0);
bump_counter(&counter, 5);
out(`box counter -> {}`, counter.box_get());"#,
                explanation: "`Result[T, E]` plus the `?` operator short-circuits on failure, and `try { ... }` blocks wrap multi-expression workflows. In host Result-returning functions and try blocks, runtime `?` unwraps unit, string or scalar successes and preserves the error. Use `match` for aggregate success payloads. Import `core::types` to bring the `Result`/`Option` enums (and variants like `Ok`, `Err`, `Some`, `None`) into scope. Heap primitives such as `Box`, slices (`[]T`), and maps (`Map[K, V]`) provide helper methods like `.box_get()`, `.len()`, and either `.get(key)` or bracket indexing for safe ownership transfers; the same APIs work in build/run modes.",
            },
            TopicSection {
                title: "Collections and indexing",
                snippet: r#"let []string probe_notes = ["ok", "stable"];
let Map[string, int32] rewards = #{
  "Harbor sweep": 175,
  "Ridge scouting": 250,
};

out(probe_notes.get(0));
out(rewards.get("Ridge scouting"));
out(probe_notes[1]);
out(rewards["Harbor sweep"]);"#,
                explanation: "Inline literals build slices and maps without helper calls. Non-empty map literals infer `Map[string, T]` automatically, so the binding can omit a type hint. Bracket indexing (`probe_notes[0]`, `rewards[\"Ridge scouting\"]`) returns `Option` values alongside methods like `.get` and `.len`, and works uniformly in build/run modes for slices, arrays, and maps.",
            },
            TopicSection {
                title: "Type checking and diagnostics",
                snippet: r#"fn choose_code(code: int32) -> Result[int32, string] {
  if code < 0 {
    Err("negative code")
  } else {
    Ok(code)
  }
}

fn probe(stats: Map[string, int32]) {
  match stats {
    #{ "hp": hp, "mp": mp } => out(hp + mp),
    _ => out("incomplete"),
  }
}"#,
                explanation: "Prime validates return types, argument arity, and match exhaustiveness before running. Mutable borrows must be unique, and diagnostics point to spans so you can fix mistakes without guessing.",
            },
            TopicSection {
                title: "Memory model overview",
                snippet: r#"struct Counter { count: int32; }

fn copy_and_borrow() {
  let Counter base = Counter{ count: 1 };
  let Counter copy = base;     // value copy
  let &Counter view = &base;   // shared ref via Arc<Mutex<_>>
  out(view.count);
}"#,
                explanation: "Structs and enums are value types: assignment copies fields. References wrap data in a shared heap cell (`Arc<Mutex<_>>`) so borrows live on the heap until all refs drop. There is no manual alloc/free; heap helpers (Box, slices, maps) cover common cases.",
            },
            TopicSection {
                title: "Collect channel messages",
                snippet: r#"prime-lang run workspace/demos/channel_demo/channel_demo.prime"#,
                explanation: "The example spawns a producer, sums messages until the channel closes and joins the producer. For the supported target-specific scheduling and cancellation behavior see docs/AsyncAwait.md.",
            },
        ],
    },
    Topic {
        key: "editor-tooling",
        title: "Editor, LSP, and Grammar Tooling",
        category: "Tooling",
        summary: "Prime ships an LSP server, Tree-sitter grammar, optional icon plugin, and helper scripts so editors stay in sync with the compiler and formatter.",
        aliases: &[
            "editor",
            "lsp",
            "neovim",
            "vscode",
            "treesitter",
            "tree-sitter",
            "icons",
            "ide",
            "highlight",
            "symbols",
            "regression",
            "check",
            "troubleshoot",
        ],
        sections: &[
            TopicSection {
                title: "LSP server setup (Neovim sample)",
                snippet: r#"local lspconfig = require "lspconfig"
local configs = require "lspconfig.configs"
if not configs.primelang then
  configs.primelang = {
    default_config = {
      cmd = { "prime-lang", "lsp" },
      filetypes = { "prime" },
      root_dir = require("lspconfig.util").root_pattern(".git"),
    },
  }
end
lspconfig.primelang.setup {}"#,
                explanation: "The server uses stdio. See lazy/astronvim/plugins/prime-lang.lua for the repository configuration.",
            },
            TopicSection {
                title: "Tree-sitter grammar",
                snippet: r#"tree-sitter/prime/grammar.js
tree-sitter/prime/src/parser.c
lazy/astronvim/plugins/prime-lang.lua"#,
                explanation: "The repository contains the editor grammar, generated parser and an example AstroNvim configuration.",
            },
            TopicSection {
                title: "Completion coverage for demos",
                snippet: r#"recv_task(rx)
recv_timeout(rx, 5)
sleep_task(10)
fs_write("demo.txt", "hello")"#,
                explanation: "Completions surface the built-ins used across the bundled demos: channels (`send`/`recv`/`recv_task`/`recv_timeout`/`sleep_task`/`join`), heap (`box_*`/`map_*`/`slice_*`), input/output (`in`/`out`), timers (`sleep_ms`/`now_ms`), and filesystem helpers (`fs_*`). Module targets from the manifest drive embedded hints: GPIO/delay/reset suggestions appear for `no_std` targets, while host-only `fs_*` completions stay hidden on embedded files.",
            },
            TopicSection {
                title: "Prime icon plugin",
                snippet: r#"opts.extension.prime = {
  glyph = "",
  hl = "MiniIconsCyan",
}
opts.filetype.prime = opts.extension.prime"#,
                explanation: "The optional Mini Icons configuration associates .prime files with a glyph.",
            },
            TopicSection {
                title: "Watch a source file",
                snippet: r#"prime-lang lint workspace/demos/channel_demo/channel_demo.prime --watch"#,
                explanation: "The watcher checks on startup and after edits. Press Ctrl+C to exit.",
            },
        ],
    },
    Topic {
        key: "macros",
        title: "Macros & hygiene",
        category: "Language Features",
        summary: "Macro parameters, hygiene escapes with `@`, repeat separators (including custom tokens), macro-aware completions, visibility (`pub`, `pub(package)`, private), and hover support that shows full macro declarations.",
        aliases: &["macro", "macro-hygiene"],
        sections: &[
            TopicSection {
                title: "Hygiene & explicit capture",
                snippet: r#"macro capture_outer() -> int32 {
  @value + 1
}"#,
                explanation: "Macro identifiers are hygienic by default. Prefix with `@` to intentionally capture a caller binding (e.g., `@value` reads the call-site `value`).",
            },
            TopicSection {
                title: "Repeat params & separators",
                snippet: r#"macro collect(values: repeat) -> (int32, int32, int32) {
  values
}

let (a, b, c) = ~collect(@sep = , 1, 2, 3);
let int32 total = ~collect(@sep = ;, { 1; }, { 2; }, 3);"#,
                explanation: "Repeat params split on the first top-level separator: comma joins into a tuple, semicolon joins into a sequential block. Use `@sep = <token>` immediately after the opening `(` (an optional comma after the prefix is accepted) to set the separator, e.g. `@sep = ,`, `@sep = ;`, or `@sep = |`. Repeat params support quantifiers: `repeat+` (default) and `repeat*` (zero-or-more).",
            },
            TopicSection {
                title: "LSP hover for macros",
                snippet: r#"macro tally(values: repeat) -> int32 {
  values
}"#,
                explanation: "Hover on macro names to see the full declaration (including body) and parameter kinds. Macros participate in navigation and rename alongside functions.",
            },
            TopicSection {
                title: "CLI macro expansion",
                snippet: r#"prime-lang expand workspace/tests/macros/macros.prime --line 24 --column 7
prime-lang expand workspace/tests/macros/macros.prime --print-expanded"#,
                explanation: "Use `prime-lang expand` to inspect generated code. With `--line/--column` (or `--offset`), it prints the macro trace and only the expanded items originating from that call—ideal for editor shortcuts. Without a position or with `--print-expanded`, it emits the fully expanded, formatted module.",
            },
        ],
    },
    Topic {
        key: "ownership",
        title: "Ownership, Moves, and Borrows",
        category: "Type System",
        summary: "Prime enforces a single active mutable borrow per binding, detects use-after-move for heap values, and blocks references to temporaries that would dangle.",
        aliases: &[
            "ownership",
            "borrow",
            "borrowing",
            "move",
            "moves",
            "reborrow",
            "dangling",
            "aliasing",
        ],
        sections: &[
            TopicSection {
                title: "Moves consume heap bindings",
                snippet: r#"let []string squad = ["alpha", "bravo"];
let []string redeployed = move squad;
// `squad` is now moved; using it afterwards reports \"was moved\""#,
                explanation: "The `move` expression transfers ownership of heap-backed values (Box, slices, maps). The typechecker remembers the move across branches, so later reads or method calls on the moved binding surface a \"was moved\" diagnostic.",
            },
            TopicSection {
                title: "Mutable borrows avoid aliasing",
                snippet: r#"let mut int32 value = 0;
let &mut int32 first = &mut value;
let &mut int32 second = &mut *first; // error: `value` is already mutably borrowed"#,
                explanation: "Only one `&mut` to a binding may be live at a time, even through aliases. The checker follows re-borrows through dereferences so nested `&mut` chains still honor exclusivity and produce actionable hover/diagnostic messages.",
            },
            TopicSection {
                title: "No dangling references to temporaries",
                snippet: r#"fn compute() -> int32 { 4 }

fn bad() {
  let &int32 alias = &compute(); // rejected: temporary would dangle
}"#,
                explanation: "Taking a reference to a temporary (call result, inline literal, or block expression) is rejected to prevent dangling pointers. Persist the value into a binding before borrowing so its lifetime is clear.",
            },
            TopicSection {
                title: "Diagnostics show borrowers, moves, and spans",
                snippet: r#"let mut int32 hp = 10;
let &mut int32 a = &mut hp;
let &mut int32 b = &mut hp; // error highlights first borrow span and binding origin

let []int32 roster = [1, 2];
let []int32 moved = move roster;
out(roster); // error points at the move site"#,
                explanation: "Borrow and move errors include the borrower name, the borrow span, and the binding origin. Build-mode mirrors the interpreter wording and spans, so diagnostics stay consistent between `prime-lang run` and `prime-lang build`.",
            },
        ],
    },
    Topic {
        key: "recent-updates",
        title: "Implementation status",
        category: "Meta",
        summary: "Current execution paths and limits.",
        aliases: &[
            "updates",
            "release",
            "changelog",
            "november-2025",
            "april-2026",
            "recent",
        ],
        sections: &[TopicSection {
            title: "Checking and building",
            snippet: r#"prime-lang check workspace/demos/channel_demo/channel_demo.prime
prime-lang build workspace/demos/channel_demo/channel_demo.prime --name channel_demo"#,
            explanation: "The checker validates the expanded AST. Native lowering can still report a code generation error, for example E0NATIVETRY when runtime `?` would unwrap an aggregate payload. See docs/CompilerFlow.md and docs/Checking.md.",
        }],
    },
];
