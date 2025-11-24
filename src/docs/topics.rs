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
import demos::error_handling;"#,
                explanation: "Every file begins with `module path::to::module;`. Imports use the same canonical segments, allowing the manifest loader (`project::package`) and the LSP to resolve dependencies unambiguously.",
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
                title: "Install & build locally",
                snippet: r#"git clone https://github.com/asoldo/prime-lang.git
cd prime-lang
cargo install --path .
# or, for iterative dev:
cargo build"#,
                explanation: "The README's Quick Start path builds the CLI from a fresh checkout. `cargo install --path .` puts `prime-lang` on your $PATH; `cargo build` keeps artifacts in `target/` for iterative work.",
            },
            TopicSection {
                title: "Run and build the sample",
                snippet: r#"prime-lang run main.prime         # interpret
prime-lang build main.prime       # emit LLVM/native to ./.build.prime/output
./.build.prime/output/output      # run the native binary"#,
                explanation: "`main.prime` ships with the repo so you can verify interpretation and LLVM build-mode immediately. Build-mode mirrors interpreter semantics for control flow and `try { }` / `?`, and the native binary now exits with code 0 for CI scripts.",
            },
            TopicSection {
                title: "List and query docs",
                snippet: r#"prime-lang docs
prime-lang docs --list
prime-lang docs --query for,match"#,
                explanation: "`prime-lang docs` renders all topics, `--list` shows topic keys and aliases, and `--query` filters to specific topics. Unknown queries exit with an error plus a hint, matching the README guidance.",
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
                snippet: r#"$ prime-lang run main.prime
$ prime-lang build main.prime --name demo
$ prime-lang lint main.prime --watch
$ prime-lang fmt main.prime --write
$ prime-lang lsp
$ prime-lang init adventure
$ prime-lang add demos.new --path demos/new_demo.prime
$ prime-lang docs --query core-syntax"#,
                explanation: "Each subcommand is wired up in `src/main.rs` via clap. `run` interprets a file after loading the manifest, `build` emits LLVM + native binaries, `lint`/`fmt` offer single-shot or watch modes, `lsp` serves editors, `init` scaffolds a workspace, `add` appends a manifest entry and stub file, and `docs` prints these references.",
            },
            TopicSection {
                title: "Example programs",
                snippet: r#"prime-lang run pattern_demo.prime        # module demos::patterns
prime-lang run error_handling_demo.prime # demos::error_handling
prime-lang run lab_demo.prime            # demos::lab_demo
prime-lang run ns_demo.prime             # demos::ns"#,
                explanation: "Demos ship in the root manifest so you can exercise patterns, error handling, namespaces, interfaces, and borrowing. Library-only modules like `types.prime` and `pkg_lib.prime` omit `main` intentionally and are meant to be imported.",
            },
            TopicSection {
                title: "prime.toml manifest",
                snippet: r#"manifest_version = "2"

[package]
entry = "app::main"
kind = "binary"
name = "prime-lang-playground"
version = "0.1.0"

[[modules]]
name = "app::main"
path = "main.prime"
visibility = "pub""#,
                explanation: "`project::manifest::PackageManifest` reads `prime.toml`, mapping module names to canonical file paths. Entries must match each file's `module ...;` header so `load_package` can pull the correct dependencies before typechecking or interpretation.",
            },
            TopicSection {
                title: "Module & library discovery",
                snippet: r#"$ prime-lang add demos::patterns --path pattern_demo.prime --visibility pub
$ prime-lang add core::types --path types.prime --library
# stubs `module demos::patterns;` or `library core::types;` and records it in prime.toml
"#,
                explanation: "`prime-lang add` edits the manifest and writes a stub file, keeping the graph in sync. Modules are runnable (with `main`), libraries are import-only (no `main`), and tests use `test ...;` headers. The manifest `kind` reflects the header so the loader/LSP apply the right rules.",
            },
            TopicSection {
                title: "Interpreter concurrency (beta)",
                snippet: r#"let (tx, rx) = channel[int32]();
let handle = spawn (send(tx, 1));
let Option[int32] first = recv(rx);
join(handle);"#,
                explanation: "`spawn` returns `JoinHandle[T]`; `channel[T]()` yields `(Sender[T], Receiver[T])`. `send` returns `Result[(), string]` and `recv` yields `Option[T]`. In run mode, tasks execute on real threads and `recv` blocks until a value arrives or the channel closes; build mode mirrors the same blocking semantics while evaluating deterministically during compilation.",
            },
            TopicSection {
                title: "Checking examples & regression safety",
                snippet: r#"./scripts/check_examples.sh
PRIME_RUN_EXAMPLES=1 ./scripts/check_examples.sh"#,
                explanation: "The helper script lints every module in `prime.toml` and optionally runs them, mirroring the README's regression advice. Use it after syntax or parser changes to keep `prime-lang docs` snippets and demos compiling.",
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
                snippet: r#"- main.prime – modules, structs/enums, interfaces, ownership, UI-ish printing
- pattern_demo.prime – matches/destructuring over tuples, maps, structs, slices
- error_handling_demo.prime – Result, try { }, and ? propagation
- lab_demo.prime – range loops, map destructuring, mutable refs, generic interface
- pointer_demo.prime – raw pointers derived from references plus stored ranges"#,
                explanation: "Each entry is runnable via `prime-lang run <file>` and mapped in the manifest under `demos::...`. They demonstrate the syntax shown throughout the README so `prime-lang docs` can quote real code.",
            },
            TopicSection {
                title: "Libraries and namespace demos",
                snippet: r#"- types.prime / pkg_lib.prime – library-only modules (no main)
- ns_demo.prime – namespace overloading with foo/bar labels
- pkg_app.prime – banner/promotion output paired with pkg_lib"#,
                explanation: "Library modules intentionally lack `main`, so running them directly reports `Unknown symbol main`; import them from other modules. Namespace demos show how module paths and declarations line up.",
            },
            TopicSection {
                title: "Validated outputs snapshot",
                snippet: r#"- borrow_demo.prime – aliasing/borrowing prints loop merges and HP/MP stats
- heap_demo.prime / heap_features.prime – box counters, roster prints, redeploys
- interface_demo.prime / interface_generics_demo.prime – method calls and announcements"#,
                explanation: "The README lists expected outputs so you can smoke-test changes. Use them as fixtures when expanding the language—`prime-lang docs` draws from the same files for examples.",
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
                explanation: "Channels and joins now support timeouts via `recv_timeout(rx, millis)`; `sleep(ms)` pauses the current task. Build and run modes mirror semantics so determinism holds during compilation, while runtime uses host threads and timers.",
            },
            TopicSection {
                title: "ABI & FFI status",
                snippet: r#"- Targets: host triple by default; pointer width matches host
- Scalars: int/uint widths lower than 128 bits emit native integer types
- ABI shims: runtime_abi.rs exposes stable C-callable entry points for runtime primitives
- Limits: no non-native pointer sizes; 128-bit is the widest scalar; FFI considered experimental pre-1.0"#,
                explanation: "Codegen relies on LLVM’s host triple today and assumes the host pointer width. Runtime ABI functions (`prime_*` shims in runtime_abi.rs) back channels, slices, maps, and boxed values. External FFI and cross-target builds remain experimental until locked for 1.0; avoid relying on undocumented calling conventions.",
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
                explanation: "`out(expr)` prints strings, numbers, tuples, and structs using their debug representations. Tuple returns destructure cleanly at call sites, matching the README's basics walkthrough.",
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
                explanation: "Type parameters appear inside square brackets and can be inferred or specified explicitly. The compiler monomorphizes these functions so they work uniformly for structs, scalars, or user-defined types.",
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
                explanation: "`&T` shares read-only access, `&mut T` grants unique mutation, and explicit `*` deref copies values out of heap-backed cells. `ptr` / `ptr_mut` produce raw pointers from existing references when you need to sidestep borrow checks, and `defer` schedules cleanup at scope exit—mirroring the README's maintenance example.",
            },
            TopicSection {
                title: "Raw pointers from references",
                snippet: r#"fn boost_hp(target: &mut Player, delta: int32) {
  let ptr = ptr_mut(&mut target.hp);
  *ptr = *ptr + delta;
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
                explanation: "`Result[T, E]` plus the `?` operator short-circuits on failure, and `try { ... }` blocks wrap multi-expression workflows. Import `core::types` to bring the `Result`/`Option` enums (and variants like `Ok`, `Err`, `Some`, `None`) into scope. Heap primitives such as `Box`, slices (`[]T`), and maps (`Map[K, V]`) provide helper methods like `.box_get()`, `.len()`, and either `.get(key)` or bracket indexing for safe ownership transfers; the same APIs work in build/run modes.",
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
                explanation: "Prime validates return types, argument arity, and match exhaustiveness before running. Mutable borrows must be unique, and diagnostics point to spans so you can fix mistakes without guessing—matching the README's static type-checking overview.",
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
                explanation: "Structs and enums are value types: assignment copies fields. References wrap data in a shared heap cell (`Arc<Mutex<_>>`) so borrows live on the heap until all refs drop. There is no manual alloc/free; heap helpers (Box, slices, maps) cover common cases, echoing the README's memory model notes.",
            },
            TopicSection {
                title: "Concurrency (build/run parity)",
                snippet: r#"fn worker(id: int32, tx: Sender[int32], values: []int32) -> Result[(), string] {
  let mut int32 total = 0;
  for value in values {
    total = total + value;
  }
  send(tx, total + id)
}

fn main() {
  let (tx, rx) = channel[int32]();
  let handle = spawn worker(100, tx, [1, 2, 3, 4]);
  match join(handle) {
    Ok(result) => match result {
      Ok(()) => out(rx.recv()),
      Err(msg) => out(msg),
    },
    Err(msg) => out(msg),
  }
}"#,
                explanation: "Channels (`channel[T]`), `spawn expr`, and `join(handle)` work in both the interpreter and build mode. Build-mode execution stays deterministic unless you opt into `PRIME_BUILD_PARALLEL=1`; emitted binaries always use OS threads. See `parallel_demo.prime` for a two-worker aggregation example.",
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
                explanation: "`prime-lang lsp` speaks LSP over stdio and mirrors rust-analyzer-style capabilities (hover, diagnostics, formatting, symbols). The README's Neovim snippet registers the server, hooks keymaps (gd/gD/rename), and enables on-save formatting when supported.",
            },
            TopicSection {
                title: "Tree-sitter grammar & highlights",
                snippet: r#"local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.prime = {
  install_info = {
    url = "https://github.com/asoldo/prime_lang_treesitter.git",
    files = { "src/parser.c" },
  },
  filetype = "prime",
}
vim.treesitter.language.register("prime", "prime")"#,
                explanation: "Add the `prime_lang_treesitter` parser so syntax highlighting, locals view, and outline panes work even before the LSP attaches. The README shows a Lazy spec that enables highlighting globally and registers the filetype mapping for *.prime files.",
            },
            TopicSection {
                title: "Prime icon plugin",
                snippet: r#"opts.extension.prime = {
  glyph = "",
  hl = "MiniIconsCyan",
}
opts.filetype.prime = opts.extension.prime"#,
                explanation: "An optional Mini Icons extension tags `.prime` files with a dedicated glyph. The README's sample extends `mini.icons` (or AstroNvim defaults) so explorer panes and tabs show a Prime-specific icon.",
            },
            TopicSection {
                title: "Troubleshooting & watch mode",
                snippet: r#"prime-lang lint --file main.prime --watch
prime-lang fmt --write main.prime
tree-sitter init   # if ABI warnings appear"#,
                explanation: "Use lint watch for rapid feedback, format files via the built-in formatter, and re-init the Tree-sitter grammar when ABI warnings surface. These match the README's development tips.",
            },
        ],
    },
    Topic {
        key: "macros",
        title: "Macros & hygiene",
        category: "Language Features",
        summary: "Macro parameters, hygiene escapes with `@`, repeat separators, and hover support that shows full macro declarations.",
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
                explanation: "Repeat params split on the first top-level separator: comma joins into a tuple, semicolon joins into a sequential block. Place `@sep = ,` or `@sep = ;` immediately after the opening `(` (an optional comma after the prefix is accepted).",
            },
            TopicSection {
                title: "LSP hover for macros",
                snippet: r#"macro tally(values: repeat) -> int32 {
  values
}"#,
                explanation: "Hover on macro names to see the full declaration (including body) and parameter kinds. Macros participate in navigation and rename alongside functions.",
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
        ],
    },
    Topic {
        key: "recent-updates",
        title: "Recent Updates & Release Notes",
        category: "Meta",
        summary: "Release callouts from the README so `prime-lang docs` can surface the latest behavior changes (tests, headers, module separators).",
        aliases: &[
            "updates",
            "release",
            "changelog",
            "november-2025",
            "april-2026",
            "recent",
        ],
        sections: &[
            TopicSection {
                title: "Interpreter and build parity (April 2026)",
                snippet: r#"- Native binary now exits with status code 0 after successful build
- Build-mode mirrors interpreter control flow, including return/break/continue
- `try { }` blocks and postfix `?` use identical semantics in run/build"#,
                explanation: "The README highlights April 2026 updates that fixed native exit codes and aligned LLVM output with interpreter control flow. Scripts and CI harnesses can now rely on identical semantics between `prime-lang run` and `prime-lang build`.",
            },
            TopicSection {
                title: "Iterable ranges & threaded spawn (current)",
                snippet: r#"- `Range[T]` bounds stay typed; stored ranges can be iterated later
- `for` accepts anything implementing `iter()` (`Iterable[T]`)
- Interpreter spawn/channel now run on OS threads; `recv` blocks until close
- Build mode mirrors blocking semantics; set `PRIME_BUILD_PARALLEL=1` for threaded build spawn"#,
                explanation: "Ranges now carry their element type, `for` loops consult an `iter()` method when present, and run-mode concurrency uses real threads with blocking channel semantics. Build mode defaults to deterministic evaluation but uses the same blocking rules; exporting `PRIME_BUILD_PARALLEL=1` runs build-mode `spawn` on threads while preserving `send/recv/join` parity with runtime.",
            },
            TopicSection {
                title: "November 2025 release notes placeholder",
                snippet: r#"- README tracks a \"Recent Updates (November 2025)\" section
- Use `git log` or commits near that date for detailed changes"#,
                explanation: "The README reserves space for November 2025 highlights. When you add release notes, keep them concise so `prime-lang docs --query recent` can surface them alongside newer callouts.",
            },
        ],
    },
];
