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
prime-lang run main.prime         # interpret
prime-lang build main.prime       # emit LLVM, link to ./.build.prime/output
./.build.prime/output/output      # run the native binary
# Try another module: prime-lang run --entry demos::error_handling
```

The repository ships with `main.prime` so you can confirm the full toolchain
works end‑to‑end immediately after building. As of the April 2026 compiler
updates the emitted native binary now exits with status code `0`, so scripting
and CI integrations no longer see spurious failures after a successful build.
Build-mode now mirrors the interpreter for control flow (`return`/`break`/`continue`)
and for the new `try {}` / `?` sugar, so you can rely on identical semantics in both
`prime run` and `prime build`.

### Example Programs

- `main.prime` – Feature tour: modules, structs/enums, interfaces, ownership, and UI-ish printing.
- `pattern_demo.prime` – Match/if/while/for destructuring across tuples, maps, structs, slices.
- `error_handling_demo.prime` – `Result`, `try {}`, and `?` propagation.
- `lab_demo.prime` – A syntax-rich lab scenario (range loops, map destructuring, mutable refs, generic interface) used as an internal example; not exposed via `prime-lang docs`.
- `pointer_demo.prime` – Raw pointers derived from references plus first-class ranges.
- Library modules: `types.prime` and `pkg_lib.prime` are libraries only (no `main`). Running them directly reports `Unknown symbol main`; import them from other modules instead.

Run any of them directly:

```bash
prime-lang run lab_demo.prime            # module demos::lab_demo in the manifest
prime-lang run pattern_demo.prime        # module demos::patterns
prime-lang run error_handling_demo.prime # module demos::error_handling
```

Validated outputs (fresh run, current syntax):

- `borrow_demo.prime` — aliasing/borrowing demos; prints loop merges and HP/MP stats.
- `error_handling_demo.prime` — shows Ok/Err propagation, span calculations.
- `generic_demo.prime` — identity/promote helpers with typed outputs.
- `heap_demo.prime` / `heap_features.prime` — box counters, roster prints, redeploy outputs.
- `interface_demo.prime` / `interface_generics_demo.prime` — method calls and announcements.
- `lab_demo.prime` — three plan runs with totals/synergy summaries and pairing.
- `main.prime` — full gameplay log (players, enemies, quests, patrols, defers).
- `ns_demo.prime` — namespace overloading demo with foo/bar labels.
- `pattern_demo.prime` — pattern matches over tuples/maps/slices plus mutation demos.
- `pkg_app.prime` — banner/promotion output.
- `pointer_demo.prime` — pointer-based HP tweaks and stored ranges.

## CLI Overview & Built-in Docs

Every command is defined in `src/main.rs` with clap, so `prime-lang --help` stays
fresh. Highlights:

| Command | Purpose |
| --- | --- |
| `prime-lang run <file>` | Interpret a `.prime` entry point after loading its manifest dependencies (modules only; libraries are rejected) |
| `prime-lang build <file> --name demo` | Compile to LLVM IR/object code and write a runnable binary below `./.build.prime/demo` (modules only) |
| `prime-lang lint <file> [--watch]` | Single-shot or watch-mode linting with the same parser used by the LSP |
| `prime-lang fmt <file> [--write]` | Preview or apply the formatter |
| `prime-lang lsp` | Start the language server over stdio (Neovim/VS Code use this entry point) |
| `prime-lang init [path]` | Scaffold a fresh workspace with `prime.toml` |
| `prime-lang add <module> [--path file.prime] [--test|--library]` | Append a module, test, or library entry to the manifest and stub the file with the correct header |
| `prime-lang test [target,...]` | Run test modules (header `test ...;`) by name or file; discovers tests when no target is provided |
| `prime-lang docs [--list|--query ...]` | Print the curated reference topics described below |
| Concurrency | `spawn expr` → `JoinHandle[T]`; `channel[T]()` → `(Sender[T], Receiver[T])`; `send/recv/close/join` built-ins (run + build; synchronous scheduling) |

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

### `prime.toml` manifest

`prime-lang` workspaces load `prime.toml` through
`project::manifest::PackageManifest`. The manifest tells the loader which module
names map to which files and what the entry module is:

```toml
manifest_version = "2"

[package]
entry = "app::main"
kind = "binary"
name = "prime-lang-playground"
version = "0.1.0"

[[modules]]
name = "app::main"
path = "main.prime"
visibility = "pub"
kind = "module"

[[modules]]
name = "core::types"
path = "types.prime"
visibility = "pub"
kind = "library"
```

Module names can use either `::` or `.` separators; `core.types` and `core::types`
resolve to the same module internally. Tests can be listed with `[[tests]]` blocks
if you want manifest metadata; otherwise any file that starts with `test ...;` is
discovered by `prime-lang test`.

### Tests

- Write test files with a `test my.module;` header. They can import other modules
  and define any number of functions; no `main` is required.
- `prime-lang test` runs every zero-arg function in the test module (bool return
  is treated as pass/fail). The full return list is passed to the next function
  when arity matches (supports multi-value chaining) or to a single-arg function
  when there is exactly one return; otherwise the function is skipped with a note.
- Targets can be files or module names (`prime-lang test tests::basic,other.test`);
  no target runs all discovered test headers.
- `prime-lang run/build` refuse to execute/compile `test ...;` targets.
- Built-in helpers for tests: `assert(cond: bool)` and `expect(cond: bool, message: string)`.

`prime-lang add demos::patterns --path pattern_demo.prime` will append another
`[[modules]]` table and create the stub file automatically. Keeping the manifest
in sync with each file’s `module ...;` declaration lets the CLI, interpreter,
compiler, and LSP share the same package graph.

## Language Basics

Prime keeps the syntax close to systems languages you already know. Key features
in `main.prime` and `types.prime` demonstrate the core semantics:

### Modules, Libraries & Imports

```prime
module app::main;
library core::types;

import core::types;           // load structs/enums from types.prime
import math::random;          // relative paths become math/random.prime
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
- `channel[T]()` returns `(Sender[T], Receiver[T])`; `send`/`recv`/`close` act on endpoints. `recv` yields `Option[T]` (None if closed).
- Build mode supports these built-ins with the same synchronous semantics (no real parallelism yet).

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
  MissionEvent::Waiting => { /* zero-field variant */ },
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

- `try { ... }` wraps the final expression in `Result::Ok` and propagates any
  `FlowSignal::Propagate` out of the block automatically—perfect for composing
  fallible helpers.
- The postfix `?` operator unwraps `Result::Ok` (returning the payload) or
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
  `float64`, plus `bool`, `rune`, and `string`.
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
- structured completions (locals, structs, enums, constants, keywords)
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

### Recent Updates (November 2025)
