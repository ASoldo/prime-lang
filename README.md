<img width="948" height="515" alt="image" src="https://github.com/user-attachments/assets/303940a2-5537-41f1-a3ab-e46b67500921" />

# Prime Language

Prime is a learning-oriented language and toolchain for experimenting with
parsing, interpretation, LLVM code generation, and Neovim integrations. This
repository contains the compiler/interpreter (`prime-lang`), the LSP server, and
the CLI utilities you use while editing `.prime` files.

---

## Quick Start

```bash
git clone https://github.com/asoldo/prime-lang.git
cd prime-lang
cargo install --path .
prime-lang run main.prime         # interpret
prime-lang build main.prime       # emit LLVM, link to ./.build.prime/output
./.build.prime/output/output      # run the native binary
```

The repository ships with `main.prime` so you can confirm the full toolchain
works end‑to‑end immediately after building.

## Language Basics

Prime keeps the syntax close to systems languages you already know. Key features
in `main.prime` and `types.prime` demonstrate the core semantics:

### Modules & Imports

```prime
import "types";               // load structs/enums from types.prime
import "math/random";         // relative paths become math/random.prime
```

Modules compile to the same package; there is no module system magic. Each file
exposes the structs, enums, consts, and functions it defines.

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
  Percent(rate) => { /* block */ },
};

if hero.hp < 50 {
  out("status: fragile");
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
```

- `match` is exhaustive and destructures enum payloads or patterns like
  `let Vec2 next = add_vec2(...)`.
- Loops include `for range`, `while`, and `match` drives branching.

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
- `defer` schedules work to run when the current scope exits—ideal for cleanup.

### Built-in I/O & APIs

- `out(expr)` prints values (strings, numbers, tuples, structs implement a debug
  representation).
- `match Option[...]` and `Result[...]` mirror Rust semantics for optional and
  fallible flows.

```prime
import "math";

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

Functions whose first parameter is a struct act like methods. You can call them
via `structInstance.method(args...)` and the compiler rewrites it to a regular
function call behind the scenes:

```prime
fn heal(mut player: Player, boost: int32) -> Player {
  player.hp = player.hp + boost;
  player
}

let Player leveled = hero.heal(24);
```

- Methods can take ownership, `&Player`, or `&mut Player` depending on how you
  declare the first parameter.
- There is no trait/impl syntax yet; everything lives as free functions.

### Types & Memory Model

- Numbers: `int8`…`int64`, `uint8`…`uint64`, `isize`/`usize`, `float32`,
  `float64`, plus `bool`, `rune`, and `string`.
- Structs/enums are value types; assignment copies the fields.
- References (`&T`) wrap the value in an `Rc<RefCell<_>>`, so borrowing a value
  moves it to the heap until all references drop. Raw pointers (`*T`) exist for
  unsafe interop, but there is no manual `alloc/free`.
- Dynamic containers (growable slices, maps) are not implemented yet. Programs
  stick to fixed-size structs and module-defined helpers—the next milestone is
  a richer standard library plus explicit ownership rules.

With these primitives you can already compose larger programs (see
`main.prime`/`types.prime`) while keeping the roadmap—better containers and
memory management—in mind.


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
    if client.server_capabilities.documentFormattingProvider then
      vim.api.nvim_create_autocmd("BufWritePre", {
        buffer = bufnr,
        callback = function() vim.lsp.buf.format { bufnr = bufnr } end,
      })
    end
  end,
}
```

### Treesitter Highlighting & Symbols

Use the `prime_lang_treesitter` grammar repo plus a small Lazy spec to wire up
syntax highlighting, locals view, and outline panes:

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
          url = "https://github.com/asoldo/prime_lang_treesitter.git",
          files = { "src/parser.c" },
          branch = "main",
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
