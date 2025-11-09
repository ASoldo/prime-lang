<img width="948" height="515" alt="image" src="https://github.com/user-attachments/assets/303940a2-5537-41f1-a3ab-e46b67500921" />

# Prime Language

Prime is a learning-oriented language and toolchain for experimenting with
parsing, interpretation, LLVM code generation, and Neovim integrations. This
repository contains the compiler/interpreter (`prime-lang`), the LSP server, and
the CLI utilities you use while editing `.prime` files.

---

## Language Snapshot

* Single entry point: `fn main() { ... }`
* Integer declarations: `let int name = <expression>;`
* Output: `out(<expression>);`
* Expressions: identifiers, integers, parentheses, and binary `+ - * /`

```prime
fn main() {
  let int a = 5;
  let int b = 10;
  let int c = 2;
  let int d = 2;
  let int e = 4;
  out(((a + b) - ((c * d) / e)));
}
```


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

Hover tips: the server now returns contextual documentation for every built-in.
Press `K` on `fn`, `main`, `let`, `int`, `out`, literals, or identifiers to see
the relevant description.
Unused variable warnings are emitted as lint diagnostics (yellow) when a binding is declared but never referenced.

## Treesitter Highlighting & Symbols

Use the `prime_lang_treesitter` grammar repo plus a small Lazy spec to wire up
highlighting and aerial symbols:

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

      -- highlights.scm
      do
        local highlights = [[
"fn"   @keyword
"main" @function
"let"  @keyword
"int"  @type
"out"  @function

(let_statement
  (identifier) @variable)

(number)     @number
(identifier) @identifier
]]
        local f = assert(io.open(query_root .. "/highlights.scm", "w"))
        f:write(highlights)
        f:close()
      end

      -- aerial.scm
      do
        local aerial = [[
; fn main() { ... }
((program
    "fn"
    "main" @name
    "(" ")"
    (block) @body) @symbol
  (#set! "kind" "Function"))

; let int a = 5;
((let_statement
    (identifier) @name
    (_)*) @symbol
  (#set! "kind" "Variable"))
]]
        local f = assert(io.open(query_root .. "/aerial.scm", "w"))
        f:write(aerial)
        f:close()
      end

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
