# Getting Started

## Prerequisites

- Install Rust with rustup. The repository selects Rust 1.100.0 for development. The locked host CLI supports Rust 1.91 and newer.
- LLVM 21 available for `llvm-sys` (build-mode uses `llc`). On Linux you can set `LLVM_SYS_211_PREFIX` and/or `PRIME_LLC` if LLVM is not in the default path.

On Linux the host runtime also needs a C linker, pkg-config, ALSA and X11/Wayland development headers. The [CI workflow](../.github/workflows/ci.yml) lists the Ubuntu packages. The example inventory requires Python 3.11 or newer.

## First run

From the repository root, install with `cargo install --locked --path .`. With rustup’s default layout the executable goes in `~/.cargo/bin`. Put that directory on PATH, then try:

```sh
prime-lang <<'PRIME'
out(6 * 7);
PRIME
```

The interpreter prints `42`. In a fresh directory, save the same source and build it:

```sh
cat > answer.prime <<'PRIME'
out(6 * 7);
PRIME
prime-lang build answer.prime --name answer
./.build.prime/answer/answer
```

The compiled executable prints `42`. The [stdin guide](Inline.md) shows how to use the same source with `check -`, `run --trace -` and `build -`.

## Create a package

In a directory of your choice, create a new package named `hello`. The scaffold writes a manifest and an entry module.

```sh
prime-lang new hello --bin
cd hello
prime-lang check main.prime
prime-lang run main.prime
```

The program prints `Hello from Prime!`. Replace the contents of its `main.prime` with this program.

```prime
module hello::main;

fn main() {
  let int32 pieces = 3;
  out(`score = {pieces * 10}`);
}
```

Run it again, then build and execute it. Both executions should print `score = 30`.

```sh
prime-lang run main.prime
prime-lang build main.prime --name score
./.build.prime/score/score
```

To see which Prime functions the interpreter enters, run `prime-lang run --trace main.prime`. The trace goes to stderr and the score goes to stdout. Tools consuming these records can retrieve the installed CLI’s JSON Schemas with `prime-lang schema trace` and `prime-lang schema check`. [Reading a trace](Debugging.md) · [Check reports](Checking.md)

## Try a bundled demo

Return to the checkout root for the bundled examples. Run `prime-lang run workspace/demos/channel_demo/channel_demo.prime` to send three integers and print their sum, `6`. The [web server](LoopbackWebServer.md) reads its port and request count from stdin and serves real requests on loopback.

[Tetris](../workspace/demos/tetris/tetris.prime) reads media paths relative to the checkout root. A short run on a host without a display or sound device is:

```sh
PRIME_AUDIO_SILENT=1 PRIME_GFX_HEADLESS=1 PRIME_GFX_MAX_FRAMES=3 \
  prime-lang run workspace/demos/tetris/tetris.prime
```

ESP32 blink needs a cross toolchain to build and a wired board to observe its GPIO and UART. Use the [embedded build instructions](../README.md#embedded--esp32-xtensa-quickstart) with `--no-flash` to verify firmware packaging.

## Neovim

With `prime-lang` on PATH, add this to your Neovim 0.12 `init.lua`. Open a `.prime` file under a package containing `prime.toml`. Use `:checkhealth vim.lsp` to inspect the client and its root.

```lua
vim.filetype.add({ extension = { prime = "prime" } })
vim.lsp.config("prime_lang", {
  cmd = { "prime-lang", "lsp" },
  filetypes = { "prime" },
  root_markers = { "prime.toml", ".git" },
})
vim.lsp.enable("prime_lang")
```

If you use `mini.icons`, add these two entries to its setup options. They associate the extension and detected filetype with the same glyph. Displaying the glyph requires a terminal font that contains it.

```lua
local prime_icon = { glyph = "", hl = "MiniIconsCyan" }
require("mini.icons").setup({
  extension = { prime = prime_icon },
  filetype = { prime = prime_icon },
})
```

The [AstroNvim config](../lazy/astronvim/plugins/prime-lang.lua) also maps definition, references, rename and macro expansion. Its Tree-sitter setup resolves `tree-sitter/prime` relative to Neovim’s working directory and calls the plugin’s `get_parser_configs()` API. Use a compatible plugin version and open Neovim from the checkout root for that setup. [Editor grammar](../tree-sitter/prime/grammar.js)

## Comments and docs

- `//` line comments (kept by the formatter).
- `///` item docs (attached to the next `fn` / `struct` / `interface` / `macro` / etc).
- `//!` module/file docs (top of the file).

Doc comments show up in hover and are used by the HTML docs generator.

## Generate / serve docs

```bash
prime-lang docs --generate --out docs.html
prime-lang docs --serve --port 7878
```

When serving docs over HTTP, Prime watches your workspace `.prime` files and
auto-reloads the page as docs regenerate on save.

## Reproduce the guide checks

From the checkout root, build the CLI and exercise the guide in a temporary consumer package. The tests execute the heredoc and saved-file examples in both guides, then run the scaffold, the edited source and the channel commands printed by the built-in quick start. They compare interpreter and executable output with the expected text.

```sh
cargo build --locked --bin prime-lang
python3 scripts/tests/test_getting_started.py -v
```

## CI-equivalent checks

```bash
./scripts/ci.sh
```
