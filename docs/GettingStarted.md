# Getting Started

## Prerequisites

- Install Rust with rustup. The repository selects Rust 1.100.0 for development. The locked host CLI supports Rust 1.91 and newer.
- LLVM 21 available for `llvm-sys` (build-mode uses `llc`). On Linux you can set `LLVM_SYS_211_PREFIX` and/or `PRIME_LLC` if LLVM is not in the default path.

On Linux the host runtime also needs a C linker, pkg-config, ALSA and X11/Wayland development headers. The [CI workflow](../.github/workflows/ci.yml) lists the Ubuntu packages. The example inventory requires Python 3.11 or newer.

## Install the CLI

```bash
cargo install --locked --path .
prime-lang --help
```

## Run a demo

```bash
prime-lang check workspace/demos/tetris/tetris.prime --format json
prime-lang run workspace/demos/closures/closure_demo.prime
prime-lang run workspace/demos/macro/macro_demo.prime
prime-lang run workspace/demos/channel_demo/channel_demo.prime
prime-lang run workspace/demos/tetris/tetris.prime
```

## Build a native binary (host)

```bash
prime-lang build workspace/demos/channel_demo/channel_demo.prime --name channel_demo
./.build.prime/channel_demo/channel_demo
```

## Editor (LSP + tree-sitter)

- LSP server entrypoint: `prime-lang lsp` (stdio).
- Tree-sitter grammar: `tree-sitter/prime/grammar.js`.
- AstroNvim example config lives at `lazy/astronvim/plugins/prime-lang.lua`.

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

## CI-equivalent checks

```bash
./scripts/ci.sh
```
