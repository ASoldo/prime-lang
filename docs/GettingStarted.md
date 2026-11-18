# Getting Started

## Prerequisites

- Install Rust with rustup. The repository selects Rust 1.100.0 for development. The locked host CLI supports Rust 1.91 and newer.
- LLVM 21 available for `llvm-sys` (build-mode uses `llc`). On Linux you can set `LLVM_SYS_211_PREFIX` and/or `PRIME_LLC` if LLVM is not in the default path.

On Linux the host runtime also needs a C linker, pkg-config, ALSA and X11/Wayland development headers. The [CI workflow](../.github/workflows/ci.yml) lists the Ubuntu packages. The example inventory requires Python 3.11 or newer.

## Your first program

From the repository root, install the CLI. With rustup's default layout, the binary goes in `~/.cargo/bin` on your PATH.

```sh
cargo install --locked --path .
```

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

To see which Prime functions the interpreter enters, run `prime-lang run --trace main.prime`. The trace goes to stderr and the score goes to stdout. [Reading a trace](Debugging.md)

## Try a bundled demo

Return to the checkout root for the bundled examples. The channel example in the [README](../README.md#first-run) runs without input or assets. The [web server](LoopbackWebServer.md) reads its port and request count from stdin and serves real requests on loopback.

[Tetris](../workspace/demos/tetris/tetris.prime) reads media paths relative to the checkout root. A short run on a host without a display or sound device is:

```sh
PRIME_AUDIO_SILENT=1 PRIME_GFX_HEADLESS=1 PRIME_GFX_MAX_FRAMES=3 \
  prime-lang run workspace/demos/tetris/tetris.prime
```

ESP32 blink needs a cross toolchain to build and a wired board to observe its GPIO and UART. Use the [embedded build instructions](../README.md#embedded--esp32-xtensa-quickstart) with `--no-flash` to verify firmware packaging.

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

## Reproduce the guide checks

From the checkout root, build the CLI and exercise the guide in a temporary consumer package. The tests run the scaffold, the edited source and the channel commands printed by the built-in quick start. They compare interpreter and executable output with the expected text.

```sh
cargo build --locked --bin prime-lang
python3 scripts/tests/test_getting_started.py -v
```

## CI-equivalent checks

```bash
./scripts/ci.sh
```
