# Prime Language

Prime is an expression-oriented language with a tree-walking interpreter, an LLVM compiler and a language server. The same CLI checks, runs and builds a program.

## First run

From a source checkout with Rust, LLVM and the native development libraries installed, run the channel demo. It sends three integers, closes the sender and prints their sum, `6`. [Host prerequisites](docs/GettingStarted.md#prerequisites)

```sh
cargo install --locked --path .
prime-lang run workspace/demos/channel_demo/channel_demo.prime
```

To see the same program compiled, build it and execute the result from the repository root.

```sh
prime-lang build workspace/demos/channel_demo/channel_demo.prime --name channel_demo
./.build.prime/channel_demo/channel_demo
```

Create and edit your own first program in [Getting Started](docs/GettingStarted.md).

## Explore the language

| To try | Start here |
| --- | --- |
| Modules, ownership and pattern matching | [Language outline](docs/LanguageOutline.md) and [pattern demo](workspace/demos/patterns/pattern_demo.prime) |
| Error handling with `Result`, `try` and `?` | [Error handling demo](workspace/demos/error_handling/error_handling_demo.prime) |
| Tasks, channels, timeouts and cancellation | [Async semantics and target limits](docs/AsyncAwait.md) |
| Text and byte file I/O at execution time | [Host filesystem](docs/LanguageOutline.md#host-filesystem) and [file demo](workspace/demos/fs_demo/fs_demo.prime) |
| A web server you can query with curl | [Run the loopback HTTP server](docs/LoopbackWebServer.md) |
| Graphics and audio | [Tetris](workspace/demos/tetris/tetris.prime), [graphics](docs/GraphicsRuntime.md) and [audio](docs/AudioRuntime.md) |
| Type diagnostics, JSON output and frozen dependencies | [Checking a program](docs/Checking.md) |
| Editor setup, workspace docs and CI | [Development workflow](docs/Development.md) |
| How a checked program executes | [Architecture](docs/Architecture.md), [compiler flow](docs/CompilerFlow.md) and [runtime ABI](docs/RuntimeFFI.md) |

Use `prime-lang --help` for command syntax, `prime-lang docs --list` for built-in topics or `prime-lang docs --query match` for control-flow examples.

Run Tetris from the checkout root so its relative PNG and WAV paths resolve. Interactive play uses a display, keyboard and audio output. The CI smoke runs both backends for three headless frames with silent audio. It exercises execution and exit. Visual quality and audible playback need an interactive run.

## Working with a checkout

The workspace manifest lists packages. Each member's `prime.toml` maps module names to source paths. Use a `module` with `fn main()` as an entry point, a `library` to export items, and a `test` module for Prime tests. For example, the channel demo imports `core::types`, whose prelude exports `Option` and `Result`. [Member manifest](workspace/demos/channel_demo/prime.toml) · [Core library](workspace/core/types.prime)

From the checkout root:

```sh
prime-lang check workspace/core/types.prime --format json
prime-lang test workspace/tests/basic/basic.prime
prime-lang expand workspace/tests/macros/macros.prime --print-expanded
prime-lang docs --generate --out docs.html
```

To check the host filesystem example, build it from the checkout root with `prime-lang build workspace/demos/fs_demo/fs_demo.prime --name fs_demo`. Running `./.build.prime/fs_demo/fs_demo` writes and reads `workspace/demos/fs_demo/tmp_demo.txt` relative to the current directory.

### Embedded / ESP32 (Xtensa) Quickstart

The [blink manifest](workspace/demos/esp32_blink/prime.toml) targets classic Xtensa ESP32. Install the Espressif Rust toolchain (`esp`), esp-clang, the Xtensa GCC tools and ESP-IDF. Build ESP-IDF's blink example once for `esp32` to generate `sections.ld` and `memory.ld`. With the tools installed and the IDF example built, point the Prime CLI at that checkout and its generated linker scripts. Adjust the two paths for your installation.

```sh
export IDF_PATH="$HOME/esp/esp-idf"
export IDF_BUILD_ROOT="$IDF_PATH/examples/get-started/blink/build/esp-idf"
prime-lang build workspace/demos/esp32_blink/esp32_blink.prime --name esp32_blink --no-flash
```

The manifest reads ROM linker scripts from `IDF_PATH` and generated memory layouts from `IDF_BUILD_ROOT`. `CARGO_TARGET_DIR` selects the runtime cache. The build produces an ELF and a binary image under `.build.prime/`. To flash a board, select its serial port and enable `build.flash.enabled` in a local manifest, then run the build without `--no-flash`.

On hardware, the demo logs to UART at 115200 baud, toggles GPIO2 and reads a momentary switch on GPIO18 connected to ground. Boards with a different LED pin need an adjustment to the demo's `led` constant. The startup code disables watchdogs. The [embedded pipeline](docs/EmbeddedPipeline.md) describes board-specific prerequisites and the [timer tests](docs/EmbeddedTimers.md) record the emulator coverage.

## License

The source and documentation use the [MIT License](LICENSE). Tetris artwork and audio under `workspace/demos/tetris/assets/` have separate licensing status and are excluded from the Cargo source package.
