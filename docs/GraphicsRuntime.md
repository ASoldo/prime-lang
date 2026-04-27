# Prime Graphics Runtime

Prime has a first host-native graphics runtime for interpreter-run programs. It
is intentionally small: it provides a native window, a CPU pixel buffer, PNG
sprite drawing, bitmap text, frame presentation, and keyboard polling.

Use it with `prime-lang run`; build-mode/native binary lowering rejects these
built-ins with a clear runtime-only diagnostic until the compiler has a graphics
backend.

## Built-ins

```prime
fn gfx_open(title: string, width: int32, height: int32) -> Result[(), string]
fn gfx_clear(r: int32, g: int32, b: int32) -> ()
fn gfx_rect(x: int32, y: int32, width: int32, height: int32, r: int32, g: int32, b: int32) -> ()
fn gfx_sprite(path: string, x: int32, y: int32, width: int32, height: int32, r: int32, g: int32, b: int32) -> Result[(), string]
fn gfx_text(text: string, x: int32, y: int32, scale: int32, r: int32, g: int32, b: int32) -> ()
fn gfx_present() -> bool
fn gfx_key_down(key: string) -> bool
fn gfx_key_pressed(key: string) -> bool
fn gfx_should_close() -> bool
fn gfx_close() -> ()
```

`gfx_sprite` loads PNG files and caches them by path. The source pixels are
tinted by luminance so a single grayscale block image can become different game
piece colors.

Supported key names include `left`, `right`, `up`, `down`, `space`, `enter`,
`escape`, `z`, `x`, `c`, `p`, `r`, `a`, `d`, `s`, and `w`.

## Headless Checks

The runtime can run without opening a real window:

```sh
PRIME_GFX_HEADLESS=1 PRIME_GFX_MAX_FRAMES=3 \
  cargo run -- run workspace/demos/tetris/tetris.prime
```

`PRIME_GFX_MAX_FRAMES` makes `gfx_present()` return `false` after a fixed number
of frames, which is useful for automated demo smoke tests.

## Demo

The native Tetris demo lives in `workspace/demos/tetris` and uses
`workspace/demos/tetris/assets/block.png` for all pieces.

```sh
cargo run -- run workspace/demos/tetris/tetris.prime
```
