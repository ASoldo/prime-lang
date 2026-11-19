# Prime Native Tetris Demo

This demo exercises the first host-native Prime graphics/audio runtime. It
opens a native window, draws a textured Tetris board with `assets/block.png`,
loops `assets/arcade-music-loop.wav`, and reads keyboard input through
`gfx_key_down` / `gfx_key_pressed`.

With rootless Podman, run `./scripts/tetris build` once, then `./scripts/tetris solo` from the repository root. The same image starts a browser match with `./scripts/tetris online`. [Podman player guide](../../../docs/Podman.md)

To run the source with a host development toolchain:

```sh
cargo run -- run workspace/demos/tetris/tetris.prime
```

Build it as a native executable:

```sh
cargo run -- build workspace/demos/tetris/tetris.prime --name prime_tetris
./.build.prime/prime_tetris/prime_tetris
```

Controls:

- uConsole Start: start/restart
- uConsole A: rotate clockwise
- uConsole B: rotate counterclockwise
- uConsole X: hard drop
- uConsole Y: hold
- uConsole Select: pause/resume
- Keyboard fallback: Left/Right or A/D move, Down/S soft drop, X rotate, Up/Space drop, C hold, Enter start, P pause, R restart
- Esc/window close: quit

For headless/silent smoke checks:

```sh
PRIME_AUDIO_SILENT=1 PRIME_GFX_HEADLESS=1 PRIME_GFX_MAX_FRAMES=3 \
  cargo run -- run workspace/demos/tetris/tetris.prime
PRIME_AUDIO_SILENT=1 PRIME_GFX_HEADLESS=1 PRIME_GFX_MAX_FRAMES=3 \
  ./.build.prime/prime_tetris/prime_tetris
```
