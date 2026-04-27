# Prime Native Tetris Demo

This demo exercises the first host-native Prime graphics/audio runtime. It
opens a native window, draws a textured Tetris board with `assets/block.png`,
loops `assets/arcade-music-loop.wav`, and reads keyboard input through
`gfx_key_down` / `gfx_key_pressed`.

Run it from the repository root:

```sh
cargo run -- run workspace/demos/tetris/tetris.prime
```

Controls:

- Left/Right or A/D: move
- Up/X: rotate
- Down/S: soft drop
- Space: hard drop
- P: pause
- R: restart after game over
- Esc/window close: quit

For headless/silent smoke checks:

```sh
PRIME_AUDIO_SILENT=1 PRIME_GFX_HEADLESS=1 PRIME_GFX_MAX_FRAMES=3 \
  cargo run -- run workspace/demos/tetris/tetris.prime
```
