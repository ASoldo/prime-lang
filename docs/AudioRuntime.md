# Prime Audio Runtime

Prime has a first host-native audio runtime for interpreter-run programs. It
uses the host default output device and keeps playback handles in the runtime so
games can start, stop, and adjust sounds from Prime code.

Use it with `prime-lang run`; build-mode/native binary lowering rejects these
built-ins with a clear runtime-only diagnostic until the compiler has an audio
backend.

## Built-ins

```prime
fn audio_play(path: string, looped: bool) -> Result[int32, string]
fn audio_stop(handle: int32) -> bool
fn audio_stop_all() -> ()
fn audio_set_volume(handle: int32, volume_percent: int32) -> bool
fn audio_is_playing(handle: int32) -> bool
```

`audio_play` returns an integer handle. Keep that handle if you want to stop or
adjust a specific sound. `audio_stop_all()` is useful for shutdown paths.

## Silent Checks

The runtime can run without an audio device:

```sh
PRIME_AUDIO_SILENT=1 PRIME_GFX_HEADLESS=1 PRIME_GFX_MAX_FRAMES=3 \
  cargo run -- run workspace/demos/tetris/tetris.prime
```

The Tetris demo loops `workspace/demos/tetris/assets/arcade-music-loop.wav` while
the game window is open.
