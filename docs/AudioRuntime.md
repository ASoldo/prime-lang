# Prime Audio Runtime

The host interpreter and compiled binaries use the host audio runtime to play, stop and adjust sounds. Interactive playback needs an audio output device and readable media files. Run the Tetris commands below from the checkout root so its media paths resolve.

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
  cargo run --locked -- run workspace/demos/tetris/tetris.prime
```

The Tetris demo loops `workspace/demos/tetris/assets/arcade-music-loop.wav` while
the game window is open.
