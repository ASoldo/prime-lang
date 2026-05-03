use rodio::{Decoder, OutputStream, OutputStreamHandle, Sink, Source};
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::File;
use std::io::BufReader;
use std::path::{Path, PathBuf};
use std::sync::{Mutex, MutexGuard};

struct RuntimeSlot(Mutex<Option<AudioRuntime>>);

// Prime native programs call the audio ABI from the generated main thread. The
// mutex keeps access serialized and avoids Rust TLS assumptions in generated
// executables linked against the runtime static library.
unsafe impl Sync for RuntimeSlot {}

static RUNTIME: RuntimeSlot = RuntimeSlot(Mutex::new(None));

enum AudioRuntime {
    Real(RealAudio),
    Silent(SilentAudio),
}

struct RealAudio {
    _stream: OutputStream,
    handle: OutputStreamHandle,
    sinks: HashMap<i32, Sink>,
    next_id: i32,
}

struct SilentAudio {
    handles: HashSet<i32>,
    next_id: i32,
}

pub(crate) fn play(path: &str, looped: bool) -> Result<i32, String> {
    let mut slot = runtime_slot()?;
    if slot.is_none() {
        *slot = Some(new_runtime()?);
    }
    let runtime = slot
        .as_mut()
        .ok_or_else(|| "audio runtime failed to start".to_string())?;
    match runtime {
        AudioRuntime::Real(runtime) => runtime.play(path, looped),
        AudioRuntime::Silent(runtime) => Ok(runtime.play()),
    }
}

pub(crate) fn stop(handle: i32) -> bool {
    let Ok(mut slot) = runtime_slot() else {
        return false;
    };
    match slot.as_mut() {
        Some(AudioRuntime::Real(runtime)) => runtime.stop(handle),
        Some(AudioRuntime::Silent(runtime)) => runtime.stop(handle),
        None => false,
    }
}

pub(crate) fn stop_all() {
    let Ok(mut slot) = runtime_slot() else {
        return;
    };
    if let Some(runtime) = slot.as_mut() {
        match runtime {
            AudioRuntime::Real(runtime) => runtime.stop_all(),
            AudioRuntime::Silent(runtime) => runtime.stop_all(),
        }
    }
}

pub(crate) fn set_volume(handle: i32, volume_percent: i32) -> bool {
    let Ok(mut slot) = runtime_slot() else {
        return false;
    };
    match slot.as_mut() {
        Some(AudioRuntime::Real(runtime)) => runtime.set_volume(handle, volume_percent),
        Some(AudioRuntime::Silent(runtime)) => runtime.handles.contains(&handle),
        None => false,
    }
}

pub(crate) fn is_playing(handle: i32) -> bool {
    let Ok(slot) = runtime_slot() else {
        return false;
    };
    match slot.as_ref() {
        Some(AudioRuntime::Real(runtime)) => runtime.is_playing(handle),
        Some(AudioRuntime::Silent(runtime)) => runtime.handles.contains(&handle),
        None => false,
    }
}

fn runtime_slot() -> Result<MutexGuard<'static, Option<AudioRuntime>>, String> {
    RUNTIME
        .0
        .lock()
        .map_err(|_| "audio runtime lock is poisoned".to_string())
}

fn new_runtime() -> Result<AudioRuntime, String> {
    if silent_mode() {
        return Ok(AudioRuntime::Silent(SilentAudio {
            handles: HashSet::new(),
            next_id: 1,
        }));
    }
    let (stream, handle) =
        OutputStream::try_default().map_err(|err| format!("failed to open audio device: {err}"))?;
    Ok(AudioRuntime::Real(RealAudio {
        _stream: stream,
        handle,
        sinks: HashMap::new(),
        next_id: 1,
    }))
}

fn silent_mode() -> bool {
    env::var("PRIME_AUDIO_SILENT").is_ok_and(|value| value == "1" || value == "true")
}

impl RealAudio {
    fn play(&mut self, path: &str, looped: bool) -> Result<i32, String> {
        self.cleanup_finished();
        let resolved_path = resolve_asset_path(path);
        let file = File::open(&resolved_path)
            .map_err(|err| format!("failed to open audio `{path}`: {err}"))?;
        let decoder = Decoder::new(BufReader::new(file))
            .map_err(|err| format!("failed to decode audio `{path}`: {err}"))?;
        let sink = Sink::try_new(&self.handle)
            .map_err(|err| format!("failed to create audio sink: {err}"))?;
        if looped {
            sink.append(decoder.repeat_infinite());
        } else {
            sink.append(decoder);
        }
        let id = self.alloc_id();
        self.sinks.insert(id, sink);
        Ok(id)
    }

    fn stop(&mut self, handle: i32) -> bool {
        match self.sinks.remove(&handle) {
            Some(sink) => {
                sink.stop();
                true
            }
            None => false,
        }
    }

    fn stop_all(&mut self) {
        for (_, sink) in self.sinks.drain() {
            sink.stop();
        }
    }

    fn set_volume(&mut self, handle: i32, volume_percent: i32) -> bool {
        match self.sinks.get(&handle) {
            Some(sink) => {
                let volume = volume_percent.clamp(0, 300) as f32 / 100.0;
                sink.set_volume(volume);
                true
            }
            None => false,
        }
    }

    fn is_playing(&self, handle: i32) -> bool {
        self.sinks.get(&handle).is_some_and(|sink| !sink.empty())
    }

    fn cleanup_finished(&mut self) {
        self.sinks.retain(|_, sink| !sink.empty());
    }

    fn alloc_id(&mut self) -> i32 {
        let id = self.next_id;
        self.next_id += 1;
        if self.next_id <= 0 {
            self.next_id = 1;
        }
        id
    }
}

fn resolve_asset_path(path: &str) -> PathBuf {
    let candidate = Path::new(path);
    if candidate.is_absolute() || candidate.exists() {
        return candidate.to_path_buf();
    }
    if let Ok(exe) = env::current_exe() {
        for base in exe.ancestors().skip(1) {
            let joined = base.join(candidate);
            if joined.exists() {
                return joined;
            }
        }
    }
    candidate.to_path_buf()
}

impl SilentAudio {
    fn play(&mut self) -> i32 {
        let id = self.next_id;
        self.next_id += 1;
        if self.next_id <= 0 {
            self.next_id = 1;
        }
        self.handles.insert(id);
        id
    }

    fn stop(&mut self, handle: i32) -> bool {
        self.handles.remove(&handle)
    }

    fn stop_all(&mut self) {
        self.handles.clear();
    }
}
