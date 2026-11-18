#![allow(clippy::missing_safety_doc)]
use super::base::read_host_str;

#[allow(clippy::duplicate_mod)]
#[path = "audio.rs"]
mod audio_runtime;

#[unsafe(export_name = "prime_audio_play")]
pub unsafe extern "C" fn prime_audio_play(
    path_ptr: *const u8,
    path_len: usize,
    looped: bool,
) -> i32 {
    let Some(path) = (unsafe { read_host_str(path_ptr, path_len) }) else {
        return 0;
    };
    audio_runtime::play(path, looped).unwrap_or(0)
}

#[unsafe(export_name = "prime_audio_stop")]
pub unsafe extern "C" fn prime_audio_stop(handle: i32) -> bool {
    audio_runtime::stop(handle)
}

#[unsafe(export_name = "prime_audio_stop_all")]
pub unsafe extern "C" fn prime_audio_stop_all() {
    audio_runtime::stop_all();
}

#[unsafe(export_name = "prime_audio_set_volume")]
pub unsafe extern "C" fn prime_audio_set_volume(handle: i32, volume_percent: i32) -> bool {
    audio_runtime::set_volume(handle, volume_percent)
}

#[unsafe(export_name = "prime_audio_is_playing")]
pub unsafe extern "C" fn prime_audio_is_playing(handle: i32) -> bool {
    audio_runtime::is_playing(handle)
}
