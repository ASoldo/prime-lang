use std::sync::OnceLock;

/// Thin platform abstraction for std-only built-ins (fs/time/etc.). This allows
/// swapping in a HAL later while keeping the call sites stable today.
pub trait Platform: Send + Sync {
    fn now_ms(&self) -> i128;
    fn sleep_ms(&self, millis: i128);
    fn fs_exists(&self, path: &str) -> bool;
    fn fs_read(&self, path: &str) -> Result<String, String>;
    fn fs_write(&self, path: &str, contents: &str) -> Result<(), String>;
}

pub struct StdPlatform;

impl Platform for StdPlatform {
    fn now_ms(&self) -> i128 {
        let now = std::time::SystemTime::now();
        now.duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_millis() as i128)
            .unwrap_or(0)
    }

    fn sleep_ms(&self, millis: i128) {
        if millis > 0 {
            std::thread::sleep(std::time::Duration::from_millis(millis as u64));
        }
    }

    fn fs_exists(&self, path: &str) -> bool {
        std::path::Path::new(path).exists()
    }

    fn fs_read(&self, path: &str) -> Result<String, String> {
        std::fs::read_to_string(path).map_err(|err| err.to_string())
    }

    fn fs_write(&self, path: &str, contents: &str) -> Result<(), String> {
        std::fs::write(path, contents)
            .map_err(|err| err.to_string())
    }
}

static PLATFORM: OnceLock<Box<dyn Platform>> = OnceLock::new();

pub fn platform() -> &'static dyn Platform {
    PLATFORM
        .get_or_init(|| Box::new(StdPlatform) as Box<dyn Platform>)
        .as_ref()
}

#[cfg(not(feature = "std-builtins"))]
const _: () = {
    // Fast failure for no-std profiles until a HAL-backed platform is wired up.
    #[allow(dead_code)]
    fn missing_feature() {
        compile_error!("std-builtins feature disabled: fs/time/channel/spawn built-ins unavailable");
    }
};
