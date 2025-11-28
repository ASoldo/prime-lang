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

pub struct NoStdPlatform;

impl Platform for NoStdPlatform {
    fn now_ms(&self) -> i128 {
        0
    }

    fn sleep_ms(&self, _millis: i128) {}

    fn fs_exists(&self, _path: &str) -> bool {
        false
    }

    fn fs_read(&self, _path: &str) -> Result<String, String> {
        Err(std_disabled_message("fs_read"))
    }

    fn fs_write(&self, _path: &str, _contents: &str) -> Result<(), String> {
        Err(std_disabled_message("fs_write"))
    }
}

fn default_platform() -> Box<dyn Platform> {
    if cfg!(feature = "std-builtins") {
        Box::new(StdPlatform)
    } else {
        Box::new(NoStdPlatform)
    }
}

static PLATFORM: OnceLock<Box<dyn Platform>> = OnceLock::new();

pub fn platform() -> &'static dyn Platform {
    PLATFORM
        .get_or_init(default_platform)
        .as_ref()
}

#[allow(dead_code)]
pub fn install_platform<P: Platform + 'static>(platform: P) -> Result<(), Box<dyn Platform>> {
    PLATFORM.set(Box::new(platform)).map_err(|boxed| boxed)
}

pub fn std_builtins_enabled() -> bool {
    cfg!(feature = "std-builtins")
}

pub fn std_disabled_message(op: &str) -> String {
    format!(
        "`{}` unavailable: std-builtins feature disabled (enable the feature to use fs/time/spawn/channel)",
        op
    )
}
