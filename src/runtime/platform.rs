use crate::target::{BuildOptions, embedded_target_hint};
use std::sync::OnceLock;

/// Thin platform abstraction for std-only built-ins (fs/time/etc.). This allows
/// swapping in a HAL later while keeping the call sites stable today.
pub trait Platform: Send + Sync {
    fn now_ms(&self) -> i128;
    fn sleep_ms(&self, millis: i128);
    fn delay_ms(&self, millis: i32);
    fn pin_mode(&self, pin: i32, mode: i32) -> Result<(), String>;
    fn digital_write(&self, pin: i32, level: i32) -> Result<(), String>;
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

    fn delay_ms(&self, millis: i32) {
        if millis > 0 {
            std::thread::sleep(std::time::Duration::from_millis(millis as u64));
        }
    }

    fn pin_mode(&self, _pin: i32, _mode: i32) -> Result<(), String> {
        Err(embedded_disabled_message("pin_mode"))
    }

    fn digital_write(&self, _pin: i32, _level: i32) -> Result<(), String> {
        Err(embedded_disabled_message("digital_write"))
    }

    fn fs_exists(&self, path: &str) -> bool {
        std::path::Path::new(path).exists()
    }

    fn fs_read(&self, path: &str) -> Result<String, String> {
        std::fs::read_to_string(path).map_err(|err| err.to_string())
    }

    fn fs_write(&self, path: &str, contents: &str) -> Result<(), String> {
        std::fs::write(path, contents).map_err(|err| err.to_string())
    }
}

pub struct NoStdPlatform;

impl Platform for NoStdPlatform {
    fn now_ms(&self) -> i128 {
        0
    }

    fn sleep_ms(&self, _millis: i128) {}

    fn delay_ms(&self, _millis: i32) {}

    fn pin_mode(&self, _pin: i32, _mode: i32) -> Result<(), String> {
        Err(embedded_disabled_message("pin_mode"))
    }

    fn digital_write(&self, _pin: i32, _level: i32) -> Result<(), String> {
        Err(embedded_disabled_message("digital_write"))
    }

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
    PLATFORM.get_or_init(default_platform).as_ref()
}

#[allow(dead_code)]
pub fn install_platform<P: Platform + 'static>(platform: P) -> Result<(), Box<dyn Platform>> {
    PLATFORM.set(Box::new(platform))
}

pub fn configure_platform(options: &BuildOptions) {
    let wants_esp32 =
        options.target.is_embedded() || matches!(options.platform.as_deref(), Some("esp32"));
    if wants_esp32 {
        let _ = install_platform(Esp32Platform);
    }
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

pub fn embedded_disabled_message(op: &str) -> String {
    format!(
        "`{}` unavailable: embedded-only built-in (compile with --target {} and platform esp32)",
        op,
        embedded_target_hint()
    )
}

pub struct Esp32Platform;

unsafe extern "C" {
    fn prime_pin_mode(pin: i32, mode: i32) -> i32;
    fn prime_digital_write(pin: i32, level: i32) -> i32;
    fn prime_delay_ms(ms: i32);
    fn prime_now_ms() -> i128;
}

impl Platform for Esp32Platform {
    fn now_ms(&self) -> i128 {
        unsafe { prime_now_ms() as i128 }
    }

    fn sleep_ms(&self, millis: i128) {
        if millis > 0 {
            unsafe {
                prime_delay_ms(millis as i32);
            }
        }
    }

    fn delay_ms(&self, millis: i32) {
        if millis > 0 {
            unsafe {
                prime_delay_ms(millis);
            }
        }
    }

    fn pin_mode(&self, pin: i32, mode: i32) -> Result<(), String> {
        let status = unsafe { prime_pin_mode(pin, mode) };
        if status == 0 {
            Ok(())
        } else {
            Err(format!("pin_mode failed with status {}", status))
        }
    }

    fn digital_write(&self, pin: i32, level: i32) -> Result<(), String> {
        let status = unsafe { prime_digital_write(pin, level) };
        if status == 0 {
            Ok(())
        } else {
            Err(format!("digital_write failed with status {}", status))
        }
    }

    fn fs_exists(&self, _path: &str) -> bool {
        false
    }

    fn fs_read(&self, _path: &str) -> Result<String, String> {
        Err(embedded_disabled_message("fs_read"))
    }

    fn fs_write(&self, _path: &str, _contents: &str) -> Result<(), String> {
        Err(embedded_disabled_message("fs_write"))
    }
}
