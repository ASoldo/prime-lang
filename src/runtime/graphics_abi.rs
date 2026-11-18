#![allow(clippy::missing_safety_doc)]
use super::base::read_host_str;

#[allow(clippy::duplicate_mod)]
#[path = "graphics.rs"]
mod graphics_runtime;

#[unsafe(export_name = "prime_gfx_open")]
pub unsafe extern "C" fn prime_gfx_open(
    title_ptr: *const u8,
    title_len: usize,
    width: i32,
    height: i32,
) -> bool {
    let Some(title) = (unsafe { read_host_str(title_ptr, title_len) }) else {
        return false;
    };
    graphics_runtime::open(title, width, height).is_ok()
}

#[unsafe(export_name = "prime_gfx_clear")]
pub unsafe extern "C" fn prime_gfx_clear(r: i32, g: i32, b: i32) {
    let _ = graphics_runtime::clear(r, g, b);
}

#[unsafe(export_name = "prime_gfx_rect")]
pub unsafe extern "C" fn prime_gfx_rect(
    x: i32,
    y: i32,
    width: i32,
    height: i32,
    r: i32,
    g: i32,
    b: i32,
) {
    let _ = graphics_runtime::rect(x, y, width, height, r, g, b);
}

#[unsafe(export_name = "prime_gfx_sprite")]
pub unsafe extern "C" fn prime_gfx_sprite(
    path_ptr: *const u8,
    path_len: usize,
    x: i32,
    y: i32,
    width: i32,
    height: i32,
    r: i32,
    g: i32,
    b: i32,
) -> bool {
    let Some(path) = (unsafe { read_host_str(path_ptr, path_len) }) else {
        return false;
    };
    graphics_runtime::sprite(
        path,
        graphics_runtime::RectSpec {
            x,
            y,
            width,
            height,
        },
        graphics_runtime::ColorSpec { r, g, b },
    )
    .is_ok()
}

#[unsafe(export_name = "prime_gfx_text")]
pub unsafe extern "C" fn prime_gfx_text(
    text_ptr: *const u8,
    text_len: usize,
    x: i32,
    y: i32,
    scale: i32,
    r: i32,
    g: i32,
    b: i32,
) {
    if let Some(text) = unsafe { read_host_str(text_ptr, text_len) } {
        let _ = graphics_runtime::text(text, x, y, scale, r, g, b);
    }
}

#[unsafe(export_name = "prime_gfx_text_int")]
pub unsafe extern "C" fn prime_gfx_text_int(
    label_ptr: *const u8,
    label_len: usize,
    value: i32,
    x: i32,
    y: i32,
    scale: i32,
    r: i32,
    g: i32,
    b: i32,
) {
    if let Some(label) = unsafe { read_host_str(label_ptr, label_len) } {
        let text = format!("{label} {value}");
        let _ = graphics_runtime::text(&text, x, y, scale, r, g, b);
    }
}

#[unsafe(export_name = "prime_gfx_present")]
pub unsafe extern "C" fn prime_gfx_present() -> bool {
    graphics_runtime::present().unwrap_or(false)
}

#[unsafe(export_name = "prime_gfx_key_down")]
pub unsafe extern "C" fn prime_gfx_key_down(key_ptr: *const u8, key_len: usize) -> bool {
    let Some(key) = (unsafe { read_host_str(key_ptr, key_len) }) else {
        return false;
    };
    graphics_runtime::key_down(key).unwrap_or(false)
}

#[unsafe(export_name = "prime_gfx_key_pressed")]
pub unsafe extern "C" fn prime_gfx_key_pressed(key_ptr: *const u8, key_len: usize) -> bool {
    let Some(key) = (unsafe { read_host_str(key_ptr, key_len) }) else {
        return false;
    };
    graphics_runtime::key_pressed(key).unwrap_or(false)
}

#[unsafe(export_name = "prime_gfx_should_close")]
pub unsafe extern "C" fn prime_gfx_should_close() -> bool {
    graphics_runtime::should_close()
}

#[unsafe(export_name = "prime_gfx_close")]
pub unsafe extern "C" fn prime_gfx_close() {
    graphics_runtime::close();
}
