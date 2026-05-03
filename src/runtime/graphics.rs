use image::ImageReader;
use minifb::{Key, KeyRepeat, ScaleMode, Window, WindowOptions};
use std::collections::HashMap;
use std::env;
use std::path::{Path, PathBuf};
use std::sync::{Mutex, MutexGuard};
#[cfg(target_os = "linux")]
use std::{
    fs::File,
    io::{ErrorKind, Read},
    sync::Arc,
    thread,
};

struct RuntimeSlot(Mutex<Option<GraphicsRuntime>>);

// Prime native programs call the graphics ABI from the generated main thread.
// The mutex serializes accidental re-entry without relying on Rust TLS setup in
// binaries whose entrypoint is generated outside Rust.
unsafe impl Sync for RuntimeSlot {}

static RUNTIME: RuntimeSlot = RuntimeSlot(Mutex::new(None));

#[derive(Clone)]
struct Sprite {
    width: usize,
    height: usize,
    pixels: Vec<[u8; 4]>,
}

enum GraphicsRuntime {
    Window(Box<WindowRuntime>),
    Headless(BufferRuntime),
}

struct WindowRuntime {
    window: Window,
    buffer: BufferRuntime,
}

struct BufferRuntime {
    width: usize,
    height: usize,
    buffer: Vec<u32>,
    sprites: HashMap<String, Sprite>,
    gamepad: GamepadRuntime,
    frames: usize,
    max_frames: Option<usize>,
}

struct GamepadRuntime {
    #[cfg(target_os = "linux")]
    state: Option<Arc<Mutex<GamepadState>>>,
}

#[cfg(target_os = "linux")]
struct GamepadState {
    buttons: Vec<bool>,
    pressed: Vec<bool>,
}

pub(crate) struct RectSpec {
    pub x: i32,
    pub y: i32,
    pub width: i32,
    pub height: i32,
}

pub(crate) struct ColorSpec {
    pub r: i32,
    pub g: i32,
    pub b: i32,
}

impl BufferRuntime {
    fn next_frame_allows_continue(&mut self) -> bool {
        self.frames += 1;
        match self.max_frames {
            Some(max) => self.frames < max,
            None => true,
        }
    }
}

pub(crate) fn open(title: &str, width: i32, height: i32) -> Result<(), String> {
    let width = checked_dimension(width, "width")?;
    let height = checked_dimension(height, "height")?;
    let buffer = BufferRuntime {
        width,
        height,
        buffer: vec![0; width * height],
        sprites: HashMap::new(),
        gamepad: GamepadRuntime::open(),
        frames: 0,
        max_frames: max_frames(),
    };
    let runtime = if should_use_headless() {
        GraphicsRuntime::Headless(buffer)
    } else {
        let mut window = Window::new(
            title,
            width,
            height,
            WindowOptions {
                resize: true,
                scale_mode: ScaleMode::Center,
                ..WindowOptions::default()
            },
        )
        .map_err(|err| format!("failed to open graphics window: {err}"))?;
        window.set_background_color(3, 9, 19);
        GraphicsRuntime::Window(Box::new(WindowRuntime { window, buffer }))
    };
    *runtime_slot()? = Some(runtime);
    Ok(())
}

pub(crate) fn clear(r: i32, g: i32, b: i32) -> Result<(), String> {
    with_buffer(|buffer| {
        let color = color_u32(r, g, b);
        buffer.buffer.fill(color);
        Ok(())
    })
}

pub(crate) fn rect(
    x: i32,
    y: i32,
    width: i32,
    height: i32,
    r: i32,
    g: i32,
    b: i32,
) -> Result<(), String> {
    with_buffer(|buffer| {
        draw_rect(buffer, x, y, width, height, color_u32(r, g, b));
        Ok(())
    })
}

pub(crate) fn sprite(path: &str, area: RectSpec, tint: ColorSpec) -> Result<(), String> {
    with_buffer(|buffer| {
        let sprite = load_sprite(&mut buffer.sprites, path)?;
        draw_sprite(buffer, &sprite, area, tint);
        Ok(())
    })
}

pub(crate) fn text(
    text: &str,
    x: i32,
    y: i32,
    scale: i32,
    r: i32,
    g: i32,
    b: i32,
) -> Result<(), String> {
    with_buffer(|buffer| {
        let scale = scale.max(1);
        draw_text(buffer, text, x, y, scale, color_u32(r, g, b));
        Ok(())
    })
}

pub(crate) fn present() -> Result<bool, String> {
    let mut slot = runtime_slot()?;
    let runtime = slot
        .as_mut()
        .ok_or_else(|| "graphics window is not open".to_string())?;
    match runtime {
        GraphicsRuntime::Window(runtime) => {
            runtime
                .window
                .update_with_buffer(
                    &runtime.buffer.buffer,
                    runtime.buffer.width,
                    runtime.buffer.height,
                )
                .map_err(|err| format!("failed to present graphics frame: {err}"))?;
            Ok(runtime.window.is_open() && runtime.buffer.next_frame_allows_continue())
        }
        GraphicsRuntime::Headless(runtime) => Ok(runtime.next_frame_allows_continue()),
    }
}

pub(crate) fn key_down(name: &str) -> Result<bool, String> {
    if let Some(result) = gamepad_button_down(name) {
        return result;
    }
    with_window(name, |window, key| window.is_key_down(key))
}

pub(crate) fn key_pressed(name: &str) -> Result<bool, String> {
    if let Some(result) = gamepad_button_pressed(name) {
        return result;
    }
    with_window(name, |window, key| {
        window.is_key_pressed(key, KeyRepeat::No)
    })
}

pub(crate) fn should_close() -> bool {
    let Ok(slot) = runtime_slot() else {
        return true;
    };
    match slot.as_ref() {
        Some(GraphicsRuntime::Window(runtime)) => !runtime.window.is_open(),
        Some(GraphicsRuntime::Headless(_)) => false,
        None => true,
    }
}

pub(crate) fn close() {
    if let Ok(mut slot) = runtime_slot() {
        *slot = None;
    }
}

fn checked_dimension(value: i32, label: &str) -> Result<usize, String> {
    if value <= 0 {
        return Err(format!("graphics {label} must be greater than zero"));
    }
    Ok(value as usize)
}

fn should_use_headless() -> bool {
    if env::var("PRIME_GFX_HEADLESS").is_ok_and(|value| value == "1" || value == "true") {
        return true;
    }
    #[cfg(target_os = "linux")]
    {
        env::var_os("DISPLAY").is_none() && env::var_os("WAYLAND_DISPLAY").is_none()
    }
    #[cfg(not(target_os = "linux"))]
    {
        false
    }
}

fn max_frames() -> Option<usize> {
    env::var("PRIME_GFX_MAX_FRAMES")
        .ok()
        .and_then(|value| value.parse::<usize>().ok())
        .filter(|frames| *frames > 0)
}

fn with_buffer<T>(op: impl FnOnce(&mut BufferRuntime) -> Result<T, String>) -> Result<T, String> {
    let mut slot = runtime_slot()?;
    let runtime = slot
        .as_mut()
        .ok_or_else(|| "graphics window is not open".to_string())?;
    match runtime {
        GraphicsRuntime::Window(runtime) => op(&mut runtime.buffer),
        GraphicsRuntime::Headless(runtime) => op(runtime),
    }
}

fn with_window(name: &str, op: impl FnOnce(&Window, Key) -> bool) -> Result<bool, String> {
    let Some(key) = key_from_name(name) else {
        return Err(format!("unknown graphics key `{name}`"));
    };
    let slot = runtime_slot()?;
    match slot.as_ref() {
        Some(GraphicsRuntime::Window(runtime)) => Ok(op(&runtime.window, key)),
        Some(GraphicsRuntime::Headless(_)) => Ok(false),
        None => Err("graphics window is not open".to_string()),
    }
}

fn gamepad_button_down(name: &str) -> Option<Result<bool, String>> {
    let buttons = gamepad_buttons_from_name(name)?;
    Some(with_gamepad(|gamepad| {
        buttons.iter().any(|button| gamepad.button_down(*button))
    }))
}

fn gamepad_button_pressed(name: &str) -> Option<Result<bool, String>> {
    let buttons = gamepad_buttons_from_name(name)?;
    Some(with_gamepad(|gamepad| {
        buttons.iter().any(|button| gamepad.button_pressed(*button))
    }))
}

fn with_gamepad(op: impl FnOnce(&GamepadRuntime) -> bool) -> Result<bool, String> {
    let slot = runtime_slot()?;
    match slot.as_ref() {
        Some(GraphicsRuntime::Window(runtime)) => Ok(op(&runtime.buffer.gamepad)),
        Some(GraphicsRuntime::Headless(runtime)) => Ok(op(&runtime.gamepad)),
        None => Err("graphics window is not open".to_string()),
    }
}

fn gamepad_buttons_from_name(name: &str) -> Option<&'static [usize]> {
    match name.to_ascii_lowercase().as_str() {
        // ClockworkPI uConsole joystick mapping: x=b0, a=b1, b=b2, y=b3.
        "uconsole_x" | "gamepad_x" | "pad_x" => Some(&[0]),
        "uconsole_a" | "gamepad_a" | "pad_a" => Some(&[1]),
        "uconsole_b" | "gamepad_b" | "pad_b" => Some(&[2]),
        "uconsole_y" | "gamepad_y" | "pad_y" => Some(&[3]),
        "uconsole_select" | "gamepad_select" | "pad_select" => Some(&[4, 6, 8]),
        "uconsole_start" | "gamepad_start" | "pad_start" => Some(&[5, 7, 9]),
        _ => None,
    }
}

impl GamepadRuntime {
    fn open() -> Self {
        #[cfg(target_os = "linux")]
        {
            Self {
                state: open_linux_gamepad(),
            }
        }
        #[cfg(not(target_os = "linux"))]
        {
            Self {}
        }
    }

    fn button_down(&self, button: usize) -> bool {
        #[cfg(target_os = "linux")]
        {
            let Some(state) = self.state.as_ref() else {
                return false;
            };
            let Ok(state) = state.lock() else {
                return false;
            };
            return state.buttons.get(button).copied().unwrap_or(false);
        }
        #[cfg(not(target_os = "linux"))]
        {
            let _ = button;
            false
        }
    }

    fn button_pressed(&self, button: usize) -> bool {
        #[cfg(target_os = "linux")]
        {
            let Some(state) = self.state.as_ref() else {
                return false;
            };
            let Ok(mut state) = state.lock() else {
                return false;
            };
            let pressed = state.pressed.get(button).copied().unwrap_or(false);
            if pressed {
                state.pressed[button] = false;
            }
            return pressed;
        }
        #[cfg(not(target_os = "linux"))]
        {
            let _ = button;
            false
        }
    }
}

#[cfg(target_os = "linux")]
fn open_linux_gamepad() -> Option<Arc<Mutex<GamepadState>>> {
    let paths = gamepad_paths();
    for path in paths {
        let Ok(file) = File::open(&path) else {
            continue;
        };
        let state = Arc::new(Mutex::new(GamepadState {
            buttons: vec![false; 64],
            pressed: vec![false; 64],
        }));
        let reader_state = Arc::clone(&state);
        let _ = thread::Builder::new()
            .name("prime-gamepad-js".into())
            .spawn(move || read_linux_joystick(file, reader_state));
        return Some(state);
    }
    None
}

#[cfg(target_os = "linux")]
fn gamepad_paths() -> Vec<String> {
    if let Ok(path) = env::var("PRIME_GAMEPAD_DEVICE") {
        return vec![path];
    }
    vec![
        "/dev/input/by-id/platform-noserial-joystick".into(),
        "/dev/input/js0".into(),
    ]
}

#[cfg(target_os = "linux")]
fn read_linux_joystick(mut file: File, state: Arc<Mutex<GamepadState>>) {
    const JS_EVENT_BUTTON: u8 = 0x01;
    const JS_EVENT_INIT: u8 = 0x80;
    loop {
        let mut event = [0_u8; 8];
        match file.read_exact(&mut event) {
            Ok(()) => {
                let value = i16::from_ne_bytes([event[4], event[5]]);
                let event_type = event[6];
                let number = event[7] as usize;
                if event_type & JS_EVENT_BUTTON == 0 {
                    continue;
                }
                let is_initial = event_type & JS_EVENT_INIT != 0;
                let Ok(mut state) = state.lock() else {
                    return;
                };
                if number >= state.buttons.len() {
                    state.buttons.resize(number + 1, false);
                    state.pressed.resize(number + 1, false);
                }
                let down = value != 0;
                if down && !state.buttons[number] && !is_initial {
                    state.pressed[number] = true;
                }
                state.buttons[number] = down;
            }
            Err(err) if err.kind() == ErrorKind::Interrupted => continue,
            Err(_) => return,
        }
    }
}

fn runtime_slot() -> Result<MutexGuard<'static, Option<GraphicsRuntime>>, String> {
    RUNTIME
        .0
        .lock()
        .map_err(|_| "graphics runtime lock is poisoned".to_string())
}

fn load_sprite(cache: &mut HashMap<String, Sprite>, path: &str) -> Result<Sprite, String> {
    if let Some(sprite) = cache.get(path) {
        return Ok(sprite.clone());
    }
    let resolved_path = resolve_asset_path(path);
    let decoded = ImageReader::open(&resolved_path)
        .map_err(|err| format!("failed to open sprite `{path}`: {err}"))?
        .decode()
        .map_err(|err| format!("failed to decode sprite `{path}`: {err}"))?
        .to_rgba8();
    let width = decoded.width() as usize;
    let height = decoded.height() as usize;
    let pixels = decoded
        .pixels()
        .map(|pixel| [pixel[0], pixel[1], pixel[2], pixel[3]])
        .collect();
    let sprite = Sprite {
        width,
        height,
        pixels,
    };
    cache.insert(path.to_string(), sprite.clone());
    Ok(sprite)
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

fn draw_rect(buffer: &mut BufferRuntime, x: i32, y: i32, width: i32, height: i32, color: u32) {
    if width <= 0 || height <= 0 {
        return;
    }
    let min_x = x.max(0) as usize;
    let min_y = y.max(0) as usize;
    let max_x = (x + width).min(buffer.width as i32).max(0) as usize;
    let max_y = (y + height).min(buffer.height as i32).max(0) as usize;
    for py in min_y..max_y {
        let row = py * buffer.width;
        for px in min_x..max_x {
            buffer.buffer[row + px] = color;
        }
    }
}

fn draw_sprite(buffer: &mut BufferRuntime, sprite: &Sprite, area: RectSpec, tint: ColorSpec) {
    let RectSpec {
        x,
        y,
        width,
        height,
    } = area;
    if width <= 0 || height <= 0 || sprite.width == 0 || sprite.height == 0 {
        return;
    }
    let tint_r = color_channel(tint.r);
    let tint_g = color_channel(tint.g);
    let tint_b = color_channel(tint.b);
    let min_x = x.max(0);
    let min_y = y.max(0);
    let max_x = (x + width).min(buffer.width as i32).max(0);
    let max_y = (y + height).min(buffer.height as i32).max(0);
    for py in min_y..max_y {
        let sy = ((py - y) as usize * sprite.height) / height as usize;
        let row = py as usize * buffer.width;
        for px in min_x..max_x {
            let sx = ((px - x) as usize * sprite.width) / width as usize;
            let pixel = sprite.pixels[sy * sprite.width + sx];
            if pixel[3] == 0 {
                continue;
            }
            let lum = ((pixel[0] as u32 + pixel[1] as u32 + pixel[2] as u32) / 3) as u8;
            let out_r = ((tint_r as u16 * lum as u16) / 255) as u8;
            let out_g = ((tint_g as u16 * lum as u16) / 255) as u8;
            let out_b = ((tint_b as u16 * lum as u16) / 255) as u8;
            buffer.buffer[row + px as usize] =
                ((out_r as u32) << 16) | ((out_g as u32) << 8) | out_b as u32;
        }
    }
}

fn draw_text(buffer: &mut BufferRuntime, text: &str, x: i32, y: i32, scale: i32, color: u32) {
    let mut cursor = x;
    for ch in text.chars() {
        if ch == '\n' {
            cursor = x;
            continue;
        }
        let glyph = glyph_rows(ch);
        for (row_index, row_bits) in glyph.iter().enumerate() {
            for col in 0..5 {
                if row_bits & (1 << (4 - col)) != 0 {
                    draw_rect(
                        buffer,
                        cursor + col * scale,
                        y + row_index as i32 * scale,
                        scale,
                        scale,
                        color,
                    );
                }
            }
        }
        cursor += 6 * scale;
    }
}

fn color_u32(r: i32, g: i32, b: i32) -> u32 {
    ((color_channel(r) as u32) << 16) | ((color_channel(g) as u32) << 8) | color_channel(b) as u32
}

fn color_channel(value: i32) -> u8 {
    value.clamp(0, 255) as u8
}

fn key_from_name(name: &str) -> Option<Key> {
    match name.to_ascii_lowercase().as_str() {
        "left" => Some(Key::Left),
        "right" => Some(Key::Right),
        "up" => Some(Key::Up),
        "down" => Some(Key::Down),
        "space" => Some(Key::Space),
        "enter" | "return" => Some(Key::Enter),
        "escape" | "esc" => Some(Key::Escape),
        "z" => Some(Key::Z),
        "x" => Some(Key::X),
        "c" => Some(Key::C),
        "p" => Some(Key::P),
        "r" => Some(Key::R),
        "a" => Some(Key::A),
        "d" => Some(Key::D),
        "s" => Some(Key::S),
        "w" => Some(Key::W),
        _ => None,
    }
}

fn glyph_rows(ch: char) -> [u8; 7] {
    match ch.to_ascii_uppercase() {
        'A' => [
            0b01110, 0b10001, 0b10001, 0b11111, 0b10001, 0b10001, 0b10001,
        ],
        'B' => [
            0b11110, 0b10001, 0b10001, 0b11110, 0b10001, 0b10001, 0b11110,
        ],
        'C' => [
            0b01111, 0b10000, 0b10000, 0b10000, 0b10000, 0b10000, 0b01111,
        ],
        'D' => [
            0b11110, 0b10001, 0b10001, 0b10001, 0b10001, 0b10001, 0b11110,
        ],
        'E' => [
            0b11111, 0b10000, 0b10000, 0b11110, 0b10000, 0b10000, 0b11111,
        ],
        'F' => [
            0b11111, 0b10000, 0b10000, 0b11110, 0b10000, 0b10000, 0b10000,
        ],
        'G' => [
            0b01111, 0b10000, 0b10000, 0b10111, 0b10001, 0b10001, 0b01111,
        ],
        'H' => [
            0b10001, 0b10001, 0b10001, 0b11111, 0b10001, 0b10001, 0b10001,
        ],
        'I' => [
            0b11111, 0b00100, 0b00100, 0b00100, 0b00100, 0b00100, 0b11111,
        ],
        'J' => [
            0b00111, 0b00010, 0b00010, 0b00010, 0b10010, 0b10010, 0b01100,
        ],
        'K' => [
            0b10001, 0b10010, 0b10100, 0b11000, 0b10100, 0b10010, 0b10001,
        ],
        'L' => [
            0b10000, 0b10000, 0b10000, 0b10000, 0b10000, 0b10000, 0b11111,
        ],
        'M' => [
            0b10001, 0b11011, 0b10101, 0b10101, 0b10001, 0b10001, 0b10001,
        ],
        'N' => [
            0b10001, 0b11001, 0b10101, 0b10011, 0b10001, 0b10001, 0b10001,
        ],
        'O' => [
            0b01110, 0b10001, 0b10001, 0b10001, 0b10001, 0b10001, 0b01110,
        ],
        'P' => [
            0b11110, 0b10001, 0b10001, 0b11110, 0b10000, 0b10000, 0b10000,
        ],
        'Q' => [
            0b01110, 0b10001, 0b10001, 0b10001, 0b10101, 0b10010, 0b01101,
        ],
        'R' => [
            0b11110, 0b10001, 0b10001, 0b11110, 0b10100, 0b10010, 0b10001,
        ],
        'S' => [
            0b01111, 0b10000, 0b10000, 0b01110, 0b00001, 0b00001, 0b11110,
        ],
        'T' => [
            0b11111, 0b00100, 0b00100, 0b00100, 0b00100, 0b00100, 0b00100,
        ],
        'U' => [
            0b10001, 0b10001, 0b10001, 0b10001, 0b10001, 0b10001, 0b01110,
        ],
        'V' => [
            0b10001, 0b10001, 0b10001, 0b10001, 0b10001, 0b01010, 0b00100,
        ],
        'W' => [
            0b10001, 0b10001, 0b10001, 0b10101, 0b10101, 0b11011, 0b10001,
        ],
        'X' => [
            0b10001, 0b10001, 0b01010, 0b00100, 0b01010, 0b10001, 0b10001,
        ],
        'Y' => [
            0b10001, 0b10001, 0b01010, 0b00100, 0b00100, 0b00100, 0b00100,
        ],
        'Z' => [
            0b11111, 0b00001, 0b00010, 0b00100, 0b01000, 0b10000, 0b11111,
        ],
        '0' => [
            0b01110, 0b10001, 0b10011, 0b10101, 0b11001, 0b10001, 0b01110,
        ],
        '1' => [
            0b00100, 0b01100, 0b00100, 0b00100, 0b00100, 0b00100, 0b01110,
        ],
        '2' => [
            0b01110, 0b10001, 0b00001, 0b00110, 0b01000, 0b10000, 0b11111,
        ],
        '3' => [
            0b11110, 0b00001, 0b00001, 0b01110, 0b00001, 0b00001, 0b11110,
        ],
        '4' => [
            0b00010, 0b00110, 0b01010, 0b10010, 0b11111, 0b00010, 0b00010,
        ],
        '5' => [
            0b11111, 0b10000, 0b10000, 0b11110, 0b00001, 0b00001, 0b11110,
        ],
        '6' => [
            0b01110, 0b10000, 0b10000, 0b11110, 0b10001, 0b10001, 0b01110,
        ],
        '7' => [
            0b11111, 0b00001, 0b00010, 0b00100, 0b01000, 0b01000, 0b01000,
        ],
        '8' => [
            0b01110, 0b10001, 0b10001, 0b01110, 0b10001, 0b10001, 0b01110,
        ],
        '9' => [
            0b01110, 0b10001, 0b10001, 0b01111, 0b00001, 0b00001, 0b01110,
        ],
        ':' => [0, 0b00100, 0b00100, 0, 0b00100, 0b00100, 0],
        '-' => [0, 0, 0, 0b11111, 0, 0, 0],
        '+' => [0, 0b00100, 0b00100, 0b11111, 0b00100, 0b00100, 0],
        '/' => [
            0b00001, 0b00010, 0b00010, 0b00100, 0b01000, 0b01000, 0b10000,
        ],
        '.' => [0, 0, 0, 0, 0, 0b01100, 0b01100],
        ',' => [0, 0, 0, 0, 0, 0b01100, 0b01000],
        '!' => [0b00100, 0b00100, 0b00100, 0b00100, 0b00100, 0, 0b00100],
        '?' => [0b01110, 0b10001, 0b00001, 0b00010, 0b00100, 0, 0b00100],
        '(' => [
            0b00010, 0b00100, 0b01000, 0b01000, 0b01000, 0b00100, 0b00010,
        ],
        ')' => [
            0b01000, 0b00100, 0b00010, 0b00010, 0b00010, 0b00100, 0b01000,
        ],
        '[' => [
            0b01110, 0b01000, 0b01000, 0b01000, 0b01000, 0b01000, 0b01110,
        ],
        ']' => [
            0b01110, 0b00010, 0b00010, 0b00010, 0b00010, 0b00010, 0b01110,
        ],
        ' ' => [0, 0, 0, 0, 0, 0, 0],
        _ => [0b11111, 0b10001, 0b00010, 0b00100, 0b00100, 0, 0b00100],
    }
}
