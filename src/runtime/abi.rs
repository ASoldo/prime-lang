#![allow(unsafe_op_in_unsafe_fn)]
#![cfg_attr(
    any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ),
    no_std
)]

// Minimal no_std runtime ABI for embedded targets.
// Host builds reuse the same minimal surface just to satisfy compiler imports.

mod embedded {
    use core::ffi::c_void;
    use core::ptr;
    use core::sync::atomic::{AtomicBool, Ordering};

    #[repr(C)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum PrimeStatus {
        Ok = 0,
        Invalid = 1,
        Closed = 2,
        Panic = 3,
    }

    #[repr(C)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum PrimeTag {
        Unit = 0,
        Int = 1,
        Float = 2,
        Bool = 3,
        String = 4,
        Slice = 5,
        Map = 6,
        Enum = 7,
        Reference = 8,
        Sender = 9,
        Receiver = 10,
        JoinHandle = 11,
        Struct = 12,
    }

    #[repr(transparent)]
    #[derive(Clone, Copy)]
    pub struct PrimeHandle(*mut c_void);

    impl PrimeHandle {
        pub const fn null() -> Self {
            Self(ptr::null_mut())
        }
    }

    unsafe impl Send for PrimeHandle {}
    unsafe impl Sync for PrimeHandle {}

    // Type codes used by the compiler.
    pub const TYPE_STRING: u32 = 0;
    pub const TYPE_BOOL: u32 = 1;
    pub const TYPE_INT8: u32 = 2;
    pub const TYPE_INT16: u32 = 3;
    pub const TYPE_INT32: u32 = 4;
    pub const TYPE_INT64: u32 = 5;
    pub const TYPE_ISIZE: u32 = 6;
    pub const TYPE_UINT8: u32 = 7;
    pub const TYPE_UINT16: u32 = 8;
    pub const TYPE_UINT32: u32 = 9;
    pub const TYPE_UINT64: u32 = 10;
    pub const TYPE_USIZE: u32 = 11;
    pub const TYPE_FLOAT32: u32 = 12;
    pub const TYPE_FLOAT64: u32 = 13;
    pub const TYPE_RUNE: u32 = 14;

    // Required FFI entry points used by the compiler.
    #[unsafe(export_name = "prime_value_retain")]
    pub unsafe extern "C" fn prime_value_retain(value: PrimeHandle) -> PrimeHandle {
        value
    }

    #[unsafe(export_name = "prime_value_release")]
    pub unsafe extern "C" fn prime_value_release(_value: PrimeHandle) {}

    #[unsafe(export_name = "prime_unit_new")]
    pub unsafe extern "C" fn prime_unit_new() -> PrimeHandle {
        PrimeHandle::null()
    }

    #[unsafe(export_name = "prime_int_new")]
    pub unsafe extern "C" fn prime_int_new(_value: i128) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[unsafe(export_name = "prime_float_new")]
    pub unsafe extern "C" fn prime_float_new(_value: f64) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[unsafe(export_name = "prime_bool_new")]
    pub unsafe extern "C" fn prime_bool_new(_value: bool) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[unsafe(export_name = "prime_string_new")]
    pub unsafe extern "C" fn prime_string_new(_data: *const u8, _len: usize) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[unsafe(export_name = "prime_slice_new")]
    pub unsafe extern "C" fn prime_slice_new() -> PrimeHandle {
        PrimeHandle::null()
    }

    #[unsafe(export_name = "prime_map_new")]
    pub unsafe extern "C" fn prime_map_new() -> PrimeHandle {
        PrimeHandle::null()
    }

    #[unsafe(export_name = "prime_struct_new")]
    pub unsafe extern "C" fn prime_struct_new(_name_ptr: *const u8, _name_len: usize) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[unsafe(export_name = "prime_enum_new")]
    pub unsafe extern "C" fn prime_enum_new(
        _tag: u32,
        _values_ptr: *const PrimeHandle,
        _values_len: usize,
    ) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[unsafe(export_name = "prime_reference_new")]
    pub unsafe extern "C" fn prime_reference_new(_target: PrimeHandle, _mutable: bool) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[unsafe(export_name = "prime_format")]
    pub unsafe extern "C" fn prime_format(
        _fmt_parts_ptr: *const PrimeHandle,
        _fmt_parts_len: usize,
        _fmt_values_ptr: *const PrimeHandle,
        _fmt_values_len: usize,
    ) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[unsafe(export_name = "prime_spawn")]
    pub unsafe extern "C" fn prime_spawn(
        entry: Option<unsafe extern "C" fn(arg: PrimeHandle) -> PrimeHandle>,
        arg: PrimeHandle,
        handle_out: *mut PrimeHandle,
    ) -> PrimeHandle {
        let _ = (entry, arg, handle_out);
        PrimeHandle::null()
    }

    #[unsafe(export_name = "prime_slice_push")]
    pub unsafe extern "C" fn prime_slice_push(_slice: PrimeHandle, _value: PrimeHandle) -> PrimeStatus {
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_slice_get")]
    pub unsafe extern "C" fn prime_slice_get(
        _slice: PrimeHandle,
        _idx: usize,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if !value_out.is_null() {
            value_out.write(PrimeHandle::null());
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_slice_remove")]
    pub unsafe extern "C" fn prime_slice_remove(
        _slice: PrimeHandle,
        _idx: usize,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if !value_out.is_null() {
            value_out.write(PrimeHandle::null());
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_slice_set")]
    pub unsafe extern "C" fn prime_slice_set(_slice: PrimeHandle, _value: PrimeHandle) -> PrimeStatus {
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_map_insert")]
    pub unsafe extern "C" fn prime_map_insert(
        _map: PrimeHandle,
        _key_ptr: *const u8,
        _key_len: usize,
        _value: PrimeHandle,
    ) -> PrimeStatus {
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_map_get")]
    pub unsafe extern "C" fn prime_map_get(
        _map: PrimeHandle,
        _key_ptr: *const u8,
        _key_len: usize,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if !value_out.is_null() {
            value_out.write(PrimeHandle::null());
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_map_remove")]
    pub unsafe extern "C" fn prime_map_remove(
        _map: PrimeHandle,
        _key_ptr: *const u8,
        _key_len: usize,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if !value_out.is_null() {
            value_out.write(PrimeHandle::null());
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_struct_set")]
    pub unsafe extern "C" fn prime_struct_set(
        _handle: PrimeHandle,
        _field_ptr: *const u8,
        _field_len: usize,
        _value: PrimeHandle,
    ) -> PrimeStatus {
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_reference_write")]
    pub unsafe extern "C" fn prime_reference_write(_target: PrimeHandle, _value: PrimeHandle) -> PrimeStatus {
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_send")]
    pub unsafe extern "C" fn prime_send(_sender: PrimeHandle, _value: PrimeHandle) -> PrimeStatus {
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_recv")]
    pub unsafe extern "C" fn prime_recv(_receiver: PrimeHandle, value_out: *mut PrimeHandle) -> PrimeStatus {
        if !value_out.is_null() {
            value_out.write(PrimeHandle::null());
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_recv_timeout")]
    pub unsafe extern "C" fn prime_recv_timeout(
        _receiver: PrimeHandle,
        _millis: i64,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if !value_out.is_null() {
            value_out.write(PrimeHandle::null());
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_close")]
    pub unsafe extern "C" fn prime_close(_handle: PrimeHandle) -> PrimeStatus {
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_join")]
    pub unsafe extern "C" fn prime_join(_handle: PrimeHandle, result_out: *mut PrimeHandle) -> PrimeStatus {
        if !result_out.is_null() {
            result_out.write(PrimeHandle::null());
        }
        PrimeStatus::Invalid
    }

    #[cfg(any(target_arch = "xtensa", all(target_arch = "riscv32", target_os = "none")))]
    #[panic_handler]
    fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
        loop {
            core::hint::spin_loop();
        }
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_slice_len_handle(_handle: PrimeHandle) -> usize {
        0
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_map_len_handle(_handle: PrimeHandle) -> usize {
        0
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_enum_tag(_handle: PrimeHandle) -> u32 {
        0
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_enum_get(_handle: PrimeHandle, _idx: usize) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_reference_read(_target: PrimeHandle) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_print(_value: PrimeHandle) {}

    #[unsafe(export_name = "prime_now_ms")]
    pub unsafe extern "C" fn prime_now_ms() -> i128 {
        0
    }

    #[cfg(target_arch = "xtensa")]
    unsafe extern "C" {
        static mut _bss_start: u32;
        static mut _bss_end: u32;
        static mut _data_start: u32;
        static mut _data_end: u32;
        static _data_seg_org: u32;
        fn main();
    }

    // Minimal ROM bindings for timing.
    #[cfg(target_arch = "xtensa")]
    unsafe extern "C" {
        fn ets_delay_us(us: u32);
        fn ets_printf(fmt: *const u8, ...) -> i32;
    }

    #[inline]
    fn delay_us(us: u32) {
        #[cfg(target_arch = "xtensa")]
        unsafe {
            ets_delay_us(us);
        }
        #[cfg(not(target_arch = "xtensa"))]
        {
            // Crude fallback for host builds; not used on-device.
            let spins = us.saturating_mul(200);
            for _ in 0..spins {
                core::hint::spin_loop();
            }
        }
    }

    static LOG_ONCE: AtomicBool = AtomicBool::new(false);

    #[inline]
    fn log_hello_once() {
        if LOG_ONCE.swap(true, Ordering::Relaxed) {
            return;
        }
        #[cfg(target_arch = "xtensa")]
        unsafe {
            ets_printf(b"hello esp32 world\n\0".as_ptr());
        }
    }

    // Register offsets for classic ESP32 (Xtensa). Kept tiny so we don't pull in IDF.
    const DR_REG_GPIO_BASE: u32 = 0x3ff44000;
    const GPIO_OUT_W1TS_REG: *mut u32 = (DR_REG_GPIO_BASE + 0x0008) as *mut u32;
    const GPIO_OUT_W1TC_REG: *mut u32 = (DR_REG_GPIO_BASE + 0x000c) as *mut u32;
    const GPIO_ENABLE_W1TS_REG: *mut u32 = (DR_REG_GPIO_BASE + 0x0024) as *mut u32;

    const DR_REG_IO_MUX_BASE: u32 = 0x3ff49000;
    // Only used for the demo pin (GPIO2). Extend as needed for more pins.
    const IO_MUX_GPIO2_REG: *mut u32 = (DR_REG_IO_MUX_BASE + 0x40) as *mut u32;
    const MCU_SEL_MASK: u32 = 0b111 << 12;
    const FUNC_GPIO: u32 = 0b010;

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_delay_ms(ms: i32) -> PrimeStatus {
        log_hello_once();
        // Use ROM delay for a reasonably accurate millisecond delay.
        let us = (ms as i64).max(0).min((u32::MAX / 1000) as i64) as u32 * 1000;
        delay_us(us);
        PrimeStatus::Ok
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_pin_mode(pin: i32, mode: i32) -> PrimeStatus {
        log_hello_once();
        // Mode 1 -> output (matches demo usage).
        if mode != 1 {
            return PrimeStatus::Invalid;
        }
        if pin != 2 {
            return PrimeStatus::Invalid;
        }
        // Route GPIO2 to the GPIO peripheral.
        let reg = IO_MUX_GPIO2_REG;
        let mut val = core::ptr::read_volatile(reg);
        val &= !MCU_SEL_MASK;
        val |= FUNC_GPIO << 12;
        core::ptr::write_volatile(reg, val);

        // Enable output driver for the pin.
        let mask = 1u32 << (pin as u32);
        core::ptr::write_volatile(GPIO_ENABLE_W1TS_REG, mask);
        PrimeStatus::Ok
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_digital_write(pin: i32, level: i32) -> PrimeStatus {
        if pin != 2 {
            return PrimeStatus::Invalid;
        }
        let mask = 1u32 << (pin as u32);
        if level != 0 {
            core::ptr::write_volatile(GPIO_OUT_W1TS_REG, mask);
        } else {
            core::ptr::write_volatile(GPIO_OUT_W1TC_REG, mask);
        }
        PrimeStatus::Ok
    }

    #[cfg(target_arch = "xtensa")]
    #[inline]
    unsafe fn zero_bss() {
        let mut p = core::ptr::addr_of!(_bss_start) as *mut u32;
        let end = core::ptr::addr_of!(_bss_end) as *mut u32;
        while p < end {
            core::ptr::write_volatile(p, 0);
            p = p.add(1);
        }
    }

    #[cfg(target_arch = "xtensa")]
    #[inline]
    unsafe fn init_data() {
        let mut rom = core::ptr::addr_of!(_data_seg_org) as *const u32;
        let mut ram = core::ptr::addr_of!(_data_start) as *mut u32;
        let end = core::ptr::addr_of!(_data_end) as *mut u32;
        while ram < end {
            core::ptr::write_volatile(ram, core::ptr::read_volatile(rom));
            ram = ram.add(1);
            rom = rom.add(1);
        }
    }

    #[cfg(target_arch = "xtensa")]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn call_user_start_cpu0() -> ! {
        zero_bss();
        init_data();
        main();
        loop {
            core::hint::spin_loop();
        }
    }

}

#[cfg(any(
    target_arch = "xtensa",
    all(target_arch = "riscv32", target_os = "none"),
))]
pub use embedded::*;

#[cfg(not(any(
    target_arch = "xtensa",
    all(target_arch = "riscv32", target_os = "none"),
)))]
mod host {
    // Export the same symbols for host builds to satisfy compiler imports.
    pub use super::embedded::*;
}

#[cfg(not(any(
    target_arch = "xtensa",
    all(target_arch = "riscv32", target_os = "none"),
)))]
pub use host::*;
