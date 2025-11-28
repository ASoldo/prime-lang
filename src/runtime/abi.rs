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

    // Built-ins for embedded: implement delay/pin/digital write as busy loops/stubs.
    #[inline]
    fn busy_delay_ms(ms: i32) {
        // Crude spin to avoid pulling in std/usleep.
        let cycles = (ms as u32).saturating_mul(50_000);
        for _ in 0..cycles {
            core::hint::spin_loop();
        }
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_delay_ms(ms: i32) -> PrimeStatus {
        busy_delay_ms(ms);
        PrimeStatus::Ok
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_pin_mode(_pin: i32, _mode: i32) -> PrimeStatus {
        PrimeStatus::Ok
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_digital_write(_pin: i32, _level: i32) -> PrimeStatus {
        PrimeStatus::Ok
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
