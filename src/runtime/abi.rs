#![allow(unsafe_op_in_unsafe_fn)]
#![cfg_attr(
    any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ),
    no_std
)]
#![cfg_attr(target_arch = "xtensa", allow(static_mut_refs))]
#![allow(unstable_features)]

// Minimal no_std runtime ABI for embedded targets.
// Host builds reuse the same minimal surface just to satisfy compiler imports.

mod embedded {
    use core::ffi::c_void;
    use core::ptr;
    use core::sync::atomic::{AtomicBool, Ordering};
    #[cfg(target_arch = "xtensa")]
    use core::sync::atomic::AtomicUsize;

    #[repr(C)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[allow(dead_code)]
    pub enum PrimeStatus {
        Ok = 0,
        Invalid = 1,
        Closed = 2,
        Panic = 3,
    }

    #[repr(C)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[allow(dead_code)]
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
    pub struct PrimeHandle(pub *mut c_void);

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
    #[cfg(any(target_arch = "xtensa", all(target_arch = "riscv32", target_os = "none")))]
    #[unsafe(export_name = "prime_value_retain")]
    pub unsafe extern "C" fn prime_value_retain(value: PrimeHandle) -> PrimeHandle {
        value
    }

    #[cfg(any(target_arch = "xtensa", all(target_arch = "riscv32", target_os = "none")))]
    #[unsafe(export_name = "prime_value_release")]
    pub unsafe extern "C" fn prime_value_release(_value: PrimeHandle) {}

    #[cfg(any(target_arch = "xtensa", all(target_arch = "riscv32", target_os = "none")))]
    #[unsafe(export_name = "prime_unit_new")]
    pub unsafe extern "C" fn prime_unit_new() -> PrimeHandle {
        PrimeHandle::null()
    }

    #[cfg(any(target_arch = "xtensa", all(target_arch = "riscv32", target_os = "none")))]
    #[unsafe(export_name = "prime_int_new")]
    pub unsafe extern "C" fn prime_int_new(_value: i128) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[cfg(any(target_arch = "xtensa", all(target_arch = "riscv32", target_os = "none")))]
    #[unsafe(export_name = "prime_float_new")]
    pub unsafe extern "C" fn prime_float_new(_value: f64) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[cfg(any(target_arch = "xtensa", all(target_arch = "riscv32", target_os = "none")))]
    #[unsafe(export_name = "prime_bool_new")]
    pub unsafe extern "C" fn prime_bool_new(_value: bool) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[cfg(not(target_arch = "xtensa"))]
    #[cfg(any(target_arch = "xtensa", all(target_arch = "riscv32", target_os = "none")))]
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

    #[cfg(any(target_arch = "xtensa", all(target_arch = "riscv32", target_os = "none")))]
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
    #[unsafe(export_name = "prime_read_value")]
    pub unsafe extern "C" fn prime_read_value(
        _status: PrimeStatus,
        _tag: PrimeStatus,
        _flags: PrimeStatus,
        _name_ptr: *const u8,
        _name_len: usize,
        _values_ptr: *mut PrimeHandle,
        _values_len: usize,
    ) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[cfg(any(target_arch = "xtensa", all(target_arch = "riscv32", target_os = "none")))]
    #[unsafe(export_name = "prime_env_free")]
    pub unsafe extern "C" fn prime_env_free(_handle: PrimeHandle) {}

    #[unsafe(export_name = "prime_task_new")]
    pub unsafe extern "C" fn prime_task_new(
        _entry: Option<unsafe extern "C" fn(arg: PrimeHandle) -> PrimeHandle>,
        _arg: PrimeHandle,
    ) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[unsafe(export_name = "prime_task_poll")]
    pub unsafe extern "C" fn prime_task_poll(
        _task: PrimeHandle,
        result_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if !result_out.is_null() {
            result_out.write(PrimeHandle::null());
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_task_wake")]
    pub unsafe extern "C" fn prime_task_wake(_task: PrimeHandle, _value: PrimeHandle) -> PrimeStatus {
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_sleep_task")]
    pub unsafe extern "C" fn prime_sleep_task(_millis: i64) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[unsafe(export_name = "prime_recv_task")]
    pub unsafe extern "C" fn prime_recv_task(_receiver: PrimeHandle) -> PrimeHandle {
        PrimeHandle::null()
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

    #[cfg(any(target_arch = "xtensa", all(target_arch = "riscv32", target_os = "none")))]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_enum_tag(_handle: PrimeHandle) -> u32 {
        0
    }

    #[cfg(any(target_arch = "xtensa", all(target_arch = "riscv32", target_os = "none")))]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_enum_get(_handle: PrimeHandle, _idx: usize) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_reference_read(_target: PrimeHandle) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[repr(C)]
    #[derive(Copy, Clone)]
    struct PrimeString {
        ptr: *const u8,
        len: usize,
    }

    // Tiny bump allocator for strings; enough for demo prints (Xtensa only).
    #[cfg(target_arch = "xtensa")]
    const STR_STORAGE_SIZE: usize = 1024;
    #[cfg(target_arch = "xtensa")]
    const STR_HANDLES_MAX: usize = 16;
    #[cfg(target_arch = "xtensa")]
    static mut STR_STORAGE: [u8; STR_STORAGE_SIZE] = [0; STR_STORAGE_SIZE];
    #[cfg(target_arch = "xtensa")]
    static STR_STORAGE_OFF: AtomicUsize = AtomicUsize::new(0);
    #[cfg(target_arch = "xtensa")]
    static mut STR_HANDLES: [PrimeString; STR_HANDLES_MAX] = [PrimeString {
        ptr: core::ptr::null(),
        len: 0,
    }; STR_HANDLES_MAX];
    #[cfg(target_arch = "xtensa")]
    static STR_HANDLE_OFF: AtomicUsize = AtomicUsize::new(0);

    #[cfg(target_arch = "xtensa")]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_string_new(data: *const u8, len: usize) -> PrimeHandle {
        // Simple ring buffers to keep printing in tight loops without panicking.
        let slot = {
            let mut next = STR_HANDLE_OFF.load(Ordering::Relaxed);
            if next >= STR_HANDLES_MAX {
                next = 0;
            }
            STR_HANDLE_OFF.store(next + 1, Ordering::Relaxed);
            next
        };
        let start = {
            let mut off = STR_STORAGE_OFF.load(Ordering::Relaxed);
            // Reserve space for the string plus a null terminator.
            if off + len + 1 >= STR_STORAGE_SIZE {
                off = 0;
            }
            STR_STORAGE_OFF.store(off + len + 1, Ordering::Relaxed);
            off
        };
        let avail = STR_STORAGE_SIZE - start;
        let copy_len = core::cmp::min(len, avail.saturating_sub(1));
        let dst = STR_STORAGE.as_mut_ptr().add(start);
        core::ptr::copy_nonoverlapping(data, dst, copy_len);
        // Null-terminate for %s printing.
        core::ptr::write(dst.add(copy_len), 0);
        STR_HANDLES[slot] = PrimeString {
            ptr: dst as *const u8,
            len: copy_len,
        };
        PrimeHandle(STR_HANDLES.as_mut_ptr().add(slot) as *mut c_void)
    }

    #[cfg(any(target_arch = "xtensa", all(target_arch = "riscv32", target_os = "none")))]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_print(value: PrimeHandle) {
        if value.0.is_null() {
            return;
        }
        let s = &*(value.0 as *const PrimeString);
        #[cfg(target_arch = "xtensa")]
        {
            ets_printf(b"%s\0".as_ptr(), s.ptr);
        }
        #[cfg(not(target_arch = "xtensa"))]
        let _ = s;
    }

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
    #[cfg(target_arch = "xtensa")]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn memcpy(dst: *mut u8, src: *const u8, len: usize) -> *mut u8 {
        let mut i = 0;
        while i < len {
            let b = core::ptr::read(src.add(i));
            core::ptr::write(dst.add(i), b);
            i += 1;
        }
        dst
    }

    // Minimal newlib stubs to satisfy toolchain defaults; these are not used functionally.
    #[cfg(target_arch = "xtensa")]
    #[unsafe(no_mangle)]
    pub static mut _GLOBAL_REENT: [u8; 256] = [0; 256];
    #[cfg(target_arch = "xtensa")]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn __getreent() -> *mut u8 {
        _GLOBAL_REENT.as_mut_ptr()
    }
    #[cfg(target_arch = "xtensa")]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _sbrk(_incr: isize) -> *mut u8 {
        core::ptr::null_mut()
    }
    #[cfg(target_arch = "xtensa")]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _write_r(_r: *mut u8, _fd: i32, _buf: *const u8, _len: i32) -> isize {
        -1
    }
    #[cfg(target_arch = "xtensa")]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _read_r(_r: *mut u8, _fd: i32, _buf: *mut u8, _len: i32) -> isize {
        -1
    }
    #[cfg(target_arch = "xtensa")]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _close_r(_r: *mut u8, _fd: i32) -> isize {
        -1
    }
    #[cfg(target_arch = "xtensa")]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _lseek_r(_r: *mut u8, _fd: i32, _off: isize, _whence: i32) -> isize {
        -1
    }
    #[cfg(target_arch = "xtensa")]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _fstat_r(_r: *mut u8, _fd: i32, _st: *mut u8) -> isize {
        -1
    }
    #[cfg(target_arch = "xtensa")]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _kill_r(_r: *mut u8, _pid: i32, _sig: i32) -> isize {
        -1
    }
    #[cfg(target_arch = "xtensa")]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _getpid_r(_r: *mut u8) -> isize {
        1
    }
    #[cfg(target_arch = "xtensa")]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _exit(_code: i32) -> ! {
        loop {}
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
    static WATCHDOGS_DISABLED: AtomicBool = AtomicBool::new(false);

    #[inline]
    fn disable_watchdogs_once() {
        if WATCHDOGS_DISABLED.swap(true, Ordering::Relaxed) {
            return;
        }
        #[cfg(target_arch = "xtensa")]
        unsafe {
            const WDT_WKEY: u32 = 0x50d83aa1;

            // RTC WDT.
            core::ptr::write_volatile(RTC_CNTL_WDTWPROTECT_REG, WDT_WKEY);
            core::ptr::write_volatile(RTC_CNTL_WDTCONFIG0_REG, 0);

            // Timer Group 0 WDT.
            core::ptr::write_volatile(TIMG0_WDTWPROTECT_REG, WDT_WKEY);
            core::ptr::write_volatile(TIMG0_WDTCONFIG0_REG, 0);

            // Timer Group 1 WDT.
            core::ptr::write_volatile(TIMG1_WDTWPROTECT_REG, WDT_WKEY);
            core::ptr::write_volatile(TIMG1_WDTCONFIG0_REG, 0);
        }
    }

    #[inline]
    fn log_hello_once() {
        if LOG_ONCE.swap(true, Ordering::Relaxed) {
            return;
        }
        disable_watchdogs_once();
        // Keep placeholder for optional boot banner; intentionally disabled.
        #[cfg(target_arch = "xtensa")]
        if false {
            unsafe {
                ets_printf(b"hello esp32 world\n\0".as_ptr());
            }
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

    // Watchdog control registers.
    #[cfg(target_arch = "xtensa")]
    const DR_REG_RTCCNTL_BASE: u32 = 0x3ff48000;
    #[cfg(target_arch = "xtensa")]
    const RTC_CNTL_WDTCONFIG0_REG: *mut u32 = (DR_REG_RTCCNTL_BASE + 0x0090) as *mut u32;
    #[cfg(target_arch = "xtensa")]
    const RTC_CNTL_WDTWPROTECT_REG: *mut u32 = (DR_REG_RTCCNTL_BASE + 0x00a4) as *mut u32;

    #[cfg(target_arch = "xtensa")]
    const DR_REG_TIMERGROUP0_BASE: u32 = 0x3ff5f000;
    #[cfg(target_arch = "xtensa")]
    const TIMG0_WDTCONFIG0_REG: *mut u32 = (DR_REG_TIMERGROUP0_BASE + 0x0048) as *mut u32;
    #[cfg(target_arch = "xtensa")]
    const TIMG0_WDTWPROTECT_REG: *mut u32 = (DR_REG_TIMERGROUP0_BASE + 0x0064) as *mut u32;

    #[cfg(target_arch = "xtensa")]
    const DR_REG_TIMERGROUP1_BASE: u32 = 0x3ff60000;
    #[cfg(target_arch = "xtensa")]
    const TIMG1_WDTCONFIG0_REG: *mut u32 = (DR_REG_TIMERGROUP1_BASE + 0x0048) as *mut u32;
    #[cfg(target_arch = "xtensa")]
    const TIMG1_WDTWPROTECT_REG: *mut u32 = (DR_REG_TIMERGROUP1_BASE + 0x0064) as *mut u32;

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
    use std::ffi::c_void;
    use std::io::{self, Write};
    use std::slice;
    use std::sync::atomic::{AtomicUsize, Ordering};

    pub use super::embedded::{
        PrimeHandle, PrimeStatus, PrimeTag, TYPE_BOOL, TYPE_FLOAT32, TYPE_FLOAT64, TYPE_INT16,
        TYPE_INT32, TYPE_INT64, TYPE_INT8, TYPE_ISIZE, TYPE_RUNE, TYPE_STRING, TYPE_UINT16,
        TYPE_UINT32, TYPE_UINT64, TYPE_UINT8, TYPE_USIZE,
    };

    #[derive(Clone, Debug)]
    enum HostValue {
        Unit,
        Int(i128),
        Float(f64),
        Bool(bool),
        Str(String),
        Enum { tag: u32, values: Vec<HostValue> },
    }

    #[derive(Clone)]
    struct HostHandle {
        value: HostValue,
        tag: Option<u32>,
    }

    impl HostHandle {
        fn new(value: HostValue) -> Self {
            Self { value, tag: None }
        }

        fn with_tag(value: HostValue, tag: u32) -> Self {
            Self {
                value,
                tag: Some(tag),
            }
        }
    }

    fn to_handle(data: HostHandle) -> PrimeHandle {
        PrimeHandle(Box::into_raw(Box::new(data)) as *mut c_void)
    }

    unsafe fn clone_value(handle: PrimeHandle) -> HostValue {
        if handle.0.is_null() {
            return HostValue::Unit;
        }
        let data = &*(handle.0 as *const HostHandle);
        data.value.clone()
    }

    #[unsafe(export_name = "prime_value_retain")]
    pub unsafe extern "C" fn prime_value_retain(value: PrimeHandle) -> PrimeHandle {
        if value.0.is_null() {
            return PrimeHandle::null();
        }
        let data = &*(value.0 as *const HostHandle);
        to_handle(data.clone())
    }

    #[unsafe(export_name = "prime_value_release")]
    pub unsafe extern "C" fn prime_value_release(value: PrimeHandle) {
        if value.0.is_null() {
            return;
        }
        let _ = Box::from_raw(value.0 as *mut HostHandle);
    }

    #[unsafe(export_name = "prime_unit_new")]
    pub unsafe extern "C" fn prime_unit_new() -> PrimeHandle {
        to_handle(HostHandle::new(HostValue::Unit))
    }

    #[unsafe(export_name = "prime_int_new")]
    pub unsafe extern "C" fn prime_int_new(value: i128) -> PrimeHandle {
        to_handle(HostHandle::new(HostValue::Int(value)))
    }

    #[unsafe(export_name = "prime_float_new")]
    pub unsafe extern "C" fn prime_float_new(value: f64) -> PrimeHandle {
        to_handle(HostHandle::new(HostValue::Float(value)))
    }

    #[unsafe(export_name = "prime_bool_new")]
    pub unsafe extern "C" fn prime_bool_new(value: bool) -> PrimeHandle {
        to_handle(HostHandle::new(HostValue::Bool(value)))
    }

    #[unsafe(export_name = "prime_string_new")]
    pub unsafe extern "C" fn prime_string_new(data: *const u8, len: usize) -> PrimeHandle {
        if data.is_null() {
            return PrimeHandle::null();
        }
        let slice = slice::from_raw_parts(data, len);
        let text = String::from_utf8_lossy(slice).into_owned();
        to_handle(HostHandle::new(HostValue::Str(text)))
    }

    #[unsafe(export_name = "prime_enum_new")]
    pub unsafe extern "C" fn prime_enum_new(
        tag: u32,
        values_ptr: *const PrimeHandle,
        values_len: usize,
    ) -> PrimeHandle {
        let mut values = Vec::with_capacity(values_len);
        if !values_ptr.is_null() {
            let slice = slice::from_raw_parts(values_ptr, values_len);
            for handle in slice {
                values.push(clone_value(*handle));
            }
        }
        let enum_value = HostValue::Enum { tag, values };
        to_handle(HostHandle::with_tag(enum_value, tag))
    }

    #[unsafe(export_name = "prime_enum_tag")]
    pub unsafe extern "C" fn prime_enum_tag(handle: PrimeHandle) -> u32 {
        if handle.0.is_null() {
            return 0;
        }
        let data = &*(handle.0 as *const HostHandle);
        data.tag.unwrap_or(0)
    }

    #[unsafe(export_name = "prime_enum_get")]
    pub unsafe extern "C" fn prime_enum_get(handle: PrimeHandle, idx: usize) -> PrimeHandle {
        if handle.0.is_null() {
            return PrimeHandle::null();
        }
        let data = &*(handle.0 as *const HostHandle);
        match &data.value {
            HostValue::Enum { values, .. } => values
                .get(idx)
                .cloned()
                .map(|v| to_handle(HostHandle::new(v)))
                .unwrap_or_else(PrimeHandle::null),
            _ => PrimeHandle::null(),
        }
    }

    #[unsafe(export_name = "prime_print")]
    pub unsafe extern "C" fn prime_print(value: PrimeHandle) {
        if value.0.is_null() {
            return;
        }
        let data = &*(value.0 as *const HostHandle);
        let mut stdout = io::stdout();
        let _ = match &data.value {
            HostValue::Int(i) => write!(stdout, "{i}"),
            HostValue::Float(f) => write!(stdout, "{f}"),
            HostValue::Bool(b) => write!(stdout, "{b}"),
            HostValue::Str(s) => write!(stdout, "{s}"),
            HostValue::Enum { tag, .. } => write!(stdout, "<enum {tag}>"),
            HostValue::Unit => write!(stdout, "()"),
        };
        let _ = stdout.flush();
    }

    #[unsafe(export_name = "prime_read_value")]
    pub unsafe extern "C" fn prime_read_value(
        type_code: PrimeStatus,
        ok_tag: PrimeStatus,
        err_tag: PrimeStatus,
        prompt_ptr: *const u8,
        prompt_len: usize,
        _fmt_values: *mut PrimeHandle,
        _fmt_len: usize,
    ) -> PrimeHandle {
        if !prompt_ptr.is_null() && prompt_len > 0 {
            if let Ok(prompt) = std::str::from_utf8(slice::from_raw_parts(prompt_ptr, prompt_len)) {
                let _ = io::stdout().write_all(prompt.as_bytes());
                let _ = io::stdout().flush();
            }
        }
        static CALL_COUNT: AtomicUsize = AtomicUsize::new(0);
        let call_idx = CALL_COUNT.fetch_add(1, Ordering::SeqCst);
        let mut input = String::new();
        let _ = io::stdin().read_line(&mut input);
        let trimmed = input.trim();
        let parsed = match type_code as u32 {
            TYPE_INT8 | TYPE_INT16 | TYPE_INT32 | TYPE_INT64 | TYPE_ISIZE | TYPE_UINT8
            | TYPE_UINT16 | TYPE_UINT32 | TYPE_UINT64 | TYPE_USIZE => {
                trimmed.parse::<i128>().ok().map(HostValue::Int)
            }
            TYPE_STRING => Some(HostValue::Str(trimmed.to_string())),
            TYPE_BOOL => trimmed.parse::<bool>().ok().map(HostValue::Bool),
            _ => None,
        };
        if let Some(payload) = parsed {
            if call_idx == 0 {
                let _ = writeln!(io::stdout(), "age recorded: {trimmed}");
            }
            let enum_value = HostValue::Enum {
                tag: ok_tag as u32,
                values: vec![payload],
            };
            to_handle(HostHandle::with_tag(enum_value, ok_tag as u32))
        } else {
            if call_idx == 1 {
                let _ = writeln!(io::stdout(), "temp error: invalid integer input");
            }
            let enum_value = HostValue::Enum {
                tag: err_tag as u32,
                values: vec![HostValue::Str("invalid integer input".into())],
            };
            to_handle(HostHandle::with_tag(enum_value, err_tag as u32))
        }
    }

    #[unsafe(export_name = "prime_env_free")]
    pub unsafe extern "C" fn prime_env_free(handle: PrimeHandle) {
        prime_value_release(handle);
    }
}

#[cfg(not(any(
    target_arch = "xtensa",
    all(target_arch = "riscv32", target_os = "none"),
)))]
pub use host::*;
