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
    #![allow(unused_imports)]
    use core::array;
    use core::cell::UnsafeCell;
    use core::ffi::c_void;
    use core::mem::MaybeUninit;
    use core::ptr;
    use core::sync::atomic::{AtomicBool, Ordering};
    #[cfg(target_arch = "xtensa")]
    use core::sync::atomic::{AtomicU32, AtomicUsize};

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
    #[derive(Clone, Copy, Debug)]
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

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    const fn parse_usize_env(var: Option<&str>, default: usize, min: usize) -> usize {
        match var {
            Some(value) => match parse_usize(value) {
                Some(parsed) if parsed >= min => parsed,
                _ => default,
            },
            None => default,
        }
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    const fn parse_u32_env(var: Option<&str>, default: u32, min: u32) -> u32 {
        match var {
            Some(value) => match parse_usize(value) {
                Some(parsed) if parsed as u32 >= min => parsed as u32,
                _ => default,
            },
            None => default,
        }
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    const fn parse_usize(value: &str) -> Option<usize> {
        let bytes = value.as_bytes();
        let mut idx = 0;
        let mut out: usize = 0;
        while idx < bytes.len() {
            let b = bytes[idx];
            if b < b'0' || b > b'9' {
                return None;
            }
            out = match out.checked_mul(10) {
                Some(v) => v,
                None => return None,
            };
            out = match out.checked_add((b - b'0') as usize) {
                Some(v) => v,
                None => return None,
            };
            idx += 1;
        }
        Some(out)
    }

    // Simple handle pool for tiny, no_std-friendly async/enum support on embedded targets.
    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    const PRIME_INT_SLOTS: usize = 16;
    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    #[derive(Copy, Clone)]
    #[repr(C)]
    struct PrimeInt {
        value: i128,
    }
    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    static mut INT_HANDLES: [MaybeUninit<PrimeInt>; PRIME_INT_SLOTS] =
        [const { MaybeUninit::uninit() }; PRIME_INT_SLOTS];
    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    static INT_HANDLE_NEXT: AtomicUsize = AtomicUsize::new(0);

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn alloc_int(value: i128) -> PrimeHandle {
        let slot = INT_HANDLE_NEXT.fetch_add(1, Ordering::Relaxed) % PRIME_INT_SLOTS;
        unsafe {
            INT_HANDLES[slot].as_mut_ptr().write(PrimeInt { value });
            PrimeHandle(INT_HANDLES.as_mut_ptr().add(slot) as *mut c_void)
        }
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    const PRIME_TASK_SLOTS: usize = parse_usize_env(option_env!("PRIME_RT_TASK_SLOTS"), 8, 1);
    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    const PRIME_CHANNEL_SLOTS: usize =
        parse_usize_env(option_env!("PRIME_RT_CHANNEL_SLOTS"), 32, 1);
    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    const PRIME_CHANNEL_CAP: usize = parse_usize_env(option_env!("PRIME_RT_CHANNEL_CAP"), 16, 1);
    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    const PRIME_CHANNEL_WAITERS: usize = parse_usize_env(
        option_env!("PRIME_RT_CHANNEL_WAITERS"),
        PRIME_TASK_SLOTS * 2,
        1,
    );
    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    const PRIME_RECV_POLL_MS: u32 = parse_u32_env(option_env!("PRIME_RT_RECV_POLL_MS"), 1, 1);

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    #[derive(Debug)]
    struct TaskSlot {
        used: AtomicBool,
        done: AtomicBool,
        deadline_ms: UnsafeCell<i128>,
        result: UnsafeCell<PrimeHandle>,
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    unsafe impl Send for TaskSlot {}
    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    unsafe impl Sync for TaskSlot {}

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    static mut TASK_SLOTS: [MaybeUninit<TaskSlot>; PRIME_TASK_SLOTS] =
        [const { MaybeUninit::uninit() }; PRIME_TASK_SLOTS];
    #[allow(dead_code)]
    static TASK_SLOTS_READY: AtomicBool = AtomicBool::new(false);

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    #[derive(Debug)]
    struct ChannelSlot {
        used: AtomicBool,
        closed: AtomicBool,
        queue: UnsafeCell<[PrimeHandle; PRIME_CHANNEL_CAP]>,
        head: UnsafeCell<usize>,
        tail: UnsafeCell<usize>,
        len: UnsafeCell<usize>,
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    unsafe impl Send for ChannelSlot {}
    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    unsafe impl Sync for ChannelSlot {}

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    static mut CHANNEL_SLOTS: [MaybeUninit<ChannelSlot>; PRIME_CHANNEL_SLOTS] =
        [const { MaybeUninit::uninit() }; PRIME_CHANNEL_SLOTS];
    #[allow(dead_code)]
    static CHANNEL_SLOTS_READY: AtomicBool = AtomicBool::new(false);

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    #[derive(Debug)]
    struct ChannelWaiterSlot {
        used: AtomicBool,
        channel: UnsafeCell<*const ChannelSlot>,
        task: UnsafeCell<*const TaskSlot>,
        deadline_ms: UnsafeCell<i128>,
        has_deadline: AtomicBool,
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    unsafe impl Send for ChannelWaiterSlot {}
    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    unsafe impl Sync for ChannelWaiterSlot {}

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    static mut CHANNEL_WAITERS: [MaybeUninit<ChannelWaiterSlot>; PRIME_CHANNEL_WAITERS] =
        [const { MaybeUninit::uninit() }; PRIME_CHANNEL_WAITERS];
    #[allow(dead_code)]
    static CHANNEL_WAITERS_READY: AtomicBool = AtomicBool::new(false);

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    #[derive(Copy, Clone)]
    #[repr(C)]
    struct PrimeEnum {
        tag: u32,
        value: PrimeHandle,
        len: usize,
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    static mut PRIME_UNIT_HANDLE: PrimeHandle = PrimeHandle::null();

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    static mut ENUM_HANDLES: [Option<PrimeEnum>; 128] = [None; 128];
    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    static ENUM_HANDLE_NEXT: AtomicUsize = AtomicUsize::new(0);
    #[cfg(target_arch = "xtensa")]
    static PRINT_COUNT: AtomicU32 = AtomicU32::new(0);
    #[cfg(target_arch = "xtensa")]
    static STRING_COUNT: AtomicU32 = AtomicU32::new(0);
    #[cfg(target_arch = "xtensa")]
    static ENUM_COUNT: AtomicU32 = AtomicU32::new(0);
    #[cfg(target_arch = "xtensa")]
    static STRING_WRAP_COUNT: AtomicU32 = AtomicU32::new(0);
    #[cfg(target_arch = "xtensa")]
    static STRING_HANDLE_WRAP: AtomicU32 = AtomicU32::new(0);
    #[cfg(target_arch = "xtensa")]
    static CHANNEL_ALLOC_COUNT: AtomicU32 = AtomicU32::new(0);
    #[cfg(target_arch = "xtensa")]
    static CHANNEL_FREE_COUNT: AtomicU32 = AtomicU32::new(0);
    #[cfg(target_arch = "xtensa")]
    static TASK_ALLOC_COUNT: AtomicU32 = AtomicU32::new(0);
    #[cfg(target_arch = "xtensa")]
    static TASK_FREE_COUNT: AtomicU32 = AtomicU32::new(0);

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn init_unit_handle() {
        #[cfg(target_arch = "xtensa")]
        unsafe {
            if PRIME_UNIT_HANDLE.0.is_null() {
                PRIME_UNIT_HANDLE =
                    PrimeHandle(&PRIME_UNIT_HANDLE as *const PrimeHandle as *mut core::ffi::c_void);
            }
        }
        #[cfg(all(target_arch = "riscv32", target_os = "none"))]
        unsafe {
            if PRIME_UNIT_HANDLE.0.is_null() {
                PRIME_UNIT_HANDLE =
                    PrimeHandle(&PRIME_UNIT_HANDLE as *const PrimeHandle as *mut core::ffi::c_void);
            }
        }
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn alloc_enum(tag: u32, value: PrimeHandle, len: usize) -> PrimeHandle {
        unsafe {
            let idx = ENUM_HANDLE_NEXT.fetch_add(1, Ordering::Relaxed) % ENUM_HANDLES.len();
            ENUM_HANDLES[idx] = Some(PrimeEnum { tag, value, len });
            let handle_ptr = ENUM_HANDLES[idx].as_ref().unwrap() as *const PrimeEnum;
            #[cfg(target_arch = "xtensa")]
            {
                ENUM_COUNT.fetch_add(1, Ordering::Relaxed);
            }
            return PrimeHandle(handle_ptr as *mut core::ffi::c_void);
        }
        PrimeHandle::null()
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn alloc_task_slot() -> Option<&'static TaskSlot> {
        ensure_task_slots();
        unsafe {
            for slot in task_slots().iter() {
                if slot
                    .used
                    .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
                    .is_ok()
                {
                    #[cfg(target_arch = "xtensa")]
                    {
                        TASK_ALLOC_COUNT.fetch_add(1, Ordering::Relaxed);
                    }
                    slot.done.store(false, Ordering::SeqCst);
                    *slot.deadline_ms.get() = 0;
                    *slot.result.get() = PrimeHandle::null();
                    return Some(slot);
                }
            }
        }
        None
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn task_from_handle(handle: PrimeHandle) -> Option<&'static TaskSlot> {
        if handle.0.is_null() {
            return None;
        }
        let ptr = handle.0 as *const TaskSlot;
        task_slots().iter().find(|slot| {
            // Reject stale handles for recycled slots.
            slot.used.load(Ordering::SeqCst) && *slot as *const TaskSlot == ptr
        })
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn alloc_channel_slot() -> Option<&'static ChannelSlot> {
        ensure_channel_slots();
        for slot in channel_slots().iter() {
            if slot
                .used
                .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
                .is_ok()
            {
                #[cfg(target_arch = "xtensa")]
                {
                    CHANNEL_ALLOC_COUNT.fetch_add(1, Ordering::Relaxed);
                }
                slot.closed.store(false, Ordering::SeqCst);
                unsafe {
                    *slot.head.get() = 0;
                    *slot.tail.get() = 0;
                    *slot.len.get() = 0;
                    let buf = &mut *slot.queue.get();
                    for elem in buf.iter_mut() {
                        *elem = PrimeHandle::null();
                    }
                }
                return Some(slot);
            }
        }
        None
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn channel_from_handle(handle: PrimeHandle) -> Option<&'static ChannelSlot> {
        if handle.0.is_null() {
            return None;
        }
        let ptr = handle.0 as *const ChannelSlot;
        channel_slots().iter().find(|slot| {
            // Guard against stale handles after a slot is recycled.
            slot.used.load(Ordering::SeqCst) && *slot as *const ChannelSlot == ptr
        })
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn ensure_task_slots() {
        if TASK_SLOTS_READY.load(Ordering::SeqCst) {
            return;
        }
        unsafe {
            for slot in TASK_SLOTS.iter_mut() {
                slot.write(TaskSlot {
                    used: AtomicBool::new(false),
                    done: AtomicBool::new(false),
                    deadline_ms: UnsafeCell::new(0),
                    result: UnsafeCell::new(PrimeHandle::null()),
                });
            }
        }
        TASK_SLOTS_READY.store(true, Ordering::SeqCst);
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn task_slots() -> &'static [TaskSlot; PRIME_TASK_SLOTS] {
        ensure_task_slots();
        unsafe { &*(TASK_SLOTS.as_ptr() as *const [TaskSlot; PRIME_TASK_SLOTS]) }
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn ensure_channel_slots() {
        if CHANNEL_SLOTS_READY.load(Ordering::SeqCst) {
            return;
        }
        unsafe {
            for slot in CHANNEL_SLOTS.iter_mut() {
                slot.write(ChannelSlot {
                    used: AtomicBool::new(false),
                    closed: AtomicBool::new(false),
                    queue: UnsafeCell::new(array::from_fn(|_| PrimeHandle::null())),
                    head: UnsafeCell::new(0),
                    tail: UnsafeCell::new(0),
                    len: UnsafeCell::new(0),
                });
            }
        }
        CHANNEL_SLOTS_READY.store(true, Ordering::SeqCst);
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn channel_slots() -> &'static [ChannelSlot; PRIME_CHANNEL_SLOTS] {
        ensure_channel_slots();
        unsafe { &*(CHANNEL_SLOTS.as_ptr() as *const [ChannelSlot; PRIME_CHANNEL_SLOTS]) }
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn ensure_channel_waiters() {
        if CHANNEL_WAITERS_READY.load(Ordering::SeqCst) {
            return;
        }
        unsafe {
            for slot in CHANNEL_WAITERS.iter_mut() {
                slot.write(ChannelWaiterSlot {
                    used: AtomicBool::new(false),
                    channel: UnsafeCell::new(core::ptr::null()),
                    task: UnsafeCell::new(core::ptr::null()),
                    deadline_ms: UnsafeCell::new(0),
                    has_deadline: AtomicBool::new(false),
                });
            }
        }
        CHANNEL_WAITERS_READY.store(true, Ordering::SeqCst);
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn channel_waiters() -> &'static [ChannelWaiterSlot; PRIME_CHANNEL_WAITERS] {
        ensure_channel_waiters();
        unsafe { &*(CHANNEL_WAITERS.as_ptr() as *const [ChannelWaiterSlot; PRIME_CHANNEL_WAITERS]) }
    }

    // Required FFI entry points used by the compiler (embedded).
    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    #[unsafe(export_name = "prime_value_retain")]
    pub unsafe extern "C" fn prime_value_retain(value: PrimeHandle) -> PrimeHandle {
        value
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    #[unsafe(export_name = "prime_value_release")]
    pub unsafe extern "C" fn prime_value_release(_value: PrimeHandle) {}

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    #[unsafe(export_name = "prime_unit_new")]
    pub unsafe extern "C" fn prime_unit_new() -> PrimeHandle {
        init_unit_handle();
        PRIME_UNIT_HANDLE
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    #[unsafe(export_name = "prime_int_new")]
    pub unsafe extern "C" fn prime_int_new(_value: i128) -> PrimeHandle {
        alloc_int(_value)
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    #[unsafe(export_name = "prime_float_new")]
    pub unsafe extern "C" fn prime_float_new(_value: f64) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    #[unsafe(export_name = "prime_bool_new")]
    pub unsafe extern "C" fn prime_bool_new(_value: bool) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[cfg(not(target_arch = "xtensa"))]
    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    #[unsafe(export_name = "prime_string_new")]
    pub unsafe extern "C" fn prime_string_new(_data: *const u8, _len: usize) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_struct_new")]
    pub unsafe extern "C" fn prime_struct_new(
        _name_ptr: *const u8,
        _name_len: usize,
    ) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    #[unsafe(export_name = "prime_enum_new")]
    pub unsafe extern "C" fn prime_enum_new(
        _values_ptr: *const PrimeHandle,
        _values_len: usize,
        _tag: u32,
    ) -> PrimeHandle {
        let payload = if !_values_ptr.is_null() && _values_len > 0 {
            *_values_ptr
        } else {
            PrimeHandle::null()
        };
        alloc_enum(_tag, payload, _values_len)
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_enum_tag")]
    pub unsafe extern "C" fn prime_enum_tag(handle: PrimeHandle) -> u32 {
        if handle.0.is_null() {
            return 0;
        }
        let enum_ptr = handle.0 as *const PrimeEnum;
        (*enum_ptr).tag
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_enum_get")]
    pub unsafe extern "C" fn prime_enum_get(handle: PrimeHandle, idx: usize) -> PrimeHandle {
        if handle.0.is_null() {
            return PrimeHandle::null();
        }
        let enum_ptr = handle.0 as *const PrimeEnum;
        if idx == 0 && (*enum_ptr).len > 0 {
            return (*enum_ptr).value;
        }
        PrimeHandle::null()
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_reference_new")]
    pub unsafe extern "C" fn prime_reference_new(
        _target: PrimeHandle,
        _mutable: bool,
    ) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_format")]
    pub unsafe extern "C" fn prime_format(
        _fmt_parts_ptr: *const PrimeHandle,
        _fmt_parts_len: usize,
        _fmt_values_ptr: *const PrimeHandle,
        _fmt_values_len: usize,
    ) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_channel_new")]
    pub unsafe extern "C" fn prime_channel_new(
        sender_out: *mut PrimeHandle,
        receiver_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if sender_out.is_null() || receiver_out.is_null() {
            return PrimeStatus::Invalid;
        }
        if let Some(slot) = alloc_channel_slot() {
            let handle = PrimeHandle(slot as *const ChannelSlot as *mut c_void);
            sender_out.write(handle);
            receiver_out.write(handle);
            return PrimeStatus::Ok;
        }
        sender_out.write(PrimeHandle::null());
        receiver_out.write(PrimeHandle::null());
        PrimeStatus::Invalid
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_send")]
    pub unsafe extern "C" fn prime_send(_sender: PrimeHandle, _value: PrimeHandle) -> PrimeStatus {
        let Some(slot) = channel_from_handle(_sender) else {
            return PrimeStatus::Invalid;
        };
        if slot.closed.load(Ordering::SeqCst) {
            return PrimeStatus::Closed;
        }
        let len = unsafe { *slot.len.get() };
        if len >= PRIME_CHANNEL_CAP {
            return PrimeStatus::Invalid;
        }
        let tail = unsafe { *slot.tail.get() };
        unsafe { (*slot.queue.get())[tail] = _value };
        unsafe { *slot.tail.get() = (tail + 1) % PRIME_CHANNEL_CAP };
        unsafe { *slot.len.get() = len + 1 };
        let _ = poll_channel_waiters();
        PrimeStatus::Ok
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_recv")]
    pub unsafe extern "C" fn prime_recv(
        _receiver: PrimeHandle,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if value_out.is_null() {
            return PrimeStatus::Invalid;
        }
        let Some(slot) = channel_from_handle(_receiver) else {
            return PrimeStatus::Invalid;
        };
        match dequeue_channel(slot) {
            ChannelPoll::Item(v) => {
                value_out.write(v);
                PrimeStatus::Ok
            }
            ChannelPoll::Closed => PrimeStatus::Closed,
            ChannelPoll::Empty => PrimeStatus::Invalid,
        }
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_recv_timeout")]
    pub unsafe extern "C" fn prime_recv_timeout(
        _receiver: PrimeHandle,
        _millis: i64,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if value_out.is_null() {
            return PrimeStatus::Invalid;
        }
        let Some(slot) = channel_from_handle(_receiver) else {
            return PrimeStatus::Invalid;
        };
        let start = prime_now_ms();
        let deadline = start.saturating_add(i128::from(_millis.max(0)));
        loop {
            match dequeue_channel(slot) {
                ChannelPoll::Item(v) => {
                    value_out.write(v);
                    return PrimeStatus::Ok;
                }
                ChannelPoll::Closed => return PrimeStatus::Closed,
                ChannelPoll::Empty => {
                    if prime_now_ms() >= deadline {
                        value_out.write(PrimeHandle::null());
                        return PrimeStatus::Invalid;
                    }
                    let _ = poll_channel_waiters();
                    prime_delay_ms(recv_poll_ms());
                }
            }
        }
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_close")]
    pub unsafe extern "C" fn prime_close(_handle: PrimeHandle) -> PrimeStatus {
        if let Some(slot) = channel_from_handle(_handle) {
            slot.closed.store(true, Ordering::SeqCst);
            let _ = poll_channel_waiters();
            // Drop any waiters that were bound to this channel to avoid stale pointers
            // keeping the slot “in use” after we recycle it.
            for waiter in channel_waiters().iter() {
                let channel_ptr = unsafe { *waiter.channel.get() };
                if channel_ptr == slot as *const ChannelSlot {
                    waiter.used.store(false, Ordering::SeqCst);
                }
            }
            #[cfg(target_arch = "xtensa")]
            {
                CHANNEL_FREE_COUNT.fetch_add(1, Ordering::Relaxed);
            }
            unsafe {
                *slot.head.get() = 0;
                *slot.tail.get() = 0;
                *slot.len.get() = 0;
                let buf = &mut *slot.queue.get();
                for elem in buf.iter_mut() {
                    *elem = PrimeHandle::null();
                }
            }
            slot.used.store(false, Ordering::SeqCst);
            return PrimeStatus::Ok;
        }
        PrimeStatus::Invalid
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    enum ChannelPoll {
        Item(PrimeHandle),
        Closed,
        Empty,
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn dequeue_channel(slot: &ChannelSlot) -> ChannelPoll {
        let len = unsafe { *slot.len.get() };
        if len == 0 {
            if slot.closed.load(Ordering::SeqCst) {
                return ChannelPoll::Closed;
            }
            return ChannelPoll::Empty;
        }
        let head = unsafe { *slot.head.get() };
        let value = unsafe { (*slot.queue.get())[head] };
        unsafe { *slot.head.get() = (head + 1) % PRIME_CHANNEL_CAP };
        unsafe { *slot.len.get() = len - 1 };
        ChannelPoll::Item(value)
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn option_handle_from_payload(payload: Option<PrimeHandle>) -> PrimeHandle {
        match payload {
            Some(value) => {
                let mut buf = [value];
                unsafe { prime_enum_new(buf.as_mut_ptr(), 1, 0) }
            }
            None => unsafe { prime_enum_new(core::ptr::null(), 0, 1) },
        }
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn register_channel_waiter(
        channel: &ChannelSlot,
        task: &TaskSlot,
        deadline_ms: Option<i128>,
    ) -> bool {
        for waiter in channel_waiters().iter() {
            if waiter
                .used
                .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
                .is_ok()
            {
                unsafe {
                    *waiter.channel.get() = channel as *const ChannelSlot;
                    *waiter.task.get() = task as *const TaskSlot;
                    *waiter.deadline_ms.get() = deadline_ms.unwrap_or(0);
                }
                waiter
                    .has_deadline
                    .store(deadline_ms.is_some(), Ordering::SeqCst);
                return true;
            }
        }
        false
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn complete_channel_waiter(
        waiter: &ChannelWaiterSlot,
        task_ptr: *const TaskSlot,
        payload: Option<PrimeHandle>,
    ) {
        let Some(task) = (unsafe { task_ptr.as_ref() }) else {
            waiter.used.store(false, Ordering::SeqCst);
            return;
        };
        unsafe {
            *task.result.get() = option_handle_from_payload(payload);
        }
        task.done.store(true, Ordering::SeqCst);
        waiter.used.store(false, Ordering::SeqCst);
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn poll_channel_waiters() -> bool {
        let mut progressed = false;
        for waiter in channel_waiters().iter() {
            if !waiter.used.load(Ordering::SeqCst) {
                continue;
            }
            let channel_ptr = unsafe { *waiter.channel.get() };
            let task_ptr = unsafe { *waiter.task.get() };
            if channel_ptr.is_null() || task_ptr.is_null() {
                waiter.used.store(false, Ordering::SeqCst);
                continue;
            }
            let channel = unsafe { &*channel_ptr };
            match dequeue_channel(channel) {
                ChannelPoll::Item(v) => {
                    complete_channel_waiter(waiter, task_ptr, Some(v));
                    progressed = true;
                }
                ChannelPoll::Closed => {
                    complete_channel_waiter(waiter, task_ptr, None);
                    progressed = true;
                }
                ChannelPoll::Empty => {
                    if waiter.has_deadline.load(Ordering::SeqCst) {
                        let deadline = unsafe { *waiter.deadline_ms.get() };
                        if deadline > 0 && unsafe { prime_now_ms() } >= deadline {
                            complete_channel_waiter(waiter, task_ptr, None);
                            progressed = true;
                        }
                    }
                }
            }
        }
        progressed
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_slice_new")]
    pub unsafe extern "C" fn prime_slice_new() -> PrimeHandle {
        PrimeHandle::null()
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_slice_push_handle")]
    pub unsafe extern "C" fn prime_slice_push_handle(
        _slice: PrimeHandle,
        _value: PrimeHandle,
    ) -> PrimeStatus {
        PrimeStatus::Invalid
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_slice_push")]
    pub unsafe extern "C" fn prime_slice_push(
        _slice: PrimeHandle,
        _value: PrimeHandle,
    ) -> PrimeStatus {
        PrimeStatus::Invalid
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_slice_len_handle")]
    pub unsafe extern "C" fn prime_slice_len_handle(_slice: PrimeHandle) -> usize {
        0
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_slice_get_handle")]
    pub unsafe extern "C" fn prime_slice_get_handle(
        _slice: PrimeHandle,
        _idx: usize,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if !value_out.is_null() {
            value_out.write(PrimeHandle::null());
        }
        PrimeStatus::Invalid
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_slice_remove_handle")]
    pub unsafe extern "C" fn prime_slice_remove_handle(
        _slice: PrimeHandle,
        _idx: usize,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if !value_out.is_null() {
            value_out.write(PrimeHandle::null());
        }
        PrimeStatus::Invalid
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_map_new")]
    pub unsafe extern "C" fn prime_map_new() -> PrimeHandle {
        PrimeHandle::null()
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_map_len_handle")]
    pub unsafe extern "C" fn prime_map_len_handle(_map: PrimeHandle) -> usize {
        0
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_map_insert_handle")]
    pub unsafe extern "C" fn prime_map_insert_handle(
        _map: PrimeHandle,
        _key_ptr: *const u8,
        _key_len: usize,
        _value: PrimeHandle,
    ) -> PrimeStatus {
        PrimeStatus::Invalid
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_map_insert")]
    pub unsafe extern "C" fn prime_map_insert(
        _map: PrimeHandle,
        _key_ptr: *const u8,
        _key_len: usize,
        _value: PrimeHandle,
    ) -> PrimeStatus {
        PrimeStatus::Invalid
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_map_get_handle")]
    pub unsafe extern "C" fn prime_map_get_handle(
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

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_map_get")]
    pub unsafe extern "C" fn prime_map_get(
        map: PrimeHandle,
        key_ptr: *const u8,
        key_len: usize,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        prime_map_get_handle(map, key_ptr, key_len, value_out)
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_map_remove_handle")]
    pub unsafe extern "C" fn prime_map_remove_handle(
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

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_map_remove")]
    pub unsafe extern "C" fn prime_map_remove_handle_alias(
        map: PrimeHandle,
        key_ptr: *const u8,
        key_len: usize,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        prime_map_remove_handle(map, key_ptr, key_len, value_out)
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_map_entry_handle")]
    pub unsafe extern "C" fn prime_map_entry_handle(
        _map: PrimeHandle,
        _idx: usize,
        key_out: *mut PrimeHandle,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if !key_out.is_null() {
            key_out.write(PrimeHandle::null());
        }
        if !value_out.is_null() {
            value_out.write(PrimeHandle::null());
        }
        PrimeStatus::Invalid
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_value_tag")]
    pub unsafe extern "C" fn prime_value_tag(_handle: PrimeHandle) -> PrimeTag {
        PrimeTag::Unit
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_value_as_int")]
    pub unsafe extern "C" fn prime_value_as_int(_handle: PrimeHandle) -> i128 {
        if _handle.0.is_null() {
            return 0;
        }
        let int_ptr = _handle.0 as *const PrimeInt;
        // Safety: handles are produced by alloc_int and point into INT_HANDLES backing storage.
        unsafe { (*int_ptr).value }
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_value_as_bool")]
    pub unsafe extern "C" fn prime_value_as_bool(_handle: PrimeHandle) -> bool {
        false
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_value_as_float")]
    pub unsafe extern "C" fn prime_value_as_float(_handle: PrimeHandle) -> f64 {
        0.0
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_value_as_string")]
    pub unsafe extern "C" fn prime_value_as_string(
        _handle: PrimeHandle,
        ptr_out: *mut *const u8,
        len_out: *mut usize,
    ) -> PrimeStatus {
        if !ptr_out.is_null() {
            ptr_out.write(core::ptr::null());
        }
        if !len_out.is_null() {
            len_out.write(0);
        }
        PrimeStatus::Invalid
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_reference_read")]
    pub unsafe extern "C" fn prime_reference_read(target: PrimeHandle) -> PrimeHandle {
        target
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_reference_write")]
    pub unsafe extern "C" fn prime_reference_write(
        _target: PrimeHandle,
        _value: PrimeHandle,
    ) -> PrimeStatus {
        // Embedded references are opaque; writes are not supported yet.
        PrimeStatus::Invalid
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_spawn")]
    pub unsafe extern "C" fn prime_spawn(
        entry: Option<unsafe extern "C" fn(arg: PrimeHandle) -> PrimeHandle>,
        arg: PrimeHandle,
        handle_out: *mut PrimeHandle,
    ) -> PrimeHandle {
        let handle = prime_task_new(entry, arg);
        if !handle_out.is_null() {
            handle_out.write(handle);
        }
        handle
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_task_new")]
    pub unsafe extern "C" fn prime_task_new(
        _entry: Option<unsafe extern "C" fn(arg: PrimeHandle) -> PrimeHandle>,
        _arg: PrimeHandle,
    ) -> PrimeHandle {
        let Some(entry) = _entry else {
            return PrimeHandle::null();
        };
        let Some(slot) = alloc_task_slot() else {
            return PrimeHandle::null();
        };
        let result = entry(_arg);
        unsafe { *slot.result.get() = result };
        slot.done.store(true, Ordering::SeqCst);
        PrimeHandle(slot as *const TaskSlot as *mut c_void)
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_task_poll")]
    pub unsafe extern "C" fn prime_task_poll(
        _task: PrimeHandle,
        result_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if result_out.is_null() {
            return PrimeStatus::Invalid;
        }
        let Some(slot) = task_from_handle(_task) else {
            return PrimeStatus::Invalid;
        };
        if !slot.done.load(Ordering::SeqCst) {
            let _ = poll_channel_waiters();
        }
        if !slot.done.load(Ordering::SeqCst) {
            let deadline = unsafe { *slot.deadline_ms.get() };
            if deadline > 0 {
                let now = unsafe { prime_now_ms() };
                if now >= deadline {
                    slot.done.store(true, Ordering::SeqCst);
                }
            }
        }
        if !slot.done.load(Ordering::SeqCst) {
            prime_delay_ms(recv_poll_ms());
            return PrimeStatus::Invalid;
        }
        let value = unsafe { *slot.result.get() };
        unsafe {
            result_out.write(value);
        }
        slot.used.store(false, Ordering::SeqCst);
        slot.done.store(false, Ordering::SeqCst);
        unsafe {
            *slot.result.get() = PrimeHandle::null();
            *slot.deadline_ms.get() = 0;
        }
        #[cfg(target_arch = "xtensa")]
        {
            TASK_FREE_COUNT.fetch_add(1, Ordering::Relaxed);
        }
        PrimeStatus::Ok
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_task_wake")]
    pub unsafe extern "C" fn prime_task_wake(
        _task: PrimeHandle,
        _value: PrimeHandle,
    ) -> PrimeStatus {
        if let Some(slot) = task_from_handle(_task) {
            unsafe { *slot.result.get() = _value };
            slot.done.store(true, Ordering::SeqCst);
            return PrimeStatus::Ok;
        }
        PrimeStatus::Invalid
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_sleep_task")]
    pub unsafe extern "C" fn prime_sleep_task(_millis: i64) -> PrimeHandle {
        // For now, block the current thread/task; Xtensa ROM delay is cheap and avoids
        // tight polling when timers are unavailable.
        prime_delay_ms(_millis as i32);
        let Some(slot) = alloc_task_slot() else {
            return PrimeHandle::null();
        };
        *slot.deadline_ms.get() = prime_now_ms();
        *slot.result.get() = prime_unit_new();
        slot.done.store(true, Ordering::SeqCst);
        PrimeHandle(slot as *const TaskSlot as *mut c_void)
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_recv_task")]
    pub unsafe extern "C" fn prime_recv_task(_receiver: PrimeHandle) -> PrimeHandle {
        let Some(slot) = alloc_task_slot() else {
            return PrimeHandle::null();
        };
        if let Some(ch) = channel_from_handle(_receiver) {
            match dequeue_channel(ch) {
                ChannelPoll::Item(v) => {
                    unsafe {
                        *slot.result.get() = option_handle_from_payload(Some(v));
                    }
                    slot.done.store(true, Ordering::SeqCst);
                }
                ChannelPoll::Closed => {
                    unsafe {
                        *slot.result.get() = option_handle_from_payload(None);
                    }
                    slot.done.store(true, Ordering::SeqCst);
                }
                ChannelPoll::Empty => {
                    if !register_channel_waiter(ch, slot, None) {
                        unsafe {
                            *slot.result.get() = option_handle_from_payload(None);
                        }
                        slot.done.store(true, Ordering::SeqCst);
                    }
                }
            }
        } else {
            unsafe {
                *slot.result.get() = option_handle_from_payload(None);
            }
            slot.done.store(true, Ordering::SeqCst);
        }
        PrimeHandle(slot as *const TaskSlot as *mut c_void)
    }

    #[repr(C)]
    #[derive(Copy, Clone)]
    #[allow(dead_code)]
    struct PrimeString {
        ptr: *const u8,
        len: usize,
    }

    // String ring buffer for UART prints; big enough for steady logging and
    // wraps safely. Each entry stores a copy so callers can pass stack/data
    // pointers without lifetime issues.
    #[cfg(target_arch = "xtensa")]
    const STR_STORAGE_SIZE: usize = 8192;
    #[cfg(target_arch = "xtensa")]
    const STR_HANDLES_MAX: usize = 64;
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
        if data.is_null() {
            return PrimeHandle::null();
        }
        STRING_COUNT.fetch_add(1, Ordering::Relaxed);
        let slot = {
            let mut next = STR_HANDLE_OFF.load(Ordering::Relaxed);
            if next >= STR_HANDLES_MAX {
                next = 0;
                STRING_HANDLE_WRAP.fetch_add(1, Ordering::Relaxed);
            }
            STR_HANDLE_OFF.store(next + 1, Ordering::Relaxed);
            next
        };
        let start = {
            let mut off = STR_STORAGE_OFF.load(Ordering::Relaxed);
            let need = len.saturating_add(1); // room for null
            if off + need >= STR_STORAGE_SIZE {
                off = 0;
                STRING_WRAP_COUNT.fetch_add(1, Ordering::Relaxed);
            }
            STR_STORAGE_OFF.store(off + need, Ordering::Relaxed);
            off
        };
        let avail = STR_STORAGE_SIZE - start;
        let copy_len = core::cmp::min(len, avail.saturating_sub(1));
        let dst = STR_STORAGE.as_mut_ptr().add(start);
        core::ptr::copy_nonoverlapping(data, dst, copy_len);
        core::ptr::write(dst.add(copy_len), 0);
        STR_HANDLES[slot] = PrimeString {
            ptr: dst as *const u8,
            len: copy_len,
        };
        PrimeHandle(STR_HANDLES.as_mut_ptr().add(slot) as *mut c_void)
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_int_to_string(value: i128) -> PrimeHandle {
        // Render i128 into decimal string using the same ring buffer as prime_string_new.
        let mut buf = [0u8; 48];
        let mut idx = buf.len();
        let neg = value < 0;
        let mut n = if neg {
            value.wrapping_neg() as u128
        } else {
            value as u128
        };
        if n == 0 {
            idx -= 1;
            buf[idx] = b'0';
        } else {
            while n > 0 {
                let digit = (n % 10) as u8;
                idx -= 1;
                buf[idx] = b'0' + digit;
                n /= 10;
            }
        }
        if neg {
            idx -= 1;
            buf[idx] = b'-';
        }
        prime_string_new(buf.as_ptr().add(idx), buf.len() - idx)
    }

    #[cfg(any(
        not(target_arch = "xtensa"),
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_int_to_string(_value: i128) -> PrimeHandle {
        // Host stub: ints are printed directly by host runtime, so this is unused.
        PrimeHandle::null()
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_print(value: PrimeHandle) {
        if value.0.is_null() {
            return;
        }
        let s = &*(value.0 as *const PrimeString);
        #[cfg(target_arch = "xtensa")]
        {
            let count = PRINT_COUNT.fetch_add(1, Ordering::Relaxed).wrapping_add(1);
            ets_printf(b"%s\0".as_ptr(), s.ptr);
            let should_log = count % 64 == 0
                && s.len > 0
                && unsafe { *s.ptr.add(s.len.saturating_sub(1)) == b'\n' };
            if should_log {
                log_stats_snapshot();
            }
        }
        #[cfg(not(target_arch = "xtensa"))]
        let _ = s;
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_now_ms")]
    pub unsafe extern "C" fn prime_now_ms() -> i128 {
        #[cfg(target_arch = "xtensa")]
        {
            static LAST_CYCLES: AtomicU32 = AtomicU32::new(0);
            static ACCUM_MS: AtomicU32 = AtomicU32::new(0);
            static REM_CYCLES: AtomicU32 = AtomicU32::new(0);
            let cycles_per_ms = cycles_per_ms();
            let cycles: u32 = xthal_get_ccount();
            let last = LAST_CYCLES.swap(cycles, Ordering::Relaxed);
            let delta = cycles.wrapping_sub(last);
            let total = delta.wrapping_add(REM_CYCLES.swap(0, Ordering::Relaxed));
            let inc = total / cycles_per_ms;
            let rem = total % cycles_per_ms;
            if rem > 0 {
                REM_CYCLES.store(rem, Ordering::Relaxed);
            }
            if inc > 0 {
                ACCUM_MS.fetch_add(inc, Ordering::Relaxed);
            }
            return ACCUM_MS.load(Ordering::Relaxed) as i128;
        }
        #[cfg(not(target_arch = "xtensa"))]
        {
            0
        }
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
        fn xthal_get_ccount() -> u32;
    }

    #[cfg(target_arch = "xtensa")]
    fn cycles_per_ms() -> u32 {
        use core::sync::atomic::{AtomicU32, Ordering};

        static CYCLES_PER_MS: AtomicU32 = AtomicU32::new(0);
        let cached = CYCLES_PER_MS.load(Ordering::Relaxed);
        if cached != 0 {
            return cached;
        }

        // Calibrate against ROM delay to match boards running at lower CPU clocks (e.g., 80MHz).
        let start = unsafe { xthal_get_ccount() };
        unsafe { ets_delay_us(1000) }; // 1 ms reference
        let delta = unsafe { xthal_get_ccount() }.wrapping_sub(start);
        let fallback = unsafe { prime_clock_hz_get().max(1) / 1000 };
        // Trust the measurement so slower clock configs (e.g., 80MHz) don't get overestimated.
        let final_cycles = if delta == 0 { fallback } else { delta };
        CYCLES_PER_MS.store(final_cycles, Ordering::Relaxed);
        final_cycles
    }

    #[cfg(target_arch = "xtensa")]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_clock_hz_get() -> u32 {
        240_000_000
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
    #[allow(dead_code)]
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

    #[inline]
    #[cfg(target_arch = "xtensa")]
    fn calibrated_delay_ms(ms: u32) {
        // Simple busy loop calibrated off CPU frequency; avoids ROM deps.
        let cycles_per_ms = cycles_per_ms();
        let total_cycles = cycles_per_ms.saturating_mul(ms);
        // Read current cycle count and spin until target is reached (handles wrap).
        let start = unsafe { xthal_get_ccount() };
        loop {
            let now = unsafe { xthal_get_ccount() };
            let elapsed = now.wrapping_sub(start);
            if elapsed as u64 >= total_cycles as u64 {
                break;
            }
            core::hint::spin_loop();
        }
    }

    #[inline]
    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    fn recv_poll_ms() -> i32 {
        PRIME_RECV_POLL_MS as i32
    }

    #[allow(dead_code)]
    static LOG_ONCE: AtomicBool = AtomicBool::new(false);
    #[allow(dead_code)]
    static WATCHDOGS_DISABLED: AtomicBool = AtomicBool::new(false);
    #[cfg(target_arch = "xtensa")]
    static RESET_REASON: AtomicU32 = AtomicU32::new(0);
    #[cfg(target_arch = "xtensa")]
    extern "C" {
        fn rtc_get_reset_reason(cpu: i32) -> u32;
    }

    #[inline]
    #[allow(dead_code)]
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
    #[allow(dead_code)]
    fn log_hello_once() {
        if LOG_ONCE.swap(true, Ordering::Relaxed) {
            return;
        }
        disable_watchdogs_once();
        #[cfg(target_arch = "xtensa")]
        unsafe {
            // CPU 0 reset reason (matches IDF's esp_reset_reason()).
            let reason = rtc_get_reset_reason(0);
            RESET_REASON.store(reason, Ordering::Relaxed);
            ets_printf(b"[rt] reset_reason=%u\n\0".as_ptr(), reason);
        }
        // Keep placeholder for optional boot banner; intentionally disabled.
        #[cfg(target_arch = "xtensa")]
        if false {
            unsafe {
                ets_printf(b"hello esp32 world\n\0".as_ptr());
            }
        }
    }

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    #[unsafe(export_name = "prime_reset_reason_raw")]
    pub unsafe extern "C" fn prime_reset_reason() -> i32 {
        RESET_REASON.load(Ordering::Relaxed) as i32
    }
    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none")
    ))]
    #[unsafe(export_name = "prime_reset_reason")]
    pub unsafe extern "C" fn prime_reset_reason_alias() -> i32 {
        prime_reset_reason()
    }

    #[cfg(any(
        not(target_arch = "xtensa"),
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_reset_reason_raw")]
    pub unsafe extern "C" fn prime_reset_reason_host() -> i32 {
        0
    }
    #[cfg(any(
        not(target_arch = "xtensa"),
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[unsafe(export_name = "prime_reset_reason")]
    pub unsafe extern "C" fn prime_reset_reason_host_alias() -> i32 {
        0
    }

    // Register offsets for classic ESP32 (Xtensa). Kept tiny so we don't pull in IDF.
    #[cfg(target_arch = "xtensa")]
    const DR_REG_GPIO_BASE: u32 = 0x3ff44000;
    #[cfg(target_arch = "xtensa")]
    const GPIO_OUT_W1TS_REG: *mut u32 = (DR_REG_GPIO_BASE + 0x0008) as *mut u32;
    #[cfg(target_arch = "xtensa")]
    const GPIO_OUT_W1TC_REG: *mut u32 = (DR_REG_GPIO_BASE + 0x000c) as *mut u32;
    #[cfg(target_arch = "xtensa")]
    const GPIO_ENABLE_W1TS_REG: *mut u32 = (DR_REG_GPIO_BASE + 0x0024) as *mut u32;

    #[cfg(target_arch = "xtensa")]
    const DR_REG_IO_MUX_BASE: u32 = 0x3ff49000;
    #[cfg(target_arch = "xtensa")]
    // IO mux registers for common LED pins (2, 4, 5).
    const IO_MUX_GPIO2_REG: *mut u32 = (DR_REG_IO_MUX_BASE + 0x40) as *mut u32;
    #[cfg(target_arch = "xtensa")]
    const IO_MUX_GPIO4_REG: *mut u32 = (DR_REG_IO_MUX_BASE + 0x48) as *mut u32;
    #[cfg(target_arch = "xtensa")]
    const IO_MUX_GPIO5_REG: *mut u32 = (DR_REG_IO_MUX_BASE + 0x4c) as *mut u32;
    #[cfg(target_arch = "xtensa")]
    const MCU_SEL_MASK: u32 = 0b111 << 12;
    #[cfg(target_arch = "xtensa")]
    const FUNC_GPIO: u32 = 0b010;

    #[cfg(target_arch = "xtensa")]
    #[inline]
    fn log_stats_snapshot() {
        let pc = PRINT_COUNT.load(Ordering::Relaxed);
        let sc = STRING_COUNT.load(Ordering::Relaxed);
        let ec = ENUM_COUNT.load(Ordering::Relaxed);
        let ca = CHANNEL_ALLOC_COUNT.load(Ordering::Relaxed);
        let cf = CHANNEL_FREE_COUNT.load(Ordering::Relaxed);
        let ta = TASK_ALLOC_COUNT.load(Ordering::Relaxed);
        let tf = TASK_FREE_COUNT.load(Ordering::Relaxed);
        let sw = STRING_WRAP_COUNT.load(Ordering::Relaxed);
        let shw = STRING_HANDLE_WRAP.load(Ordering::Relaxed);
        let off = STR_STORAGE_OFF.load(Ordering::Relaxed) as u32;
        unsafe {
            ets_printf(
                b"[rt] pc=%u sc=%u ec=%u ca=%u cf=%u ta=%u tf=%u sw=%u shw=%u off=%u\n\0".as_ptr(),
                pc,
                sc,
                ec,
                ca,
                cf,
                ta,
                tf,
                sw,
                shw,
                off,
            );
        }
    }

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

    #[cfg(target_arch = "xtensa")]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_delay_ms(ms: i32) -> PrimeStatus {
        log_hello_once();
        let millis = if ms < 0 { 0 } else { ms as u32 };
        calibrated_delay_ms(millis);
        PrimeStatus::Ok
    }

    #[cfg(target_arch = "xtensa")]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_pin_mode(pin: i32, mode: i32) -> PrimeStatus {
        log_hello_once();
        // Mode 1 -> output (matches demo usage).
        if mode != 1 {
            return PrimeStatus::Invalid;
        }
        if pin < 0 || pin > 33 {
            return PrimeStatus::Invalid;
        }
        route_gpio(pin);

        // Enable output driver for the pin.
        let mask = 1u32 << (pin as u32);
        core::ptr::write_volatile(GPIO_ENABLE_W1TS_REG, mask);
        PrimeStatus::Ok
    }

    #[cfg(target_arch = "xtensa")]
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn prime_digital_write(pin: i32, level: i32) -> PrimeStatus {
        if pin < 0 || pin > 33 {
            return PrimeStatus::Invalid;
        }
        route_gpio(pin);
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
    fn route_gpio(pin: i32) {
        let reg = match pin {
            2 => Some(IO_MUX_GPIO2_REG),
            4 => Some(IO_MUX_GPIO4_REG),
            5 => Some(IO_MUX_GPIO5_REG),
            _ => None,
        };
        if let Some(reg_ptr) = reg {
            let mut val = unsafe { core::ptr::read_volatile(reg_ptr) };
            val &= !MCU_SEL_MASK;
            val |= FUNC_GPIO << 12;
            unsafe { core::ptr::write_volatile(reg_ptr, val) };
        }
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

    #[cfg(any(
        target_arch = "xtensa",
        all(target_arch = "riscv32", target_os = "none"),
    ))]
    #[panic_handler]
    fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
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
    use std::collections::BTreeMap;
    use std::ffi::c_void;
    use std::io::{self, Write};
    use std::slice;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::mpsc;
    use std::sync::{Arc, Condvar, Mutex};
    use std::thread;

    pub use super::embedded::{
        PrimeHandle, PrimeStatus, PrimeTag, TYPE_BOOL, TYPE_FLOAT32, TYPE_FLOAT64, TYPE_INT8,
        TYPE_INT16, TYPE_INT32, TYPE_INT64, TYPE_ISIZE, TYPE_RUNE, TYPE_STRING, TYPE_UINT8,
        TYPE_UINT16, TYPE_UINT32, TYPE_UINT64, TYPE_USIZE,
    };

    unsafe extern "C" {
        fn fflush(stream: *mut std::ffi::c_void) -> i32;
    }

    #[derive(Clone, Debug)]
    enum HostValue {
        Unit,
        Int(i128),
        Float(f64),
        Bool(bool),
        Str(String),
        Enum { tag: u32, values: Vec<HostValue> },
        Slice(Vec<PrimeHandle>),
        Map(BTreeMap<String, PrimeHandle>),
        Sender(mpsc::Sender<PrimeHandle>),
        Receiver(Arc<Mutex<mpsc::Receiver<PrimeHandle>>>),
        Task(Arc<HostTaskState>),
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
        match &data.value {
            HostValue::Receiver(rx) => HostValue::Receiver(rx.clone()),
            HostValue::Slice(items) => HostValue::Slice(items.clone()),
            HostValue::Map(map) => HostValue::Map(map.clone()),
            HostValue::Sender(tx) => HostValue::Sender(tx.clone()),
            other => other.clone(),
        }
    }

    #[unsafe(export_name = "prime_channel_new")]
    pub unsafe extern "C" fn prime_channel_new(
        sender_out: *mut PrimeHandle,
        receiver_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if sender_out.is_null() || receiver_out.is_null() {
            return PrimeStatus::Invalid;
        }
        let (tx, rx) = mpsc::channel::<PrimeHandle>();
        let sender = to_handle(HostHandle::new(HostValue::Sender(tx)));
        let receiver = to_handle(HostHandle::new(HostValue::Receiver(Arc::new(Mutex::new(
            rx,
        )))));
        sender_out.write(sender);
        receiver_out.write(receiver);
        PrimeStatus::Ok
    }

    #[unsafe(export_name = "prime_send")]
    pub unsafe extern "C" fn prime_send(sender: PrimeHandle, value: PrimeHandle) -> PrimeStatus {
        if sender.0.is_null() {
            return PrimeStatus::Invalid;
        }
        let data = &*(sender.0 as *const HostHandle);
        let HostValue::Sender(tx) = &data.value else {
            return PrimeStatus::Invalid;
        };
        tx.send(prime_value_retain(value))
            .map(|_| PrimeStatus::Ok)
            .unwrap_or(PrimeStatus::Closed)
    }

    #[unsafe(export_name = "prime_recv")]
    pub unsafe extern "C" fn prime_recv(
        receiver: PrimeHandle,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if receiver.0.is_null() || value_out.is_null() {
            return PrimeStatus::Invalid;
        }
        let data = &*(receiver.0 as *const HostHandle);
        let HostValue::Receiver(rx) = &data.value else {
            return PrimeStatus::Invalid;
        };
        let outcome = rx.lock().map_err(|_| PrimeStatus::Invalid);
        if let Ok(guard) = outcome {
            match guard.recv() {
                Ok(handle) => {
                    value_out.write(handle);
                    return PrimeStatus::Ok;
                }
                Err(_) => return PrimeStatus::Closed,
            }
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_recv_timeout")]
    pub unsafe extern "C" fn prime_recv_timeout(
        receiver: PrimeHandle,
        millis: i64,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if receiver.0.is_null() || value_out.is_null() {
            return PrimeStatus::Invalid;
        }
        let data = &*(receiver.0 as *const HostHandle);
        let HostValue::Receiver(rx) = &data.value else {
            return PrimeStatus::Invalid;
        };
        let timeout = if millis < 0 {
            std::time::Duration::from_millis(0)
        } else {
            std::time::Duration::from_millis(millis as u64)
        };
        let outcome = rx.lock().map_err(|_| PrimeStatus::Invalid);
        if let Ok(guard) = outcome {
            match guard.recv_timeout(timeout) {
                Ok(handle) => {
                    value_out.write(handle);
                    return PrimeStatus::Ok;
                }
                Err(mpsc::RecvTimeoutError::Timeout) => return PrimeStatus::Invalid,
                Err(_) => return PrimeStatus::Closed,
            }
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_close")]
    pub unsafe extern "C" fn prime_close(handle: PrimeHandle) -> PrimeStatus {
        if handle.0.is_null() {
            return PrimeStatus::Invalid;
        }
        let data = &*(handle.0 as *const HostHandle);
        if let HostValue::Sender(tx) = &data.value {
            drop(tx.clone());
            return PrimeStatus::Ok;
        }
        if let HostValue::Receiver(_) = &data.value {
            return PrimeStatus::Ok;
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_slice_new")]
    pub unsafe extern "C" fn prime_slice_new() -> PrimeHandle {
        to_handle(HostHandle::new(HostValue::Slice(Vec::new())))
    }

    #[unsafe(export_name = "prime_slice_push_handle")]
    pub unsafe extern "C" fn prime_slice_push_handle(
        slice: PrimeHandle,
        value: PrimeHandle,
    ) -> PrimeStatus {
        if slice.0.is_null() {
            return PrimeStatus::Invalid;
        }
        let data = &mut *(slice.0 as *mut HostHandle);
        if let HostValue::Slice(items) = &mut data.value {
            items.push(prime_value_retain(value));
            return PrimeStatus::Ok;
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_slice_push")]
    pub unsafe extern "C" fn prime_slice_push(
        slice: PrimeHandle,
        value: PrimeHandle,
    ) -> PrimeStatus {
        prime_slice_push_handle(slice, value)
    }

    #[unsafe(export_name = "prime_slice_len_handle")]
    pub unsafe extern "C" fn prime_slice_len_handle(slice: PrimeHandle) -> usize {
        if slice.0.is_null() {
            return 0;
        }
        let data = &*(slice.0 as *const HostHandle);
        if let HostValue::Slice(items) = &data.value {
            return items.len();
        }
        0
    }

    #[unsafe(export_name = "prime_slice_get_handle")]
    pub unsafe extern "C" fn prime_slice_get_handle(
        slice: PrimeHandle,
        idx: usize,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if slice.0.is_null() || value_out.is_null() {
            return PrimeStatus::Invalid;
        }
        let data = &*(slice.0 as *const HostHandle);
        if let HostValue::Slice(items) = &data.value {
            if let Some(handle) = items.get(idx) {
                value_out.write(prime_value_retain(*handle));
                return PrimeStatus::Ok;
            }
            return PrimeStatus::Closed;
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_slice_remove_handle")]
    pub unsafe extern "C" fn prime_slice_remove_handle(
        slice: PrimeHandle,
        idx: usize,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if slice.0.is_null() || value_out.is_null() {
            return PrimeStatus::Invalid;
        }
        let data = &mut *(slice.0 as *mut HostHandle);
        if let HostValue::Slice(items) = &mut data.value {
            if idx < items.len() {
                let removed = items.remove(idx);
                value_out.write(removed);
                return PrimeStatus::Ok;
            }
            return PrimeStatus::Closed;
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_map_new")]
    pub unsafe extern "C" fn prime_map_new() -> PrimeHandle {
        to_handle(HostHandle::new(HostValue::Map(BTreeMap::new())))
    }

    #[unsafe(export_name = "prime_map_len_handle")]
    pub unsafe extern "C" fn prime_map_len_handle(map: PrimeHandle) -> usize {
        if map.0.is_null() {
            return 0;
        }
        let data = &*(map.0 as *const HostHandle);
        if let HostValue::Map(entries) = &data.value {
            return entries.len();
        }
        0
    }

    #[unsafe(export_name = "prime_map_insert_handle")]
    pub unsafe extern "C" fn prime_map_insert_handle(
        map: PrimeHandle,
        key_ptr: *const u8,
        key_len: usize,
        value: PrimeHandle,
    ) -> PrimeStatus {
        if map.0.is_null() || key_ptr.is_null() {
            return PrimeStatus::Invalid;
        }
        let data = &mut *(map.0 as *mut HostHandle);
        if let HostValue::Map(entries) = &mut data.value {
            let key = std::str::from_utf8(slice::from_raw_parts(key_ptr, key_len))
                .unwrap_or_default()
                .to_string();
            entries.insert(key, prime_value_retain(value));
            return PrimeStatus::Ok;
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_map_insert")]
    pub unsafe extern "C" fn prime_map_insert(
        map: PrimeHandle,
        key_ptr: *const u8,
        key_len: usize,
        value: PrimeHandle,
    ) -> PrimeStatus {
        prime_map_insert_handle(map, key_ptr, key_len, value)
    }

    #[unsafe(export_name = "prime_map_get_handle")]
    pub unsafe extern "C" fn prime_map_get_handle(
        map: PrimeHandle,
        key_ptr: *const u8,
        key_len: usize,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if map.0.is_null() || key_ptr.is_null() || value_out.is_null() {
            return PrimeStatus::Invalid;
        }
        let data = &*(map.0 as *const HostHandle);
        if let HostValue::Map(entries) = &data.value {
            let key =
                std::str::from_utf8(slice::from_raw_parts(key_ptr, key_len)).unwrap_or_default();
            if let Some(val) = entries.get(key) {
                value_out.write(prime_value_retain(*val));
                return PrimeStatus::Ok;
            }
            return PrimeStatus::Closed;
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_map_get")]
    pub unsafe extern "C" fn prime_map_get(
        map: PrimeHandle,
        key_ptr: *const u8,
        key_len: usize,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        prime_map_get_handle(map, key_ptr, key_len, value_out)
    }

    #[unsafe(export_name = "prime_map_remove_handle")]
    pub unsafe extern "C" fn prime_map_remove_handle(
        map: PrimeHandle,
        key_ptr: *const u8,
        key_len: usize,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if map.0.is_null() || key_ptr.is_null() || value_out.is_null() {
            return PrimeStatus::Invalid;
        }
        let data = &mut *(map.0 as *mut HostHandle);
        if let HostValue::Map(entries) = &mut data.value {
            let key = std::str::from_utf8(slice::from_raw_parts(key_ptr, key_len))
                .unwrap_or_default()
                .to_string();
            if let Some(val) = entries.remove(&key) {
                value_out.write(val);
                return PrimeStatus::Ok;
            }
            return PrimeStatus::Closed;
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_map_remove")]
    pub unsafe extern "C" fn prime_map_remove_handle_alias(
        map: PrimeHandle,
        key_ptr: *const u8,
        key_len: usize,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        prime_map_remove_handle(map, key_ptr, key_len, value_out)
    }

    #[unsafe(export_name = "prime_map_entry_handle")]
    pub unsafe extern "C" fn prime_map_entry_handle(
        map: PrimeHandle,
        idx: usize,
        key_out: *mut PrimeHandle,
        value_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if map.0.is_null() || key_out.is_null() || value_out.is_null() {
            return PrimeStatus::Invalid;
        }
        let data = &*(map.0 as *const HostHandle);
        let HostValue::Map(entries) = &data.value else {
            return PrimeStatus::Invalid;
        };
        if let Some((k, v)) = entries.iter().nth(idx) {
            let key_handle = to_handle(HostHandle::new(HostValue::Str(k.clone())));
            key_out.write(key_handle);
            value_out.write(prime_value_retain(*v));
            return PrimeStatus::Ok;
        }
        PrimeStatus::Closed
    }

    #[unsafe(export_name = "prime_reference_new")]
    pub unsafe extern "C" fn prime_reference_new(
        target: PrimeHandle,
        _mutable: bool,
    ) -> PrimeHandle {
        prime_value_retain(target)
    }

    #[unsafe(export_name = "prime_struct_new")]
    pub unsafe extern "C" fn prime_struct_new(
        _name_ptr: *const u8,
        _name_len: usize,
    ) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[unsafe(export_name = "prime_struct_insert")]
    pub unsafe extern "C" fn prime_struct_insert(
        _handle: PrimeHandle,
        _field_ptr: *const u8,
        _field_len: usize,
        _value: PrimeHandle,
    ) -> PrimeStatus {
        PrimeStatus::Invalid
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
        _entry: Option<unsafe extern "C" fn(arg: PrimeHandle) -> PrimeHandle>,
        _arg: PrimeHandle,
        handle_out: *mut PrimeHandle,
    ) -> PrimeHandle {
        if !handle_out.is_null() {
            handle_out.write(PrimeHandle::null());
        }
        PrimeHandle::null()
    }

    #[unsafe(export_name = "prime_join")]
    pub unsafe extern "C" fn prime_join(
        _handle: PrimeHandle,
        result_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if !result_out.is_null() {
            result_out.write(PrimeHandle::null());
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_delay_ms")]
    pub unsafe extern "C" fn prime_delay_ms(_millis: i32) {}

    #[unsafe(export_name = "prime_pin_mode")]
    pub unsafe extern "C" fn prime_pin_mode(_pin: i32, _mode: i32) {}

    #[unsafe(export_name = "prime_digital_write")]
    pub unsafe extern "C" fn prime_digital_write(_pin: i32, _value: i32) {}

    #[unsafe(export_name = "prime_now_ms")]
    pub unsafe extern "C" fn prime_now_ms() -> i128 {
        0
    }

    #[unsafe(export_name = "prime_value_tag")]
    pub unsafe extern "C" fn prime_value_tag(handle: PrimeHandle) -> PrimeTag {
        if handle.0.is_null() {
            return PrimeTag::Unit;
        }
        let data = &*(handle.0 as *const HostHandle);
        match data.value {
            HostValue::Unit => PrimeTag::Unit,
            HostValue::Int(_) => PrimeTag::Int,
            HostValue::Float(_) => PrimeTag::Float,
            HostValue::Bool(_) => PrimeTag::Bool,
            HostValue::Str(_) => PrimeTag::String,
            HostValue::Slice(_) => PrimeTag::Slice,
            HostValue::Map(_) => PrimeTag::Map,
            HostValue::Enum { .. } => PrimeTag::Enum,
            HostValue::Receiver(_) => PrimeTag::Receiver,
            HostValue::Sender(_) => PrimeTag::Sender,
            HostValue::Task(_) => PrimeTag::JoinHandle,
        }
    }

    #[unsafe(export_name = "prime_value_as_int")]
    pub unsafe extern "C" fn prime_value_as_int(handle: PrimeHandle) -> i128 {
        if handle.0.is_null() {
            return 0;
        }
        let data = &*(handle.0 as *const HostHandle);
        match data.value {
            HostValue::Int(i) => i,
            HostValue::Bool(b) => {
                if b {
                    1
                } else {
                    0
                }
            }
            _ => 0,
        }
    }

    #[unsafe(export_name = "prime_value_as_bool")]
    pub unsafe extern "C" fn prime_value_as_bool(handle: PrimeHandle) -> bool {
        if handle.0.is_null() {
            return false;
        }
        let data = &*(handle.0 as *const HostHandle);
        match data.value {
            HostValue::Bool(b) => b,
            HostValue::Int(i) => i != 0,
            _ => false,
        }
    }

    #[unsafe(export_name = "prime_value_as_float")]
    pub unsafe extern "C" fn prime_value_as_float(handle: PrimeHandle) -> f64 {
        if handle.0.is_null() {
            return 0.0;
        }
        let data = &*(handle.0 as *const HostHandle);
        match data.value {
            HostValue::Float(f) => f,
            HostValue::Int(i) => i as f64,
            _ => 0.0,
        }
    }

    #[unsafe(export_name = "prime_value_as_string")]
    pub unsafe extern "C" fn prime_value_as_string(
        handle: PrimeHandle,
        ptr_out: *mut *const u8,
        len_out: *mut usize,
    ) -> PrimeStatus {
        if handle.0.is_null() || ptr_out.is_null() || len_out.is_null() {
            return PrimeStatus::Invalid;
        }
        let data = &*(handle.0 as *const HostHandle);
        let HostValue::Str(ref s) = data.value else {
            return PrimeStatus::Invalid;
        };
        ptr_out.write(s.as_bytes().as_ptr());
        len_out.write(s.len());
        PrimeStatus::Ok
    }

    #[unsafe(export_name = "prime_reference_read")]
    pub unsafe extern "C" fn prime_reference_read(_target: PrimeHandle) -> PrimeHandle {
        PrimeHandle::null()
    }

    #[unsafe(export_name = "prime_reference_write")]
    pub unsafe extern "C" fn prime_reference_write(
        _target: PrimeHandle,
        _value: PrimeHandle,
    ) -> PrimeStatus {
        PrimeStatus::Invalid
    }

    #[derive(Default, Debug)]
    struct HostTaskState {
        result: Mutex<Option<PrimeHandle>>,
        done: Condvar,
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
        values_ptr: *const PrimeHandle,
        values_len: usize,
        tag: u32,
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

    fn task_to_handle(state: Arc<HostTaskState>) -> PrimeHandle {
        to_handle(HostHandle::new(HostValue::Task(state)))
    }

    #[unsafe(export_name = "prime_print")]
    pub unsafe extern "C" fn prime_print(value: PrimeHandle) {
        if value.0.is_null() {
            return;
        }
        let data = &*(value.0 as *const HostHandle);
        let _ = unsafe { fflush(std::ptr::null_mut()) };
        let mut stdout = io::stdout();
        let _ = match &data.value {
            HostValue::Int(i) => write!(stdout, "{i}"),
            HostValue::Float(f) => write!(stdout, "{f}"),
            HostValue::Bool(b) => write!(stdout, "{b}"),
            HostValue::Str(s) => write!(stdout, "{s}"),
            HostValue::Enum { tag, .. } => write!(stdout, "<enum {tag}>"),
            HostValue::Unit => write!(stdout, "()"),
            HostValue::Task(_) => write!(stdout, "<task>"),
            HostValue::Receiver(_) => write!(stdout, "<receiver>"),
            HostValue::Sender(_) => write!(stdout, "<sender>"),
            HostValue::Slice(_) => write!(stdout, "<slice>"),
            HostValue::Map(_) => write!(stdout, "<map>"),
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

    #[unsafe(export_name = "prime_task_new")]
    pub unsafe extern "C" fn prime_task_new(
        entry: Option<unsafe extern "C" fn(arg: PrimeHandle) -> PrimeHandle>,
        arg: PrimeHandle,
    ) -> PrimeHandle {
        let Some(func) = entry else {
            return PrimeHandle::null();
        };
        let state = Arc::new(HostTaskState::default());
        let state_clone = state.clone();
        thread::spawn(move || {
            let result = func(arg);
            let (lock, cv) = (&state_clone.result, &state_clone.done);
            if let Ok(mut guard) = lock.lock() {
                *guard = Some(result);
                cv.notify_all();
            }
        });
        task_to_handle(state)
    }

    #[unsafe(export_name = "prime_task_poll")]
    pub unsafe extern "C" fn prime_task_poll(
        task: PrimeHandle,
        result_out: *mut PrimeHandle,
    ) -> PrimeStatus {
        if task.0.is_null() {
            return PrimeStatus::Invalid;
        }
        if result_out.is_null() {
            return PrimeStatus::Invalid;
        }
        let data = &*(task.0 as *const HostHandle);
        let HostValue::Task(state) = &data.value else {
            return PrimeStatus::Invalid;
        };
        if let Ok(mut guard) = state.result.lock() {
            if let Some(res) = guard.take() {
                result_out.write(res);
                return PrimeStatus::Ok;
            }
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_task_wake")]
    pub unsafe extern "C" fn prime_task_wake(task: PrimeHandle, value: PrimeHandle) -> PrimeStatus {
        if task.0.is_null() {
            return PrimeStatus::Invalid;
        }
        let data = &*(task.0 as *const HostHandle);
        let HostValue::Task(state) = &data.value else {
            return PrimeStatus::Invalid;
        };
        let (lock, cv) = (&state.result, &state.done);
        if let Ok(mut guard) = lock.lock() {
            *guard = Some(prime_value_retain(value));
            cv.notify_all();
            return PrimeStatus::Ok;
        }
        PrimeStatus::Invalid
    }

    #[unsafe(export_name = "prime_sleep_task")]
    pub unsafe extern "C" fn prime_sleep_task(millis: i64) -> PrimeHandle {
        let state = Arc::new(HostTaskState::default());
        let state_clone = state.clone();
        thread::spawn(move || {
            let duration = if millis <= 0 {
                std::time::Duration::from_millis(0)
            } else {
                std::time::Duration::from_millis(millis as u64)
            };
            thread::sleep(duration);
            let unit = prime_unit_new();
            let (lock, cv) = (&state_clone.result, &state_clone.done);
            if let Ok(mut guard) = lock.lock() {
                *guard = Some(unit);
                cv.notify_all();
            }
        });
        task_to_handle(state)
    }

    #[unsafe(export_name = "prime_recv_task")]
    pub unsafe extern "C" fn prime_recv_task(receiver: PrimeHandle) -> PrimeHandle {
        let data = if receiver.0.is_null() {
            return prime_sleep_task(0);
        } else {
            &*(receiver.0 as *const HostHandle)
        };
        let HostValue::Receiver(rx) = &data.value else {
            return prime_sleep_task(0);
        };
        let state = Arc::new(HostTaskState::default());
        let state_clone = state.clone();
        let rx_clone = rx.clone();
        thread::spawn(move || {
            let received = match rx_clone.lock() {
                Ok(guard) => guard.recv().ok(),
                Err(_) => None,
            };
            let (tag, payloads) = match received {
                Some(handle) => (0, vec![clone_value(handle)]),
                None => (1, Vec::new()),
            };
            let enum_value = HostValue::Enum {
                tag,
                values: payloads,
            };
            let option_handle = to_handle(HostHandle::with_tag(enum_value, tag));
            let (lock, cv) = (&state_clone.result, &state_clone.done);
            if let Ok(mut guard) = lock.lock() {
                *guard = Some(option_handle);
                cv.notify_all();
            }
        });
        task_to_handle(state)
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
