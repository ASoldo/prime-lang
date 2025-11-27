#![allow(unsafe_op_in_unsafe_fn)]

use std::{
    collections::{BTreeMap, VecDeque},
    ffi::c_void,
    io::{self, Write},
    os::raw::c_char,
    ptr,
    sync::{
        Arc, Condvar, Mutex,
        atomic::{AtomicUsize, Ordering},
    },
    thread,
};

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
pub struct PrimeHandle(*mut PrimeValue);

// Type codes consumed by prime_read_value; must stay in sync with compiler lowering.
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

impl PrimeHandle {
    pub fn null() -> Self {
        Self(ptr::null_mut())
    }

    pub fn is_null(self) -> bool {
        self.0.is_null()
    }

    pub fn as_ptr(self) -> *mut PrimeValue {
        self.0
    }

    unsafe fn as_ref(self) -> Option<&'static PrimeValue> {
        self.0.as_ref()
    }

    unsafe fn as_mut(self) -> Option<&'static mut PrimeValue> {
        self.0.as_mut()
    }

    fn from_raw(ptr: *mut PrimeValue) -> Self {
        Self(ptr)
    }
}

unsafe impl Send for PrimeHandle {}
unsafe impl Sync for PrimeHandle {}

pub type PrimeThreadFn = unsafe extern "C" fn(arg: PrimeHandle) -> PrimeHandle;

#[allow(dead_code)]
fn _assert_runtime_sends() {
    fn assert_send<T: Send>() {}
    fn assert_sync<T: Sync>() {}
    assert_send::<PrimeHandle>();
    assert_sync::<PrimeHandle>();
    assert_send::<ThreadResult>();
}

pub struct PrimeValue {
    tag: PrimeTag,
    refs: AtomicUsize,
    payload: PrimePayload,
}

unsafe impl Send for PrimeValue {}
unsafe impl Sync for PrimeValue {}

enum PrimePayload {
    Unit,
    Int(i128),
    Float(f64),
    Bool(bool),
    String(String),
    Slice(PrimeSlice),
    Map(PrimeMap),
    Enum(PrimeEnum),
    Reference(PrimeReference),
    Sender(PrimeSender),
    Receiver(PrimeReceiver),
    JoinHandle(PrimeJoinHandle),
    Struct(PrimeStruct),
}

#[derive(Clone)]
struct PrimeSlice {
    items: Arc<Mutex<Vec<PrimeHandle>>>,
}

#[derive(Clone)]
struct PrimeMap {
    entries: Arc<Mutex<BTreeMap<String, PrimeHandle>>>,
}

#[derive(Clone)]
struct PrimeEnum {
    tag: u32,
    values: Vec<PrimeHandle>,
}

#[derive(Clone)]
struct PrimeReference {
    cell: Arc<Mutex<PrimeHandle>>,
    #[allow(dead_code)]
    mutable: bool,
}

#[derive(Clone)]
struct PrimeSender {
    inner: Arc<(Mutex<ChannelState>, Condvar)>,
}

#[derive(Clone)]
struct PrimeReceiver {
    inner: Arc<(Mutex<ChannelState>, Condvar)>,
}

#[derive(Clone)]
struct PrimeJoinHandle {
    state: Arc<Mutex<JoinState>>,
}

struct ChannelState {
    queue: VecDeque<PrimeHandle>,
    closed: bool,
}

enum ThreadResult {
    Ok(PrimeHandle),
    Panic,
}

unsafe impl Send for ThreadResult {}
unsafe impl Sync for ThreadResult {}

struct JoinState {
    handle: Option<thread::JoinHandle<ThreadResult>>,
    result: Option<ThreadResult>,
}

#[derive(Clone)]
struct PrimeStruct {
    name: String,
    fields: Arc<Mutex<BTreeMap<String, PrimeHandle>>>,
}

impl PrimeStruct {
    fn new(name: String) -> Self {
        Self {
            name,
            fields: Arc::new(Mutex::new(BTreeMap::new())),
        }
    }

    fn insert(&self, key: String, value: PrimeHandle) {
        self.fields.lock().unwrap().insert(key, retain_value(value));
    }
}

impl PrimeValue {
    fn new(tag: PrimeTag, payload: PrimePayload) -> PrimeHandle {
        PrimeHandle::from_raw(Box::into_raw(Box::new(Self {
            tag,
            refs: AtomicUsize::new(1),
            payload,
        })))
    }
}

impl PrimeSlice {
    fn new() -> Self {
        Self {
            items: Arc::new(Mutex::new(Vec::new())),
        }
    }

    fn push(&self, value: PrimeHandle) {
        self.items.lock().unwrap().push(retain_value(value));
    }

    fn snapshot(&self) -> Vec<PrimeHandle> {
        self.items.lock().unwrap().clone()
    }
}

impl PrimeMap {
    fn new() -> Self {
        Self {
            entries: Arc::new(Mutex::new(BTreeMap::new())),
        }
    }

    fn insert(&self, key: String, value: PrimeHandle) {
        self.entries
            .lock()
            .unwrap()
            .insert(key, retain_value(value));
    }

    fn snapshot(&self) -> Vec<PrimeHandle> {
        self.entries
            .lock()
            .unwrap()
            .values()
            .copied()
            .collect::<Vec<_>>()
    }
}

impl PrimeEnum {
    fn new(tag: u32, values: Vec<PrimeHandle>) -> Self {
        let retained = values.into_iter().map(retain_value).collect();
        Self {
            tag,
            values: retained,
        }
    }
}

impl PrimeReference {
    fn new(value: PrimeHandle, mutable: bool) -> Self {
        Self {
            cell: Arc::new(Mutex::new(retain_value(value))),
            mutable,
        }
    }

    fn load(&self) -> PrimeHandle {
        retain_value(*self.cell.lock().unwrap())
    }

    fn take_raw(&self) -> PrimeHandle {
        *self.cell.lock().unwrap()
    }
}

impl PrimeSender {
    fn new(inner: Arc<(Mutex<ChannelState>, Condvar)>) -> Self {
        Self { inner }
    }

    fn send(&self, value: PrimeHandle) -> PrimeStatus {
        let (lock, cv) = &*self.inner;
        let mut guard = lock.lock().unwrap();
        if guard.closed {
            return PrimeStatus::Closed;
        }
        guard.queue.push_back(retain_value(value));
        cv.notify_one();
        PrimeStatus::Ok
    }

    fn close(&self) {
        let (lock, cv) = &*self.inner;
        let mut guard = lock.lock().unwrap();
        guard.closed = true;
        cv.notify_all();
    }
}

impl PrimeReceiver {
    fn new(inner: Arc<(Mutex<ChannelState>, Condvar)>) -> Self {
        Self { inner }
    }

    fn recv(&self) -> Result<PrimeHandle, PrimeStatus> {
        let (lock, cv) = &*self.inner;
        let mut guard = lock.lock().unwrap();
        loop {
            if let Some(value) = guard.queue.pop_front() {
                return Ok(value);
            }
            if guard.closed {
                return Err(PrimeStatus::Closed);
            }
            guard = cv.wait(guard).unwrap();
        }
    }

    fn close(&self) {
        let (lock, cv) = &*self.inner;
        let mut guard = lock.lock().unwrap();
        guard.closed = true;
        cv.notify_all();
    }
}

impl PrimeJoinHandle {
    fn new(handle: thread::JoinHandle<ThreadResult>) -> Self {
        Self {
            state: Arc::new(Mutex::new(JoinState {
                handle: Some(handle),
                result: None,
            })),
        }
    }

    fn join(&self) -> ThreadResult {
        let maybe_handle = {
            let mut guard = self.state.lock().unwrap();
            if let Some(result) = guard.result.as_ref() {
                return result.clone();
            }
            guard.handle.take()
        };

        let result = maybe_handle
            .map(|h| h.join().unwrap_or(ThreadResult::Panic))
            .unwrap_or(ThreadResult::Panic);

        let mut guard = self.state.lock().unwrap();
        guard.result = Some(result.clone());
        result
    }

    fn discard(&self) {
        let result = {
            let mut guard = self.state.lock().unwrap();
            guard.handle.take().map(|handle| {
                if handle.is_finished() {
                    handle.join().unwrap_or(ThreadResult::Panic)
                } else {
                    // Let the thread keep running if the owner drops the handle prematurely.
                    std::mem::forget(handle);
                    ThreadResult::Panic
                }
            })
        };

        let stored = {
            let mut guard = self.state.lock().unwrap();
            guard.result.take()
        };

        for outcome in result.into_iter().chain(stored.into_iter()) {
            if let ThreadResult::Ok(handle) = outcome {
                unsafe {
                    release_value(handle);
                }
            }
        }
    }
}

impl Clone for ThreadResult {
    fn clone(&self) -> Self {
        match self {
            ThreadResult::Ok(handle) => ThreadResult::Ok(retain_value(*handle)),
            ThreadResult::Panic => ThreadResult::Panic,
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_value_retain(value: PrimeHandle) -> PrimeHandle {
    retain_value(value)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_value_release(value: PrimeHandle) {
    release_value(value);
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_unit_new() -> PrimeHandle {
    PrimeValue::new(PrimeTag::Unit, PrimePayload::Unit)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_int_new(value: i128) -> PrimeHandle {
    PrimeValue::new(PrimeTag::Int, PrimePayload::Int(value))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_float_new(value: f64) -> PrimeHandle {
    PrimeValue::new(PrimeTag::Float, PrimePayload::Float(value))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_bool_new(value: bool) -> PrimeHandle {
    PrimeValue::new(PrimeTag::Bool, PrimePayload::Bool(value))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_string_new(data: *const u8, len: usize) -> PrimeHandle {
    if data.is_null() && len > 0 {
        return PrimeHandle::null();
    }
    let slice = std::slice::from_raw_parts(data, len);
    let string = String::from_utf8_lossy(slice).into_owned();
    PrimeValue::new(PrimeTag::String, PrimePayload::String(string))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_slice_new() -> PrimeHandle {
    PrimeValue::new(PrimeTag::Slice, PrimePayload::Slice(PrimeSlice::new()))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_slice_push(slice: PrimeHandle, value: PrimeHandle) -> PrimeStatus {
    match as_payload_mut(slice, PrimeTag::Slice) {
        Some(PrimePayload::Slice(inner)) => {
            inner.push(value);
            PrimeStatus::Ok
        }
        _ => PrimeStatus::Invalid,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_slice_len_handle(handle: PrimeHandle) -> usize {
    match as_payload(handle, PrimeTag::Slice) {
        Some(PrimePayload::Slice(inner)) => inner.snapshot().len(),
        _ => 0,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_map_new() -> PrimeHandle {
    PrimeValue::new(PrimeTag::Map, PrimePayload::Map(PrimeMap::new()))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_map_len_handle(handle: PrimeHandle) -> usize {
    match as_payload(handle, PrimeTag::Map) {
        Some(PrimePayload::Map(inner)) => inner.entries.lock().unwrap().len(),
        _ => 0,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_map_insert(
    map: PrimeHandle,
    key_ptr: *const c_char,
    key_len: usize,
    value: PrimeHandle,
) -> PrimeStatus {
    if key_ptr.is_null() {
        return PrimeStatus::Invalid;
    }
    let key_bytes = unsafe { std::slice::from_raw_parts(key_ptr as *const u8, key_len) };
    let key = match String::from_utf8(key_bytes.to_vec()) {
        Ok(k) => k,
        Err(_) => return PrimeStatus::Invalid,
    };

    match as_payload_mut(map, PrimeTag::Map) {
        Some(PrimePayload::Map(inner)) => {
            inner.insert(key, value);
            PrimeStatus::Ok
        }
        _ => PrimeStatus::Invalid,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_struct_new(name_ptr: *const u8, name_len: usize) -> PrimeHandle {
    if name_ptr.is_null() && name_len > 0 {
        return PrimeHandle::null();
    }
    let bytes = std::slice::from_raw_parts(name_ptr, name_len);
    let name = match String::from_utf8(bytes.to_vec()) {
        Ok(n) => n,
        Err(_) => return PrimeHandle::null(),
    };
    PrimeValue::new(
        PrimeTag::Struct,
        PrimePayload::Struct(PrimeStruct::new(name)),
    )
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_struct_insert(
    handle: PrimeHandle,
    key_ptr: *const u8,
    key_len: usize,
    value: PrimeHandle,
) -> PrimeStatus {
    if key_ptr.is_null() {
        return PrimeStatus::Invalid;
    }
    let key_bytes = unsafe { std::slice::from_raw_parts(key_ptr, key_len) };
    let key = match String::from_utf8(key_bytes.to_vec()) {
        Ok(k) => k,
        Err(_) => return PrimeStatus::Invalid,
    };
    match as_payload_mut(handle, PrimeTag::Struct) {
        Some(PrimePayload::Struct(s)) => {
            s.insert(key, value);
            PrimeStatus::Ok
        }
        _ => PrimeStatus::Invalid,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_enum_new(
    values: *const PrimeHandle,
    len: usize,
    tag: u32,
) -> PrimeHandle {
    if values.is_null() && len > 0 {
        return PrimeHandle::null();
    }

    let items = std::slice::from_raw_parts(values, len);
    PrimeValue::new(
        PrimeTag::Enum,
        PrimePayload::Enum(PrimeEnum::new(tag, items.to_vec())),
    )
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_reference_new(target: PrimeHandle, mutable: bool) -> PrimeHandle {
    PrimeValue::new(
        PrimeTag::Reference,
        PrimePayload::Reference(PrimeReference::new(target, mutable)),
    )
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_reference_read(target: PrimeHandle) -> PrimeHandle {
    match as_payload(target, PrimeTag::Reference) {
        Some(PrimePayload::Reference(r)) => r.load(),
        _ => PrimeHandle::null(),
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_enum_tag(handle: PrimeHandle) -> u32 {
    match as_payload(handle, PrimeTag::Enum) {
        Some(PrimePayload::Enum(e)) => e.tag,
        _ => u32::MAX,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_enum_get(handle: PrimeHandle, idx: usize) -> PrimeHandle {
    match as_payload(handle, PrimeTag::Enum) {
        Some(PrimePayload::Enum(e)) => {
            if let Some(value) = e.values.get(idx) {
                retain_value(*value)
            } else {
                PrimeHandle::null()
            }
        }
        _ => PrimeHandle::null(),
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_read_value(
    type_code: u32,
    ok_tag: u32,
    err_tag: u32,
    prompt_ptr: *const u8,
    prompt_len: usize,
    fmt_values_ptr: *const PrimeHandle,
    fmt_values_len: usize,
) -> PrimeHandle {
    let prompt = if prompt_ptr.is_null() {
        String::new()
    } else {
        let slice = std::slice::from_raw_parts(prompt_ptr, prompt_len);
        let mut rendered = String::new();
        let mut handles = Vec::new();
        if !fmt_values_ptr.is_null() && fmt_values_len > 0 {
            handles.extend_from_slice(std::slice::from_raw_parts(fmt_values_ptr, fmt_values_len));
        }
        let mut handle_iter = handles.into_iter();
        // Minimal "{}" replacement to mirror `out` implicit placeholders.
        let mut idx = 0;
        while idx < slice.len() {
            if idx + 1 < slice.len() && slice[idx] == b'{' && slice[idx + 1] == b'}' {
                if let Some(handle) = handle_iter.next() {
                    rendered.push_str(&format_value(handle));
                }
                idx += 2;
                continue;
            }
            rendered.push(slice[idx] as char);
            idx += 1;
        }
        rendered
    };

    {
        let _ = io::stdout().write_all(prompt.as_bytes());
        let _ = io::stdout().flush();
    }

    let mut buffer = String::new();
    if let Err(err) = io::stdin().read_line(&mut buffer) {
        return build_result_enum(
            err_tag,
            vec![PrimeValue::new(
                PrimeTag::String,
                PrimePayload::String(format!("failed to read input: {err}")),
            )],
        );
    }
    let raw = buffer.trim_end_matches(&['\n', '\r'][..]).to_string();

    match parse_input_value(&raw, type_code) {
        Ok(value) => build_result_enum(ok_tag, vec![value]),
        Err(msg) => build_result_enum(
            err_tag,
            vec![PrimeValue::new(PrimeTag::String, PrimePayload::String(msg))],
        ),
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_channel_new(
    sender_out: *mut PrimeHandle,
    receiver_out: *mut PrimeHandle,
) -> PrimeStatus {
    if sender_out.is_null() || receiver_out.is_null() {
        return PrimeStatus::Invalid;
    }

    let inner = Arc::new((
        Mutex::new(ChannelState {
            queue: VecDeque::new(),
            closed: false,
        }),
        Condvar::new(),
    ));

    let sender = PrimeValue::new(
        PrimeTag::Sender,
        PrimePayload::Sender(PrimeSender::new(inner.clone())),
    );
    let receiver = PrimeValue::new(
        PrimeTag::Receiver,
        PrimePayload::Receiver(PrimeReceiver::new(inner)),
    );

    unsafe {
        *sender_out = sender;
        *receiver_out = receiver;
    }
    PrimeStatus::Ok
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_send(sender: PrimeHandle, value: PrimeHandle) -> PrimeStatus {
    match as_payload(sender, PrimeTag::Sender) {
        Some(PrimePayload::Sender(s)) => s.send(value),
        _ => PrimeStatus::Invalid,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_recv(
    receiver: PrimeHandle,
    value_out: *mut PrimeHandle,
) -> PrimeStatus {
    if value_out.is_null() {
        return PrimeStatus::Invalid;
    }
    match as_payload(receiver, PrimeTag::Receiver) {
        Some(PrimePayload::Receiver(r)) => match r.recv() {
            Ok(value) => {
                *value_out = value;
                PrimeStatus::Ok
            }
            Err(status) => status,
        },
        _ => PrimeStatus::Invalid,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_close(handle: PrimeHandle) -> PrimeStatus {
    match as_payload(handle, PrimeTag::Sender) {
        Some(PrimePayload::Sender(sender)) => {
            sender.close();
            return PrimeStatus::Ok;
        }
        _ => {}
    }

    match as_payload(handle, PrimeTag::Receiver) {
        Some(PrimePayload::Receiver(receiver)) => {
            receiver.close();
            return PrimeStatus::Ok;
        }
        _ => PrimeStatus::Invalid,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_spawn(
    entry: Option<PrimeThreadFn>,
    arg: PrimeHandle,
    handle_out: *mut PrimeHandle,
) -> PrimeStatus {
    let Some(entry_fn) = entry else {
        return PrimeStatus::Invalid;
    };

    if handle_out.is_null() {
        return PrimeStatus::Invalid;
    }

    let arg_for_thread = retain_value(arg);
    let handle = thread::spawn(move || {
        let result = std::panic::catch_unwind(|| unsafe { entry_fn(arg_for_thread) });
        match result {
            Ok(value) => ThreadResult::Ok(value),
            Err(_) => ThreadResult::Panic,
        }
    });

    let join_handle = PrimeValue::new(
        PrimeTag::JoinHandle,
        PrimePayload::JoinHandle(PrimeJoinHandle::new(handle)),
    );

    unsafe {
        *handle_out = join_handle;
    }
    PrimeStatus::Ok
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_join(
    handle: PrimeHandle,
    result_out: *mut PrimeHandle,
) -> PrimeStatus {
    if result_out.is_null() {
        return PrimeStatus::Invalid;
    }

    match as_payload(handle, PrimeTag::JoinHandle) {
        Some(PrimePayload::JoinHandle(join_handle)) => match join_handle.join() {
            ThreadResult::Ok(value) => {
                *result_out = value;
                PrimeStatus::Ok
            }
            ThreadResult::Panic => PrimeStatus::Panic,
        },
        _ => PrimeStatus::Invalid,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_print(value: PrimeHandle) {
    // Flush libc stdout so Rust stdout writes stay ordered relative to `printf` calls.
    fflush(std::ptr::null_mut());
    let rendered = format_value(value);
    print!("{rendered}");
    let _ = io::stdout().flush();
}

unsafe extern "C" {
    fn fflush(stream: *mut c_void) -> i32;
}

fn format_value(handle: PrimeHandle) -> String {
    unsafe {
        match handle.as_ref() {
            None => "<null>".to_string(),
            Some(value) => match &value.payload {
                PrimePayload::Unit => "()".to_string(),
                PrimePayload::Int(i) => i.to_string(),
                PrimePayload::Float(f) => f.to_string(),
                PrimePayload::Bool(b) => b.to_string(),
                PrimePayload::String(s) => s.clone(),
                PrimePayload::Slice(slice) => {
                    let parts: Vec<String> = slice
                        .snapshot()
                        .into_iter()
                        .map(|item| format_value(item))
                        .collect();
                    format!("[{}]", parts.join(", "))
                }
                PrimePayload::Map(map) => {
                    let entries = map.entries.lock().unwrap();
                    let parts: Vec<String> = entries
                        .iter()
                        .map(|(k, v)| format!("{k}: {}", format_value(*v)))
                        .collect();
                    format!("{{{}}}", parts.join(", "))
                }
                PrimePayload::Struct(s) => {
                    let entries = s.fields.lock().unwrap();
                    let parts: Vec<String> = entries
                        .iter()
                        .map(|(k, v)| format!("{k}: {}", format_value(*v)))
                        .collect();
                    format!("{} {{{}}}", s.name, parts.join(", "))
                }
                PrimePayload::Enum(e) => {
                    let parts: Vec<String> = e.values.iter().copied().map(format_value).collect();
                    format!("Enum#{}({})", e.tag, parts.join(", "))
                }
                PrimePayload::Reference(reference) => {
                    format!("&{}", format_value(reference.load()))
                }
                PrimePayload::Sender(_) => "<sender>".to_string(),
                PrimePayload::Receiver(_) => "<receiver>".to_string(),
                PrimePayload::JoinHandle(_) => "<join-handle>".to_string(),
            },
        }
    }
}

fn build_result_enum(tag: u32, values: Vec<PrimeHandle>) -> PrimeHandle {
    PrimeValue::new(
        PrimeTag::Enum,
        PrimePayload::Enum(PrimeEnum::new(tag, values)),
    )
}

fn parse_input_value(raw: &str, type_code: u32) -> Result<PrimeHandle, String> {
    match type_code {
        TYPE_STRING => Ok(PrimeValue::new(
            PrimeTag::String,
            PrimePayload::String(raw.to_string()),
        )),
        TYPE_BOOL => match raw.trim().to_ascii_lowercase().as_str() {
            "true" => Ok(PrimeValue::new(PrimeTag::Bool, PrimePayload::Bool(true))),
            "false" => Ok(PrimeValue::new(PrimeTag::Bool, PrimePayload::Bool(false))),
            _ => Err("expected `true` or `false`".into()),
        },
        TYPE_RUNE => {
            let mut chars = raw.chars().filter(|c| *c != '\r' && *c != '\n');
            if let Some(ch) = chars.next() {
                if chars.next().is_none() {
                    return Ok(PrimeValue::new(
                        PrimeTag::Int,
                        PrimePayload::Int(ch as i128),
                    ));
                }
            }
            Err("expected single rune".into())
        }
        TYPE_INT8 | TYPE_INT16 | TYPE_INT32 | TYPE_INT64 | TYPE_ISIZE => {
            let parsed = raw
                .trim()
                .parse::<i128>()
                .map_err(|_| "invalid integer input")?;
            let (min, max) = match type_code {
                TYPE_INT8 => (i8::MIN as i128, i8::MAX as i128),
                TYPE_INT16 => (i16::MIN as i128, i16::MAX as i128),
                TYPE_INT32 => (i32::MIN as i128, i32::MAX as i128),
                TYPE_INT64 => (i64::MIN as i128, i64::MAX as i128),
                TYPE_ISIZE => (isize::MIN as i128, isize::MAX as i128),
                _ => (i128::MIN, i128::MAX),
            };
            if parsed < min || parsed > max {
                return Err("integer out of range".into());
            }
            Ok(PrimeValue::new(PrimeTag::Int, PrimePayload::Int(parsed)))
        }
        TYPE_UINT8 | TYPE_UINT16 | TYPE_UINT32 | TYPE_UINT64 | TYPE_USIZE => {
            let parsed = raw
                .trim()
                .parse::<u128>()
                .map_err(|_| "invalid integer input")?;
            let max = match type_code {
                TYPE_UINT8 => u8::MAX as u128,
                TYPE_UINT16 => u16::MAX as u128,
                TYPE_UINT32 => u32::MAX as u128,
                TYPE_UINT64 => u64::MAX as u128,
                TYPE_USIZE => usize::MAX as u128,
                _ => u128::MAX,
            };
            if parsed > max {
                return Err("integer out of range".into());
            }
            Ok(PrimeValue::new(
                PrimeTag::Int,
                PrimePayload::Int(parsed as i128),
            ))
        }
        TYPE_FLOAT32 | TYPE_FLOAT64 => {
            let parsed = raw
                .trim()
                .parse::<f64>()
                .map_err(|_| "invalid float input")?;
            Ok(PrimeValue::new(
                PrimeTag::Float,
                PrimePayload::Float(parsed),
            ))
        }
        _ => Err("unsupported input type".into()),
    }
}

fn retain_value(handle: PrimeHandle) -> PrimeHandle {
    if let Some(value) = unsafe { handle.as_ref() } {
        value.refs.fetch_add(1, Ordering::Relaxed);
    }
    handle
}

#[allow(unsafe_op_in_unsafe_fn)]
unsafe fn release_value(handle: PrimeHandle) {
    if handle.is_null() {
        return;
    }

    let should_drop = {
        let value = handle.as_ref().unwrap();
        value.refs.fetch_sub(1, Ordering::Release) == 1
    };

    if !should_drop {
        return;
    }

    std::sync::atomic::fence(Ordering::Acquire);
    drop_value(handle);
}

#[allow(unsafe_op_in_unsafe_fn)]
unsafe fn drop_value(handle: PrimeHandle) {
    let mut value = Box::from_raw(handle.as_ptr());
    match &mut value.payload {
        PrimePayload::Slice(slice) => {
            let handles = slice.snapshot();
            for h in handles {
                release_value(h);
            }
        }
        PrimePayload::Map(map) => {
            let handles = map.snapshot();
            for h in handles {
                release_value(h);
            }
        }
        PrimePayload::Enum(e) => {
            for h in e.values.drain(..) {
                release_value(h);
            }
        }
        PrimePayload::Struct(s) => {
            let handles: Vec<_> = s.fields.lock().unwrap().values().copied().collect();
            for h in handles {
                release_value(h);
            }
        }
        PrimePayload::Reference(reference) => {
            let target = reference.take_raw();
            release_value(target);
        }
        PrimePayload::Sender(sender) => {
            sender.close();
        }
        PrimePayload::Receiver(receiver) => {
            receiver.close();
        }
        PrimePayload::JoinHandle(join_handle) => {
            join_handle.discard();
        }
        PrimePayload::Unit
        | PrimePayload::Int(_)
        | PrimePayload::Float(_)
        | PrimePayload::Bool(_)
        | PrimePayload::String(_) => {}
    }
}

#[allow(unsafe_op_in_unsafe_fn)]
unsafe fn as_payload<'a>(handle: PrimeHandle, expected: PrimeTag) -> Option<&'a PrimePayload> {
    handle
        .as_ref()
        .and_then(|value| (value.tag == expected).then_some(&value.payload))
}

#[allow(unsafe_op_in_unsafe_fn)]
unsafe fn as_payload_mut<'a>(
    handle: PrimeHandle,
    expected: PrimeTag,
) -> Option<&'a mut PrimePayload> {
    handle
        .as_mut()
        .and_then(|value| (value.tag == expected).then_some(&mut value.payload))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn creates_and_prints_values() {
        unsafe {
            let s = prime_string_new("hello".as_ptr(), 5);
            let int = prime_int_new(42);
            let slice = prime_slice_new();
            assert_eq!(prime_slice_push(slice, int), PrimeStatus::Ok);
            assert_eq!(prime_slice_push(slice, s), PrimeStatus::Ok);
            let output = format_value(slice);
            assert!(output.contains("hello"));
            release_value(slice);
            release_value(int);
            release_value(s);
        }
    }

    #[test]
    fn channels_block_and_deliver() {
        unsafe {
            let mut sender = PrimeHandle::null();
            let mut receiver = PrimeHandle::null();
            assert_eq!(
                prime_channel_new(&mut sender, &mut receiver),
                PrimeStatus::Ok
            );
            let sent = prime_int_new(99);
            assert_eq!(prime_send(sender, sent), PrimeStatus::Ok);
            let mut received = PrimeHandle::null();
            assert_eq!(prime_recv(receiver, &mut received), PrimeStatus::Ok);
            assert_eq!(format_value(received), "99".to_string());
            release_value(sender);
            release_value(receiver);
            release_value(sent);
            release_value(received);
        }
    }

    #[test]
    fn spawn_and_join_thread() {
        extern "C" fn echo(value: PrimeHandle) -> PrimeHandle {
            value
        }

        unsafe {
            let arg = prime_int_new(7);
            let mut join_handle = PrimeHandle::null();
            assert_eq!(
                prime_spawn(Some(echo), arg, &mut join_handle),
                PrimeStatus::Ok
            );

            let mut result = PrimeHandle::null();
            assert_eq!(prime_join(join_handle, &mut result), PrimeStatus::Ok);
            assert_eq!(format_value(result), "7".to_string());

            release_value(arg);
            release_value(join_handle);
            release_value(result);
        }
    }
}
