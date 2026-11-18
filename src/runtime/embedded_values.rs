// Included by the standalone no_std ABI on both 32-bit boards.
use super::*;

// Tagged integers need no owner, even when sent or nested. Channels end in 01,
// tokens in 10, and pointers to owned records are naturally word-aligned.
const INTEGER: usize = 3;
const BOOL_FALSE: usize = 7;
const BOOL_TRUE: usize = 15;
const BUSY: usize = usize::MAX;
pub(super) const MAX_FIELDS: usize = 4;

#[derive(Clone, Copy)]
pub(super) enum Scalar {
    Int(i128),
    Float(f64),
}

#[derive(Clone, Copy)]
struct Variant {
    tag: u32,
    len: usize,
    fields: [PrimeHandle; MAX_FIELDS],
}

struct Slot<T> {
    owners: AtomicUsize,
    value: UnsafeCell<core::mem::MaybeUninit<T>>,
}

// Only a producer with an exclusive reservation writes the value. Between
// publication and last release an owner can read it concurrently.
unsafe impl<T: Send> Sync for Slot<T> {}

struct Pool<T, const N: usize>([Slot<T>; N]);

impl<T: Copy, const N: usize> Pool<T, N> {
    const fn new() -> Self {
        Self(
            [const {
                Slot {
                    owners: AtomicUsize::new(0),
                    value: UnsafeCell::new(core::mem::MaybeUninit::uninit()),
                }
            }; N],
        )
    }

    fn slot(&self, h: PrimeHandle) -> Option<&Slot<T>> {
        let offset = h.0.addr().checked_sub(self.0.as_ptr().addr())?;
        let stride = core::mem::size_of::<Slot<T>>();
        if offset % stride != 0 {
            return None;
        }
        self.0.get(offset / stride)
    }

    fn new_handle(&self, value: T) -> PrimeHandle {
        for slot in &self.0 {
            if update_word(&slot.owners, |n| (n == 0).then_some(BUSY)).is_some() {
                unsafe {
                    (*slot.value.get()).write(value);
                }
                slot.owners.store(1, Ordering::SeqCst);
                return PrimeHandle(ptr::from_ref(slot).cast_mut().cast());
            }
        }
        PrimeHandle::null()
    }

    fn get(&self, h: PrimeHandle) -> Option<T> {
        let slot = self.slot(h)?;
        let owners = slot.owners.load(Ordering::SeqCst);
        (owners != 0 && owners != BUSY).then(|| unsafe { (*slot.value.get()).assume_init() })
    }
}

static SCALARS: Pool<Scalar, 16> = Pool::new();
static ENUMS: Pool<Variant, 128> = Pool::new();

// The Xtensa UART formatter uses a rotating scratch buffer. Capture its bytes
// when they cross an ownership boundary. C3 strings are already immutable.
#[cfg(target_arch = "xtensa")]
#[derive(Clone, Copy)]
struct CapturedString {
    len: usize,
    bytes: [u8; 256],
}

#[cfg(target_arch = "xtensa")]
static STRINGS: Pool<CapturedString, 16> = Pool::new();

#[cfg(target_arch = "xtensa")]
pub(super) fn captured_string(handle: PrimeHandle) -> Option<PrimeString> {
    let slot = STRINGS.slot(handle)?;
    let n = slot.owners.load(Ordering::SeqCst);
    if n == 0 || n == BUSY {
        return None;
    }
    let value = unsafe { &*(*slot.value.get()).as_ptr() };
    Some(PrimeString {
        ptr: value.bytes.as_ptr(),
        len: value.len,
    })
}

#[cfg(target_arch = "xtensa")]
fn capture_uart(handle: PrimeHandle) -> Option<PrimeHandle> {
    let first = (&raw const STR_HANDLES).cast::<PrimeString>();
    let offset = handle.0.addr().checked_sub(first.addr())?;
    let stride = core::mem::size_of::<PrimeString>();
    if offset % stride != 0 || offset / stride >= STR_HANDLES_MAX {
        return None;
    }
    let string = unsafe { first.add(offset / stride).read() };
    let mut saved = CapturedString {
        len: string.len,
        bytes: [0; 256],
    };
    if string.len >= saved.bytes.len() {
        return Some(PrimeHandle::null());
    }
    unsafe {
        ptr::copy_nonoverlapping(string.ptr, saved.bytes.as_mut_ptr(), string.len);
    }
    Some(STRINGS.new_handle(saved))
}

fn immediate(bits: usize) -> PrimeHandle {
    PrimeHandle(ptr::without_provenance_mut(bits))
}

pub(super) fn int(value: i128) -> PrimeHandle {
    if (-(1 << 28)..(1 << 28)).contains(&value) {
        immediate(((value as usize) << 3) | INTEGER)
    } else {
        SCALARS.new_handle(Scalar::Int(value))
    }
}

pub(super) fn boolean(value: bool) -> PrimeHandle {
    immediate(if value { BOOL_TRUE } else { BOOL_FALSE })
}

pub(super) fn as_bool(value: PrimeHandle) -> bool {
    value.0.addr() == BOOL_TRUE
}

pub(super) fn float(value: f64) -> PrimeHandle {
    SCALARS.new_handle(Scalar::Float(value))
}

pub(super) fn scalar(value: PrimeHandle) -> Option<Scalar> {
    let bits = value.0.addr();
    if bits & 7 == INTEGER {
        Some(Scalar::Int(((bits as isize) >> 3) as i128))
    } else {
        SCALARS.get(value)
    }
}

pub(super) fn kind(value: PrimeHandle) -> Option<PrimeTag> {
    if matches!(value.0.addr(), BOOL_FALSE | BOOL_TRUE) {
        return Some(PrimeTag::Bool);
    }
    #[cfg(target_arch = "xtensa")]
    if captured_string(value).is_some() {
        return Some(PrimeTag::String);
    }
    if ENUMS.get(value).is_some() {
        return Some(PrimeTag::Enum);
    }
    scalar(value).map(|v| match v {
        Scalar::Int(_) => PrimeTag::Int,
        Scalar::Float(_) => PrimeTag::Float,
    })
}

fn owners(value: PrimeHandle) -> Option<&'static AtomicUsize> {
    #[cfg(target_arch = "xtensa")]
    if let Some(slot) = STRINGS.slot(value) {
        return Some(&slot.owners);
    }
    ENUMS
        .slot(value)
        .map(|s| &s.owners)
        .or_else(|| SCALARS.slot(value).map(|s| &s.owners))
}

pub(super) fn retain(value: PrimeHandle) -> PrimeHandle {
    #[cfg(target_arch = "xtensa")]
    if let Some(captured) = capture_uart(value) {
        return captured;
    }
    if let Some(state) = owners(value) {
        if update_word(state, |n| (n > 0 && n < BUSY - 1).then(|| n + 1)).is_none() {
            return PrimeHandle::null();
        }
    }
    value
}

pub(super) fn release(value: PrimeHandle) {
    let Some(state) = owners(value) else { return };
    let previous = update_word(state, |n| match n {
        0 | BUSY => None,
        1 => Some(BUSY),
        n => Some(n - 1),
    });
    if previous == Some(1) {
        if let Some(slot) = ENUMS.slot(value) {
            let variant = unsafe { (*slot.value.get()).assume_init() };
            for field in &variant.fields[..variant.len] {
                release(*field);
            }
        }
        state.store(0, Ordering::SeqCst);
    }
}

pub(super) fn variant(tag: u32, fields: &[PrimeHandle]) -> PrimeHandle {
    if fields.len() > MAX_FIELDS {
        return PrimeHandle::null();
    }
    let mut v = Variant {
        tag,
        len: fields.len(),
        fields: [PrimeHandle::null(); MAX_FIELDS],
    };
    for (i, field) in fields.iter().enumerate() {
        // Token ownership inside arbitrary aggregates is not supported.
        let owned = if tokens::is_handle(*field) {
            PrimeHandle::null()
        } else {
            retain(*field)
        };
        if owned.0.is_null() {
            for field in &v.fields[..i] {
                release(*field);
            }
            return PrimeHandle::null();
        }
        v.fields[i] = owned;
    }
    let result = ENUMS.new_handle(v);
    if result.0.is_null() {
        for field in &v.fields[..v.len] {
            release(*field);
        }
    }
    result
}

pub(super) fn tag(handle: PrimeHandle) -> u32 {
    ENUMS.get(handle).map_or(u32::MAX, |v| v.tag)
}

pub(super) fn get(handle: PrimeHandle, index: usize) -> PrimeHandle {
    ENUMS
        .get(handle)
        .and_then(|v| v.fields[..v.len].get(index).copied())
        .map(retain)
        .unwrap_or(PrimeHandle::null())
}

// The compiler evaluates a static expression repeatedly in a loop. Each SSA
// producer has a frame-local owner, replaced on re-entry and released on return.
pub(super) fn require_value(value: PrimeHandle) {
    if value.0.is_null() {
        let msg = b"embedded value storage exhausted or unsupported enum payload";
        unsafe {
            prime_integer_check(false, msg.as_ptr(), msg.len());
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_frame_adopt(slot: *mut PrimeHandle, value: PrimeHandle) {
    require_value(value);
    release(slot.replace(value));
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_frame_copy(slot: *mut PrimeHandle, value: PrimeHandle) {
    prime_frame_adopt(slot, retain(value));
}

// A polled task or channel transfers its reference only on success.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_frame_out(
    slot: *mut PrimeHandle,
    out: *const PrimeHandle,
    status: PrimeStatus,
) {
    if status == PrimeStatus::Ok {
        prime_frame_adopt(slot, out.read());
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_frame_clear(slot: *mut PrimeHandle) {
    release(slot.replace(PrimeHandle::null()));
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_token_release(value: PrimeHandle) {
    tokens::release(value);
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_token_retain(value: PrimeHandle) -> PrimeHandle {
    tokens::retain(value)
}
