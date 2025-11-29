use crate::language::ast::{ClosureBody, FunctionParam};
use crate::runtime::error::RuntimeResult;
use std::collections::BTreeMap;
use std::fmt;
use std::sync::{Arc, Condvar, Mutex, mpsc};
use std::thread;
use std::time::Duration;

#[derive(Clone, Debug)]
pub enum Value {
    Unit,
    Int(i128),
    Float(f64),
    Bool(bool),
    String(String),
    Struct(StructInstance),
    Enum(EnumValue),
    Tuple(Vec<Value>),
    Range(RangeValue),
    Reference(ReferenceValue),
    Boxed(BoxValue),
    Slice(SliceValue),
    Map(MapValue),
    FormatTemplate(FormatTemplateValue),
    Sender(ChannelSender),
    Receiver(ChannelReceiver),
    Iterator(IteratorValue),
    JoinHandle(Box<JoinHandleValue>),
    Task(Box<TaskValue>),
    Pointer(PointerValue),
    Closure(ClosureValue),
    Moved,
}

type SharedValue = Arc<Mutex<Value>>;

impl Value {
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Struct(_) | Value::Enum(_) | Value::Tuple(_) | Value::Range(_) => true,
            Value::Reference(reference) => reference.cell.lock().unwrap().as_bool(),
            Value::Boxed(b) => b.cell.lock().unwrap().as_bool(),
            Value::Slice(slice) => !slice.items.lock().unwrap().is_empty(),
            Value::Map(map) => !map.entries.lock().unwrap().is_empty(),
            Value::FormatTemplate(_) => true,
            Value::Sender(_) | Value::Receiver(_) | Value::JoinHandle(_) | Value::Task(_) => true,
            Value::Pointer(_) => true,
            Value::Closure(_) => true,
            Value::Iterator(iter) => {
                let idx = *iter.index.lock().unwrap();
                let len = iter.items.lock().unwrap().len();
                idx < len
            }
            Value::Unit => false,
            Value::Moved => panic!("attempted to read moved value"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ChannelSender {
    inner: Arc<Mutex<Option<mpsc::Sender<Value>>>>,
}

#[derive(Clone, Debug)]
pub struct ChannelReceiver {
    sender: Arc<Mutex<Option<mpsc::Sender<Value>>>>,
    receiver: Arc<Mutex<mpsc::Receiver<Value>>>,
}

#[derive(Clone, Debug)]
pub enum TryRecvOutcome {
    Pending,
    Closed,
    Item(Value),
}

impl ChannelSender {
    pub fn new(sender: Arc<Mutex<Option<mpsc::Sender<Value>>>>) -> Self {
        Self { inner: sender }
    }

    pub fn send(&self, value: Value) -> Result<(), String> {
        let guard = self.inner.lock().map_err(|_| "channel poisoned")?;
        if let Some(sender) = guard.as_ref() {
            sender.send(value).map_err(|_| "channel closed".into())
        } else {
            Err("channel closed".into())
        }
    }

    pub fn close(&self) {
        if let Ok(mut guard) = self.inner.lock() {
            guard.take();
        }
    }

}

impl ChannelReceiver {
    pub fn new(
        sender: Arc<Mutex<Option<mpsc::Sender<Value>>>>,
        receiver: Arc<Mutex<mpsc::Receiver<Value>>>,
    ) -> Self {
        Self { sender, receiver }
    }

    pub fn recv(&self) -> Option<Value> {
        let guard = self.receiver.lock().ok()?;
        guard.recv().ok()
    }

    pub fn try_recv(&self) -> TryRecvOutcome {
        let guard = match self.receiver.lock() {
            Ok(g) => g,
            Err(_) => return TryRecvOutcome::Closed,
        };
        match guard.try_recv() {
            Ok(v) => TryRecvOutcome::Item(v),
            Err(mpsc::TryRecvError::Empty) => TryRecvOutcome::Pending,
            Err(mpsc::TryRecvError::Disconnected) => TryRecvOutcome::Closed,
        }
    }

    pub fn recv_timeout(&self, millis: i64) -> Option<Value> {
        let duration = if millis < 0 {
            Duration::from_millis(0)
        } else {
            Duration::from_millis(millis as u64)
        };
        let guard = self.receiver.lock().ok()?;
        match guard.recv_timeout(duration) {
            Ok(v) => Some(v),
            Err(_) => None,
        }
    }

    pub fn close(&self) {
        if let Ok(mut guard) = self.sender.lock() {
            guard.take();
        }
    }

}

#[derive(Clone, Debug)]
pub struct JoinHandleValue {
    handle: Arc<Mutex<Option<thread::JoinHandle<RuntimeResult<Value>>>>>,
}

impl JoinHandleValue {
    pub fn new(handle: thread::JoinHandle<RuntimeResult<Value>>) -> Self {
        Self {
            handle: Arc::new(Mutex::new(Some(handle))),
        }
    }

    pub fn join(&self) -> Result<Value, String> {
        let mut guard = self.handle.lock().map_err(|_| "join handle poisoned")?;
        let handle = guard
            .take()
            .ok_or_else(|| "join handle already used".to_string())?;
        match handle.join() {
            Ok(Ok(value)) => Ok(value),
            Ok(Err(err)) => Err(err.to_string()),
            Err(_) => Err("task panicked".into()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct PointerValue {
    pub cell: SharedValue,
    pub mutable: bool,
}

#[derive(Clone, Debug)]
pub struct TaskValue {
    state: Arc<(Mutex<TaskState>, Condvar)>,
}

#[derive(Debug, Default)]
pub(crate) struct TaskState {
    pub(crate) result: Option<RuntimeResult<Value>>,
    pub(crate) finished: bool,
}

impl TaskValue {
    #[allow(dead_code)]
    pub fn new() -> Self {
        TaskValue {
            state: Arc::new((Mutex::new(TaskState::default()), Condvar::new())),
        }
    }

    pub fn new_pair() -> (TaskValue, Arc<(Mutex<TaskState>, Condvar)>) {
        let state = Arc::new((Mutex::new(TaskState::default()), Condvar::new()));
        (TaskValue { state: state.clone() }, state)
    }

    pub fn with_state(state: Arc<(Mutex<TaskState>, Condvar)>) -> Self {
        TaskValue { state }
    }

    #[allow(dead_code)]
    pub fn finish(&self, result: RuntimeResult<Value>) {
        Self::store_result(&self.state, result);
    }

    pub fn store_result(state: &Arc<(Mutex<TaskState>, Condvar)>, result: RuntimeResult<Value>) {
        let (lock, cvar) = &**state;
        let mut guard = lock.lock().unwrap();
        guard.result = Some(result);
        guard.finished = true;
        cvar.notify_all();
    }

    pub fn join(&self) -> Result<Value, String> {
        let (lock, cvar) = &*self.state;
        let mut guard = lock.lock().map_err(|_| "task state poisoned")?;
        while !guard.finished {
            guard = cvar
                .wait(guard)
                .map_err(|_| "task wait poisoned".to_string())?;
        }
        match guard.result.take() {
            Some(Ok(value)) => Ok(value),
            Some(Err(err)) => Err(err.to_string()),
            None => Err("task missing result".into()),
        }
    }

    pub fn is_finished(&self) -> bool {
        let (lock, _) = &*self.state;
        lock.lock().map(|g| g.finished).unwrap_or(false)
    }

    #[allow(dead_code)]
    pub fn state(&self) -> Arc<(Mutex<TaskState>, Condvar)> {
        self.state.clone()
    }
}

#[derive(Clone, Debug)]
pub struct CapturedValue {
    pub name: String,
    pub value: Value,
    pub mutable: bool,
}

#[derive(Clone, Debug)]
pub struct ClosureValue {
    pub params: Vec<FunctionParam>,
    pub body: ClosureBody,
    #[allow(dead_code)]
    pub ret: Option<crate::language::types::TypeAnnotation>,
    pub captures: Vec<CapturedValue>,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Int(v) => write!(f, "{v}"),
            Value::Float(v) => write!(f, "{v}"),
            Value::Bool(v) => write!(f, "{v}"),
            Value::String(v) => write!(f, "{v}"),
            Value::Struct(instance) => write!(f, "{}", instance),
            Value::Enum(value) => write!(f, "{}", value),
            Value::Tuple(values) => {
                write!(f, "(")?;
                for (idx, value) in values.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{value}")?;
                }
                write!(f, ")")
            }
            Value::Range(range) => write!(
                f,
                "{}{}{}",
                range.start,
                if range.inclusive { "..=" } else { ".." },
                range.end
            ),
            Value::Reference(reference) => {
                let guard = reference.cell.lock().unwrap();
                write!(f, "&{}", *guard)
            }
            Value::Boxed(inner) => {
                let guard = inner.cell.lock().unwrap();
                write!(f, "Box({})", *guard)
            }
            Value::Slice(slice) => {
                let guard = slice.items.lock().unwrap();
                write!(f, "[")?;
                for (idx, value) in guard.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{value}")?;
                }
                write!(f, "]")
            }
            Value::Map(map) => {
                let guard = map.entries.lock().unwrap();
                write!(f, "{{")?;
                let mut first = true;
                for (key, value) in guard.iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{key}: {value}")?;
                }
                write!(f, "}}")
            }
            Value::Iterator(_) => write!(f, "<iter>"),
            Value::FormatTemplate(_) => write!(f, "<format string>"),
            Value::Sender(_) => write!(f, "Sender"),
            Value::Receiver(_) => write!(f, "Receiver"),
            Value::JoinHandle(_) => write!(f, "JoinHandle"),
            Value::Task(_) => write!(f, "Task"),
            Value::Pointer(ptr) => write!(
                f,
                "{}Pointer->{}",
                if ptr.mutable { "mut " } else { "" },
                ptr.cell.lock().unwrap()
            ),
            Value::Closure(_) => write!(f, "<closure>"),
            Value::Moved => write!(f, "<moved>"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ReferenceValue {
    pub cell: SharedValue,
    pub mutable: bool,
    pub origin: Option<String>,
}

#[derive(Clone, Debug)]
pub struct StructInstance {
    pub name: String,
    pub fields: BTreeMap<String, Value>,
    pub embedded: Vec<Value>,
}

impl StructInstance {
    pub fn get_field(&self, field: &str) -> Option<Value> {
        if let Some(value) = self.fields.get(field) {
            return Some(value.clone());
        }
        for embed in &self.embedded {
            if let Value::Struct(struct_value) = embed {
                if let Some(value) = struct_value.get_field(field) {
                    return Some(value);
                }
            }
        }
        None
    }
}

impl fmt::Display for StructInstance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{{", self.name)?;
        let mut first = true;
        for (name, value) in &self.fields {
            if !first {
                write!(f, ", ")?;
            }
            first = false;
            write!(f, "{}: {}", name, value)?;
        }
        write!(f, "}}")
    }
}

#[derive(Clone, Debug)]
pub struct EnumValue {
    pub enum_name: String,
    pub variant: String,
    pub values: Vec<Value>,
}

pub type FormatTemplateValue = FormatTemplateValueGeneric<Value>;
pub type FormatRuntimeSegment = FormatRuntimeSegmentGeneric<Value>;

#[derive(Clone, Debug)]
pub struct FormatTemplateValueGeneric<T> {
    pub segments: Vec<FormatRuntimeSegmentGeneric<T>>,
    pub implicit_placeholders: usize,
}

#[derive(Clone, Debug)]
pub enum FormatRuntimeSegmentGeneric<T> {
    Literal(String),
    Named(T),
    Implicit,
}

impl fmt::Display for EnumValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.values.is_empty() {
            write!(f, "{}::{}", self.enum_name, self.variant)
        } else {
            write!(f, "{}::{}(", self.enum_name, self.variant)?;
            for (idx, value) in self.values.iter().enumerate() {
                if idx > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{value}")?;
            }
            write!(f, ")")
        }
    }
}

#[derive(Clone, Debug)]
pub struct RangeValue {
    pub start: i128,
    pub end: i128,
    pub inclusive: bool,
}

#[derive(Clone, Debug)]
pub struct BoxValue {
    pub cell: SharedValue,
}

impl BoxValue {
    pub fn new(value: Value) -> Self {
        Self {
            cell: Arc::new(Mutex::new(value)),
        }
    }

    pub fn take(&self) -> Value {
        std::mem::replace(&mut *self.cell.lock().unwrap(), Value::Unit)
    }

    pub fn replace(&self, value: Value) -> Value {
        std::mem::replace(&mut *self.cell.lock().unwrap(), value)
    }
}

#[derive(Clone, Debug)]
pub struct SliceValue {
    pub items: Arc<Mutex<Vec<Value>>>,
}

impl SliceValue {
    pub fn new() -> Self {
        Self {
            items: Arc::new(Mutex::new(Vec::new())),
        }
    }

    pub fn from_vec(items: Vec<Value>) -> Self {
        Self {
            items: Arc::new(Mutex::new(items)),
        }
    }

    pub fn push(&self, value: Value) {
        self.items.lock().unwrap().push(value);
    }

    pub fn len(&self) -> usize {
        self.items.lock().unwrap().len()
    }

    pub fn get(&self, index: usize) -> Option<Value> {
        self.items.lock().unwrap().get(index).cloned()
    }

    pub fn set(&self, index: usize, value: Value) -> bool {
        let mut guard = self.items.lock().unwrap();
        if let Some(slot) = guard.get_mut(index) {
            *slot = value;
            true
        } else {
            false
        }
    }
}

#[derive(Clone, Debug)]
pub struct MapValue {
    pub entries: Arc<Mutex<BTreeMap<String, Value>>>,
}

#[derive(Clone, Debug)]
pub struct IteratorValue {
    pub items: Arc<Mutex<Vec<Value>>>,
    pub index: Arc<Mutex<usize>>,
}

impl IteratorValue {
    pub fn from_items(items: Vec<Value>) -> Self {
        Self {
            items: Arc::new(Mutex::new(items)),
            index: Arc::new(Mutex::new(0)),
        }
    }

    pub fn next(&self) -> Option<Value> {
        let mut idx_guard = self.index.lock().unwrap();
        let items_guard = self.items.lock().unwrap();
        if *idx_guard >= items_guard.len() {
            return None;
        }
        let value = items_guard.get(*idx_guard).cloned();
        *idx_guard += 1;
        value
    }
}

impl MapValue {
    pub fn new() -> Self {
        Self {
            entries: Arc::new(Mutex::new(BTreeMap::new())),
        }
    }

    pub fn from_entries(entries: Vec<(String, Value)>) -> Self {
        let map = Self::new();
        for (key, value) in entries {
            map.insert(key, value);
        }
        map
    }

    pub fn insert(&self, key: String, value: Value) {
        self.entries.lock().unwrap().insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        self.entries.lock().unwrap().get(key).cloned()
    }

    pub fn len(&self) -> usize {
        self.entries.lock().unwrap().len()
    }
}

pub fn make_option_value(value: Option<Value>) -> Value {
    match value {
        Some(v) => Value::Enum(EnumValue {
            enum_name: "Option".into(),
            variant: "Some".into(),
            values: vec![v],
        }),
        None => Value::Enum(EnumValue {
            enum_name: "Option".into(),
            variant: "None".into(),
            values: Vec::new(),
        }),
    }
}
