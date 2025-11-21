use std::cell::RefCell;
use std::collections::{BTreeMap, VecDeque};
use std::fmt;
use std::rc::Rc;

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
    JoinHandle(Box<JoinHandleValue>),
    Moved,
}

impl Value {
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Struct(_) | Value::Enum(_) | Value::Tuple(_) | Value::Range(_) => true,
            Value::Reference(reference) => reference.cell.borrow().as_bool(),
            Value::Boxed(b) => b.cell.borrow().as_bool(),
            Value::Slice(slice) => !slice.items.borrow().is_empty(),
            Value::Map(map) => !map.entries.borrow().is_empty(),
            Value::FormatTemplate(_) => true,
            Value::Sender(_) | Value::Receiver(_) | Value::JoinHandle(_) => true,
            Value::Unit => false,
            Value::Moved => panic!("attempted to read moved value"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ChannelSender {
    pub(crate) queue: Rc<RefCell<VecDeque<Value>>>,
    pub(crate) closed: Rc<RefCell<bool>>,
}

#[derive(Clone, Debug)]
pub struct ChannelReceiver {
    pub(crate) queue: Rc<RefCell<VecDeque<Value>>>,
    pub(crate) closed: Rc<RefCell<bool>>,
}

impl ChannelSender {
    pub fn new(queue: Rc<RefCell<VecDeque<Value>>>, closed: Rc<RefCell<bool>>) -> Self {
        Self { queue, closed }
    }

    pub fn send(&self, value: Value) -> Result<(), String> {
        if *self.closed.borrow() {
            return Err("channel closed".into());
        }
        self.queue.borrow_mut().push_back(value);
        Ok(())
    }

    pub fn close(&self) {
        *self.closed.borrow_mut() = true;
    }
}

impl ChannelReceiver {
    pub fn new(queue: Rc<RefCell<VecDeque<Value>>>, closed: Rc<RefCell<bool>>) -> Self {
        Self { queue, closed }
    }

    pub fn recv(&self) -> Option<Value> {
        if let Some(v) = self.queue.borrow_mut().pop_front() {
            return Some(v);
        }
        if *self.closed.borrow() {
            None
        } else {
            None
        }
    }

    pub fn close(&self) {
        *self.closed.borrow_mut() = true;
    }
}

#[derive(Clone, Debug)]
pub struct JoinHandleValue {
    result: Option<Value>,
}

impl JoinHandleValue {
    pub fn new(value: Value) -> Self {
        Self { result: Some(value) }
    }

    pub fn join(&mut self) -> Result<Value, String> {
        self.result
            .take()
            .ok_or_else(|| "join handle already used".into())
    }
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
            Value::Reference(reference) => write!(f, "&{}", reference.cell.borrow()),
            Value::Boxed(inner) => write!(f, "Box({})", inner.cell.borrow()),
            Value::Slice(slice) => {
                write!(f, "[")?;
                for (idx, value) in slice.items.borrow().iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{value}")?;
                }
                write!(f, "]")
            }
            Value::Map(map) => {
                write!(f, "{{")?;
                let mut first = true;
                for (key, value) in map.entries.borrow().iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{key}: {value}")?;
                }
                write!(f, "}}")
            }
            Value::FormatTemplate(_) => write!(f, "<format string>"),
            Value::Sender(_) => write!(f, "Sender"),
            Value::Receiver(_) => write!(f, "Receiver"),
            Value::JoinHandle(_) => write!(f, "JoinHandle"),
            Value::Moved => write!(f, "<moved>"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ReferenceValue {
    pub cell: Rc<RefCell<Value>>,
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
    pub cell: Rc<RefCell<Value>>,
}

impl BoxValue {
    pub fn new(value: Value) -> Self {
        Self {
            cell: Rc::new(RefCell::new(value)),
        }
    }

    pub fn take(&self) -> Value {
        std::mem::replace(&mut *self.cell.borrow_mut(), Value::Unit)
    }

    pub fn replace(&self, value: Value) -> Value {
        std::mem::replace(&mut *self.cell.borrow_mut(), value)
    }
}

#[derive(Clone, Debug)]
pub struct SliceValue {
    pub items: Rc<RefCell<Vec<Value>>>,
}

impl SliceValue {
    pub fn new() -> Self {
        Self {
            items: Rc::new(RefCell::new(Vec::new())),
        }
    }

    pub fn from_vec(items: Vec<Value>) -> Self {
        Self {
            items: Rc::new(RefCell::new(items)),
        }
    }

    pub fn push(&self, value: Value) {
        self.items.borrow_mut().push(value);
    }

    pub fn len(&self) -> usize {
        self.items.borrow().len()
    }

    pub fn get(&self, index: usize) -> Option<Value> {
        self.items.borrow().get(index).cloned()
    }
}

#[derive(Clone, Debug)]
pub struct MapValue {
    pub entries: Rc<RefCell<BTreeMap<String, Value>>>,
}

impl MapValue {
    pub fn new() -> Self {
        Self {
            entries: Rc::new(RefCell::new(BTreeMap::new())),
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
        self.entries.borrow_mut().insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        self.entries.borrow().get(key).cloned()
    }

    pub fn len(&self) -> usize {
        self.entries.borrow().len()
    }
}
