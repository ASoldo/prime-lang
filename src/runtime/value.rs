use crate::language::{span::Span, types::TypeExpr};
use std::cell::RefCell;
use std::collections::BTreeMap;
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
            Value::Unit => false,
            Value::Moved => panic!("attempted to read moved value"),
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Unit => "unit",
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::Bool(_) => "bool",
            Value::String(_) => "string",
            Value::Struct(_) => "struct",
            Value::Enum(_) => "enum",
            Value::Tuple(_) => "tuple",
            Value::Range(_) => "range",
            Value::Reference(_) => "reference",
            Value::Boxed(_) => "Box",
            Value::Slice(_) => "Slice",
            Value::Map(_) => "Map",
            Value::Moved => "moved",
        }
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
    pub span: Span,
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
    pub elem_type: Option<TypeExpr>,
}

impl SliceValue {
    pub fn new(elem_type: Option<TypeExpr>) -> Self {
        Self {
            items: Rc::new(RefCell::new(Vec::new())),
            elem_type,
        }
    }

    pub fn from_vec(items: Vec<Value>, elem_type: Option<TypeExpr>) -> Self {
        Self {
            items: Rc::new(RefCell::new(items)),
            elem_type,
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
    pub key_type: Option<TypeExpr>,
    pub value_type: Option<TypeExpr>,
}

impl MapValue {
    pub fn new(key_type: Option<TypeExpr>, value_type: Option<TypeExpr>) -> Self {
        Self {
            entries: Rc::new(RefCell::new(BTreeMap::new())),
            key_type,
            value_type,
        }
    }

    pub fn insert(&self, key: String, value: Value) {
        self.entries.borrow_mut().insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        self.entries.borrow().get(key).cloned()
    }
}
