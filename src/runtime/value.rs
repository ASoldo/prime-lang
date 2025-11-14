use crate::language::span::Span;
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
    Reference(Rc<RefCell<Value>>),
}

impl Value {
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Struct(_) | Value::Enum(_) | Value::Tuple(_) | Value::Range(_) => true,
            Value::Reference(cell) => cell.borrow().as_bool(),
            Value::Unit => false,
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
            Value::Reference(cell) => {
                write!(f, "&{}", cell.borrow())
            }
        }
    }
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
