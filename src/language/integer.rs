//! Checked integer operations use mathematical values in the interpreter and constant folder.
//! The largest language integer is uint64. LLVM uses the same values in a widened register.
use super::{
    ast::{BinaryOp, Expr, ExprId},
    types::TypeExpr,
};
use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IntegerType {
    pub name: &'static str,
    pub bits: u32,
    pub signed: bool,
}

impl IntegerType {
    pub fn from_type(ty: &TypeExpr, pointer_bits: u32) -> Option<Self> {
        let TypeExpr::Named(name, args) = ty else {
            return None;
        };
        if !args.is_empty() {
            return None;
        }
        Self::from_name(name, pointer_bits)
    }

    pub fn from_name(name: &str, pointer_bits: u32) -> Option<Self> {
        let (name, bits, signed) = match name {
            "int8" => ("int8", 8, true),
            "int16" => ("int16", 16, true),
            "int32" => ("int32", 32, true),
            "int64" => ("int64", 64, true),
            "isize" => ("isize", pointer_bits, true),
            "uint8" => ("uint8", 8, false),
            "uint16" => ("uint16", 16, false),
            "uint32" => ("uint32", 32, false),
            "uint64" => ("uint64", 64, false),
            "usize" => ("usize", pointer_bits, false),
            _ => return None,
        };
        Some(Self { name, bits, signed })
    }

    pub fn bounds(self) -> (i128, i128) {
        let limit = 1_i128 << (self.bits - u32::from(self.signed));
        (if self.signed { -limit } else { 0 }, limit - 1)
    }

    pub fn checked(self, value: i128) -> Result<i128, String> {
        let (min, max) = self.bounds();
        if (min..=max).contains(&value) {
            Ok(value)
        } else {
            Err(format!(
                "integer result `{value}` is out of range for `{}`",
                self.name
            ))
        }
    }

    pub fn cast(self, value: i128) -> i128 {
        let modulus = 1_i128 << self.bits;
        let low = value & (modulus - 1);
        if self.signed && low >= modulus / 2 {
            low - modulus
        } else {
            low
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum IntegerOp {
    Binary(BinaryOp, IntegerType),
    Neg(IntegerType),
    Cast(IntegerType),
}

#[derive(Clone, Copy, Debug)]
pub enum Scalar {
    Int(i128),
    Bool(bool),
}

impl IntegerOp {
    pub fn evaluate(self, a: i128, b: Option<i128>) -> Result<Scalar, String> {
        use BinaryOp::*;
        let (ty, value) = match self {
            Self::Cast(ty) => return Ok(Scalar::Int(ty.cast(a))),
            Self::Neg(ty) => (ty, a.checked_neg()),
            Self::Binary(op, ty) => {
                let b = b.ok_or("missing right integer operand")?;
                let result = match op {
                    Add => a.checked_add(b),
                    Sub => a.checked_sub(b),
                    Mul => a.checked_mul(b),
                    Div | Rem if b == 0 => return Err("integer division by zero".into()),
                    Div => a.checked_div(b),
                    Rem => a.checked_rem(b),
                    BitAnd => Some(a & b),
                    BitOr => Some(a | b),
                    BitXor => Some(a ^ b),
                    Eq => return Ok(Scalar::Bool(a == b)),
                    NotEq => return Ok(Scalar::Bool(a != b)),
                    Lt => return Ok(Scalar::Bool(a < b)),
                    LtEq => return Ok(Scalar::Bool(a <= b)),
                    Gt => return Ok(Scalar::Bool(a > b)),
                    GtEq => return Ok(Scalar::Bool(a >= b)),
                    And | Or => return Err("boolean operation on integer operands".into()),
                };
                (ty, result)
            }
        };
        let value =
            value.ok_or_else(|| format!("integer result is out of range for `{}`", ty.name))?;
        ty.checked(value).map(Scalar::Int)
    }
}

/// Keys address individual expanded expressions, including copies of a macro body at one span.
#[derive(Clone, Debug, Default)]
pub struct CheckedIntegers {
    pub operations: HashMap<ExprId, IntegerOp>,
    pub narrowing: HashMap<ExprId, IntegerType>,
}

/// Blocks, matches and conditionals check their individual result expressions.
pub fn value_site(expr: &Expr) -> Option<ExprId> {
    match expr {
        Expr::Identifier(ident) => Some(ident.id),
        Expr::Binary { id, .. }
        | Expr::Unary { id, .. }
        | Expr::Call { id, .. }
        | Expr::FieldAccess { id, .. }
        | Expr::Index { id, .. }
        | Expr::Await { id, .. }
        | Expr::TryPropagate { id, .. }
        | Expr::Deref { id, .. } => Some(*id),
        _ => None,
    }
}
