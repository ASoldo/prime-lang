use crate::language::span::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum TypeExpr {
    Named(String, Vec<TypeExpr>),
    Slice(Box<TypeExpr>),
    Array { size: usize, ty: Box<TypeExpr> },
    Reference { mutable: bool, ty: Box<TypeExpr> },
    Pointer { mutable: bool, ty: Box<TypeExpr> },
    Tuple(Vec<TypeExpr>),
    Unit,
}

impl TypeExpr {
    pub fn named(name: impl Into<String>) -> Self {
        TypeExpr::Named(name.into(), Vec::new())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeAnnotation {
    pub ty: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mutability {
    Immutable,
    Mutable,
}

impl Mutability {
    pub fn is_mutable(self) -> bool {
        matches!(self, Mutability::Mutable)
    }
}
