use crate::language::span::Span;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum TypeExpr {
    Named(String, Vec<TypeExpr>),
    Slice(Box<TypeExpr>),
    Array { size: usize, ty: Box<TypeExpr> },
    Reference { mutable: bool, ty: Box<TypeExpr> },
    Pointer { mutable: bool, ty: Box<TypeExpr> },
    Tuple(Vec<TypeExpr>),
    Unit,
    SelfType,
}

impl TypeExpr {
    pub fn named(name: impl Into<String>) -> Self {
        TypeExpr::Named(name.into(), Vec::new())
    }

    pub fn substitute(&self, map: &HashMap<String, TypeExpr>) -> TypeExpr {
        match self {
            TypeExpr::Named(name, args) => {
                if args.is_empty() {
                    map.get(name)
                        .cloned()
                        .unwrap_or_else(|| TypeExpr::Named(name.clone(), Vec::new()))
                } else {
                    TypeExpr::Named(
                        name.clone(),
                        args.iter().map(|ty| ty.substitute(map)).collect(),
                    )
                }
            }
            TypeExpr::Slice(inner) => TypeExpr::Slice(Box::new(inner.substitute(map))),
            TypeExpr::Array { size, ty } => TypeExpr::Array {
                size: *size,
                ty: Box::new(ty.substitute(map)),
            },
            TypeExpr::Reference { mutable, ty } => TypeExpr::Reference {
                mutable: *mutable,
                ty: Box::new(ty.substitute(map)),
            },
            TypeExpr::Pointer { mutable, ty } => TypeExpr::Pointer {
                mutable: *mutable,
                ty: Box::new(ty.substitute(map)),
            },
            TypeExpr::Tuple(types) => {
                TypeExpr::Tuple(types.iter().map(|ty| ty.substitute(map)).collect())
            }
            TypeExpr::Unit => TypeExpr::Unit,
            TypeExpr::SelfType => TypeExpr::SelfType,
        }
    }

    pub fn replace_self(&self, concrete: &TypeExpr) -> TypeExpr {
        match self {
            TypeExpr::SelfType => concrete.clone(),
            TypeExpr::Named(name, args) if name == "Self" && args.is_empty() => {
                concrete.clone()
            }
            TypeExpr::Slice(inner) => TypeExpr::Slice(Box::new(inner.replace_self(concrete))),
            TypeExpr::Array { size, ty } => TypeExpr::Array {
                size: *size,
                ty: Box::new(ty.replace_self(concrete)),
            },
            TypeExpr::Reference { mutable, ty } => TypeExpr::Reference {
                mutable: *mutable,
                ty: Box::new(ty.replace_self(concrete)),
            },
            TypeExpr::Pointer { mutable, ty } => TypeExpr::Pointer {
                mutable: *mutable,
                ty: Box::new(ty.replace_self(concrete)),
            },
            TypeExpr::Tuple(types) => {
                TypeExpr::Tuple(types.iter().map(|ty| ty.replace_self(concrete)).collect())
            }
            TypeExpr::Named(name, args) => TypeExpr::Named(
                name.clone(),
                args.iter().map(|ty| ty.replace_self(concrete)).collect(),
            ),
            TypeExpr::Unit => TypeExpr::Unit,
        }
    }

    pub fn canonical_name(&self) -> String {
        match self {
            TypeExpr::Named(name, args) => {
                if args.is_empty() {
                    name.clone()
                } else {
                    let rendered: Vec<String> = args.iter().map(|ty| ty.canonical_name()).collect();
                    format!("{}[{}]", name, rendered.join(","))
                }
            }
            TypeExpr::Slice(inner) => format!("[]{}", inner.canonical_name()),
            TypeExpr::Array { size, ty } => format!("[{};{}]", ty.canonical_name(), size),
            TypeExpr::Reference { mutable, ty } => {
                if *mutable {
                    format!("&mut {}", ty.canonical_name())
                } else {
                    format!("&{}", ty.canonical_name())
                }
            }
            TypeExpr::Pointer { mutable, ty } => {
                if *mutable {
                    format!("*mut {}", ty.canonical_name())
                } else {
                    format!("*{}", ty.canonical_name())
                }
            }
            TypeExpr::Tuple(types) => {
                let rendered: Vec<String> = types.iter().map(|ty| ty.canonical_name()).collect();
                format!("({})", rendered.join(","))
            }
            TypeExpr::Unit => "()".into(),
            TypeExpr::SelfType => "Self".into(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeAnnotation {
    pub ty: TypeExpr,
    pub span: Span,
}

impl TypeAnnotation {
    pub fn substitute(&self, map: &HashMap<String, TypeExpr>) -> TypeAnnotation {
        TypeAnnotation {
            ty: self.ty.substitute(map),
            span: self.span,
        }
    }

    pub fn replace_self(&self, concrete: &TypeExpr) -> TypeAnnotation {
        TypeAnnotation {
            ty: self.ty.replace_self(concrete),
            span: self.span,
        }
    }
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
