use crate::language::{
    span::{Span, Spanned},
    types::{Mutability, TypeAnnotation, TypeExpr},
};
use std::path::PathBuf;

#[derive(Clone, Debug)]
pub struct Program {
    pub modules: Vec<Module>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ModuleKind {
    Module,
    Library,
    Test,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub name: String,
    pub kind: ModuleKind,
    pub path: PathBuf,
    pub declared_name: Option<String>,
    pub declared_span: Option<Span>,
    pub redundant_module_spans: Vec<Span>,
    pub imports: Vec<Import>,
    pub items: Vec<Item>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

impl Default for Visibility {
    fn default() -> Self {
        Visibility::Private
    }
}

#[derive(Clone, Debug)]
pub struct Import {
    pub visibility: Visibility,
    pub path: ImportPath,
    pub alias: Option<String>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ImportPath {
    pub segments: Vec<String>,
}

impl ImportPath {
    pub fn to_string(&self) -> String {
        self.segments.join("::")
    }

    pub fn to_relative_path(&self) -> PathBuf {
        let mut buf = PathBuf::new();
        for segment in &self.segments {
            buf.push(segment);
        }
        buf
    }

    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }
}

#[derive(Clone, Debug)]
pub enum Item {
    Struct(StructDef),
    Enum(EnumDef),
    Interface(InterfaceDef),
    Impl(ImplBlock),
    Function(FunctionDef),
    Const(ConstDef),
}

#[derive(Clone, Debug)]
pub struct StructDef {
    pub name: String,
    pub type_params: Vec<String>,
    pub fields: Vec<StructField>,
    pub span: Span,
    pub visibility: Visibility,
}

#[derive(Clone, Debug)]
pub struct StructField {
    pub name: Option<String>,
    pub ty: TypeAnnotation,
    pub embedded: bool,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct EnumDef {
    pub name: String,
    pub type_params: Vec<String>,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
    pub visibility: Visibility,
}

#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Vec<TypeAnnotation>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct InterfaceDef {
    pub name: String,
    pub type_params: Vec<String>,
    pub methods: Vec<InterfaceMethod>,
    pub span: Span,
    pub visibility: Visibility,
}

#[derive(Clone, Debug)]
pub struct InterfaceMethod {
    pub name: String,
    pub params: Vec<FunctionParam>,
    pub returns: Vec<TypeAnnotation>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ImplBlock {
    pub interface: String,
    pub type_args: Vec<TypeExpr>,
    pub target: String,
    pub methods: Vec<FunctionDef>,
}

#[derive(Clone, Debug)]
pub struct FunctionDef {
    pub name: String,
    pub type_params: Vec<String>,
    pub params: Vec<FunctionParam>,
    pub returns: Vec<TypeAnnotation>,
    pub body: FunctionBody,
    pub span: Span,
    pub visibility: Visibility,
}

#[derive(Clone, Debug)]
pub struct FunctionParam {
    pub name: String,
    pub ty: TypeAnnotation,
    pub mutability: Mutability,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum FunctionBody {
    Block(Box<Block>),
    Expr(Spanned<Expr>),
}

#[derive(Clone, Debug)]
pub struct ConstDef {
    pub name: String,
    pub ty: Option<TypeAnnotation>,
    pub value: Expr,
    pub span: Span,
    pub visibility: Visibility,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub tail: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Let(LetStmt),
    Assign(AssignStmt),
    Expr(ExprStmt),
    Return(ReturnStmt),
    While(WhileStmt),
    Loop(LoopStmt),
    For(ForStmt),
    Defer(DeferStmt),
    Break,
    Continue,
    Block(Box<Block>),
}

#[derive(Clone, Debug)]
pub struct LetStmt {
    pub pattern: Pattern,
    pub ty: Option<TypeAnnotation>,
    pub value: Option<Expr>,
    pub mutability: Mutability,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct AssignStmt {
    pub target: Expr,
    pub value: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct ReturnStmt {
    pub values: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct WhileStmt {
    pub condition: WhileCondition,
    pub body: Block,
}

#[derive(Clone, Debug)]
pub struct LoopStmt {
    pub body: Block,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum WhileCondition {
    Expr(Expr),
    Let { pattern: Pattern, value: Expr },
}

#[derive(Clone, Debug)]
pub struct ForStmt {
    pub binding: String,
    pub target: ForTarget,
    pub body: Block,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ForTarget {
    Range(RangeExpr),
    Collection(Expr),
}

#[derive(Clone, Debug)]
pub struct DeferStmt {
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub enum Pattern {
    Wildcard,
    Identifier(String, Span),
    Literal(Literal),
    EnumVariant {
        enum_name: Option<String>,
        variant: String,
        bindings: Vec<Pattern>,
    },
    Tuple(Vec<Pattern>, Span),
    Map(Vec<MapPatternEntry>, Span),
    Struct {
        struct_name: Option<String>,
        fields: Vec<StructPatternField>,
        has_spread: bool,
        span: Span,
    },
    Slice {
        prefix: Vec<Pattern>,
        rest: Option<Box<Pattern>>, // captures ..rest
        suffix: Vec<Pattern>,
        span: Span,
    },
}

#[derive(Clone, Debug)]
pub struct MapPatternEntry {
    pub key: String,
    pub pattern: Pattern,
}

#[derive(Clone, Debug)]
pub struct StructPatternField {
    pub name: String,
    pub pattern: Pattern,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Identifier(Identifier),
    Literal(Literal),
    FormatString(FormatStringLiteral),
    Try {
        block: Box<Block>,
        span: Span,
    },
    TryPropagate {
        expr: Box<Expr>,
        span: Span,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
        span: Span,
    },
    Call {
        callee: Box<Expr>,
        type_args: Vec<TypeExpr>,
        args: Vec<Expr>,
        span: Span,
    },
    FieldAccess {
        base: Box<Expr>,
        field: String,
        span: Span,
    },
    EnumLiteral {
        enum_name: Option<String>,
        variant: String,
        values: Vec<Expr>,
        span: Span,
    },
    StructLiteral {
        name: String,
        fields: StructLiteralKind,
        span: Span,
    },
    MapLiteral {
        entries: Vec<MapLiteralEntry>,
        span: Span,
    },
    Block(Box<Block>),
    If(Box<IfExpr>),
    Match(MatchExpr),
    Tuple(Vec<Expr>, Span),
    ArrayLiteral(Vec<Expr>, Span),
    Range(RangeExpr),
    Reference {
        mutable: bool,
        expr: Box<Expr>,
        span: Span,
    },
    Deref {
        expr: Box<Expr>,
        span: Span,
    },
    Move {
        expr: Box<Expr>,
        span: Span,
    },
    Spawn {
        expr: Box<Expr>,
        span: Span,
    },
}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Literal {
    Int(i128, Span),
    Float(f64, Span),
    Bool(bool, Span),
    String(String, Span),
    Rune(char, Span),
}

#[derive(Clone, Debug)]
pub struct FormatStringLiteral {
    pub segments: Vec<FormatSegment>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum FormatSegment {
    Literal(String),
    Implicit(Span),
    Expr { expr: Expr, span: Span },
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Clone, Debug)]
pub struct IfExpr {
    pub condition: IfCondition,
    pub then_branch: Block,
    pub else_branch: Option<ElseBranch>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum IfCondition {
    Expr(Expr),
    Let { pattern: Pattern, value: Expr },
}

#[derive(Clone, Debug)]
pub enum ElseBranch {
    Block(Block),
    ElseIf(Box<IfExpr>),
}

#[derive(Clone, Debug)]
pub struct MatchExpr {
    pub expr: Box<Expr>,
    pub arms: Vec<MatchArmExpr>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct MatchArmExpr {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub value: Expr,
}

#[derive(Clone, Debug)]
pub struct RangeExpr {
    pub start: Box<Expr>,
    pub end: Box<Expr>,
    pub inclusive: bool,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StructLiteralKind {
    Named(Vec<StructLiteralField>),
    Positional(Vec<Expr>),
}

#[derive(Clone, Debug)]
pub struct StructLiteralField {
    pub name: String,
    pub value: Expr,
}

#[derive(Clone, Debug)]
pub struct MapLiteralEntry {
    pub key: Expr,
    pub value: Expr,
}
