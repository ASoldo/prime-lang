use crate::language::span::Span;

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Identifier(String),
    Integer(i128),
    Float(f64),
    String(String),
    Rune(char),

    Fn,
    Let,
    Mut,
    Const,
    Struct,
    Enum,
    Interface,
    Impl,
    Import,
    Using,
    ModuleKw,
    Return,
    If,
    Else,
    For,
    In,
    While,
    Match,
    Break,
    Continue,
    Defer,
    Try,
    Move,
    Pub,
    True,
    False,
    As,

    Ampersand,
    AmpersandAmpersand,
    PipePipe,
    Pipe,
    Caret,
    Bang,
    BangEq,
    Eq,
    EqEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Dot,
    DotDot,
    DotDotEq,
    Comma,
    Colon,
    ColonColon,
    Semi,
    Question,
    At,

    Arrow,    // ->
    FatArrow, // =>

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Eof,
}

impl TokenKind {}
