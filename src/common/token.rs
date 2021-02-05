use super::span::Span;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Dot,
    Comma,
    Semicolon,

    // One or two character tokens
    Bang,
    BangEq,
    Equals,
    DoubleEq,
    LeftAngle,
    LeftAngleEq,
    RightAngle,
    RightAngleEq,

    // Literals
    Identifier(String),
    String(String),
    Number(u32), // TODO f64, but that doesn't work with Eq

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // Miscellanous
    Error(String),
    EndOfFile,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}
