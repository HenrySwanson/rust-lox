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
    EndOfFile,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

impl SpannedToken {
    pub fn new(token: Token, span: Span) -> Self {
        SpannedToken { token, span }
    }
}
