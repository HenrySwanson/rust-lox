use super::span::Span;
use super::token::Token;

use std::fmt;

pub const MAX_NUMBER_ARGS: usize = 256;

#[derive(Debug)]
pub enum Error {
    IllegalToken(Span, String),
    ExpectedTokenAt(Token, Span, Token),
    ExpectedExprAt(Span, Token),
    ExpectedIdentifier(Span),
    ExpectedLValue(Span),
    TooManyArgs(Span),
}

pub type ParseResult<T> = Result<T, Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::IllegalToken(span, string) => {
                write!(f, "Illegal token {} on line {}", string, span.lo.line_no)
            }
            Error::ExpectedTokenAt(expected, span, got) => {
                write!(
                    f,
                    "Expected {:?} on line {}, got {:?}",
                    expected, span.lo.line_no, got
                )
            }
            Error::ExpectedExprAt(span, got) => {
                write!(
                    f,
                    "Expected expression on line {}, got {:?}",
                    span.lo.line_no, got
                )
            }
            Error::ExpectedIdentifier(span) => {
                write!(f, "Expected identifier on line {}", span.lo.line_no)
            }
            Error::ExpectedLValue(span) => {
                write!(
                    f,
                    "Expected something assignable on the LHS on line {}",
                    span.lo.line_no
                )
            }
            Error::TooManyArgs(span) => {
                write!(
                    f,
                    "Too many arguments to function on line {}. Lox supports only {} arguments",
                    span.lo.line_no, MAX_NUMBER_ARGS,
                )
            }
        }
    }
}
