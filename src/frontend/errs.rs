use super::span::Span;
use super::token::Token;

pub const MAX_NUMBER_ARGS: usize = 256;

#[derive(Debug)]
pub enum Error {
    IllegalToken(Span, String),
    ExpectedTokenAt(Token, Span, Token),
    ExpectedExprAt(Span, Token),
    ExpectedIdentifier(Span),
    InvalidAssignment(Span),
    TooManyArgs(Span),
}

pub type ParseResult<T> = Result<T, Error>;

impl Error {
    pub fn render(&self, source: &str) -> String {
        match self {
            Error::IllegalToken(span, string) => {
                format!("Illegal token {} on line {}", string, span.lo.line_no)
            }
            Error::ExpectedTokenAt(expected, span, got) => {
                format!(
                    "Expected {:?} on line {}, got {:?}",
                    expected, span.lo.line_no, got
                )
            }
            Error::ExpectedExprAt(span, _) => {
                format!(
                    "Error at '{}': Expect expression.",
                    span.extract_string(&source).unwrap(),
                )
            }
            Error::ExpectedIdentifier(span) => {
                format!(
                    "Error at '{}': Expect variable name.",
                    span.extract_string(source).unwrap()
                )
            }
            Error::InvalidAssignment(span) => {
                format!(
                    "Error at '{}': Invalid assignment target.",
                    span.extract_string(source).unwrap()
                )
            }
            Error::TooManyArgs(span) => {
                format!(
                    "Error at '{}': Can't have more than {} parameters",
                    span.extract_string(source).unwrap(),
                    MAX_NUMBER_ARGS,
                )
            }
        }
    }
}
