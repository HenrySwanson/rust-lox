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
    UnclosedBrace(Span),
    FunctionBodyStart(Span),
    SemiAfterExpression(Span),
    ExpectSuperDot(Span),
    ExpectSuperMethod(Span),
    ExpectPropertyName(Span),
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
                    "{}: Expect expression.",
                    get_error_prefix(span, source),
                )
            }
            Error::ExpectedIdentifier(span) => {
                format!(
                    "{}: Expect variable name.",
                    get_error_prefix(span, source),
                )
            }
            Error::InvalidAssignment(span) => {
                format!(
                    "{}: Invalid assignment target.",
                    get_error_prefix(span, source),
                )
            }
            Error::TooManyArgs(span) => {
                format!(
                    "{}: Can't have more than {} parameters",
                    get_error_prefix(span, source),
                    MAX_NUMBER_ARGS,
                )
            }
            Error::UnclosedBrace(span) => {
                format!("{}: Expected }}", get_error_prefix(span, source))
            }
            Error::FunctionBodyStart(span) => format!(
                "{}: Expect '{{' before function body.",
                get_error_prefix(span, source),
            ),
            Error::SemiAfterExpression(span) => format!(
                "{}: Expect ';' after expression.",
                get_error_prefix(span, source),
            ),
            Error::ExpectSuperDot(span) => format!(
                "{}: Expect '.' after 'super'.",
                get_error_prefix(span, source),
            ),
            Error::ExpectSuperMethod(span) => format!(
                "{}: Expect superclass method name.",
                get_error_prefix(span, source),
            ),
            Error::ExpectPropertyName(span) => format!(
                "{}: Expect property name after '.'.",
                get_error_prefix(span, source),
            ),
        }
    }
}

fn get_error_prefix(span: &Span, source: &str) -> String {
    // We include a hack to get EOF spans to say 'at end'. Probably should
    // live in Span itself, but refactoring is easier once all tests pass,
    // y'know?
    if span.lo.byte_pos < source.len() {
        format!("Error at '{}'", span.extract_string(source).unwrap())
    } else {
        String::from("Error at end")
    }
}
