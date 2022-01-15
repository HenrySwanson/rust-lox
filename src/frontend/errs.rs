use super::span::Span;
use super::token::Token;

pub const MAX_NUMBER_ARGS: usize = 255;

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub kind: ErrorKind,
}

#[derive(Debug)]
pub enum ErrorKind {
    InvalidToken(String),
    ExpectedExprAt(Token),
    ExpectedIdentifier,
    InvalidAssignment,
    TooManyArgs,
    TooManyParams,
    UnclosedBrace,
    ExpectCommaBetween,
    ExpectSuperDot,
    ExpectSuperMethod,
    ExpectSuperclassName,
    ExpectPropertyName,
    ExpectBefore(&'static str, Item),
    ExpectAfter(&'static str, Item),
}

#[derive(Debug)]
pub enum Item {
    VariableDecl,
    FunctionBody,
    Expression,
    ClassBody,
    PrintValue,
    ReturnValue,
    FunctionName,
    If,
    While,
    For,
    Condition,
    ForClause,
}

pub type ParseResult<T> = Result<T, Error>;

impl Error {
    pub fn render(&self, source: &str) -> String {
        let span = &self.span;
        match &self.kind {
            ErrorKind::InvalidToken(msg) => {
                format!("Error: {}.", msg)
            }
            ErrorKind::ExpectedExprAt(_) => {
                format!("{}: Expect expression.", get_error_prefix(span, source))
            }
            ErrorKind::ExpectedIdentifier => {
                format!("{}: Expect variable name.", get_error_prefix(span, source))
            }
            ErrorKind::InvalidAssignment => {
                format!(
                    "{}: Invalid assignment target.",
                    get_error_prefix(span, source),
                )
            }
            ErrorKind::TooManyArgs => {
                format!(
                    "{}: Can't have more than {} arguments.",
                    get_error_prefix(span, source),
                    MAX_NUMBER_ARGS,
                )
            }
            ErrorKind::TooManyParams => {
                format!(
                    "{}: Can't have more than {} parameters.",
                    get_error_prefix(span, source),
                    MAX_NUMBER_ARGS,
                )
            }
            ErrorKind::UnclosedBrace => {
                format!("{}: Expected }}", get_error_prefix(span, source))
            }
            ErrorKind::ExpectSuperDot => format!(
                "{}: Expect '.' after 'super'.",
                get_error_prefix(span, source),
            ),
            ErrorKind::ExpectSuperMethod => format!(
                "{}: Expect superclass method name.",
                get_error_prefix(span, source),
            ),
            ErrorKind::ExpectSuperclassName => format!(
                "{}: Expect superclass name.",
                get_error_prefix(span, source),
            ),
            ErrorKind::ExpectPropertyName => format!(
                "{}: Expect property name after '.'.",
                get_error_prefix(span, source),
            ),
            ErrorKind::ExpectCommaBetween => format!(
                "{}: Expect ',' between elements.",
                get_error_prefix(span, source),
            ),
            ErrorKind::ExpectBefore(expected, previous) => format!(
                "{}: Expect '{}' before {}.",
                get_error_prefix(span, source),
                expected,
                previous.as_str(),
            ),
            ErrorKind::ExpectAfter(expected, previous) => format!(
                "{}: Expect '{}' after {}.",
                get_error_prefix(span, source),
                expected,
                previous.as_str(),
            ),
        }
    }
}

impl Item {
    pub fn as_str(&self) -> &'static str {
        match self {
            Item::VariableDecl => "variable declaration",
            Item::FunctionBody => "function body",
            Item::Expression => "expression",
            Item::ClassBody => "class body",
            Item::PrintValue => "value",
            Item::ReturnValue => "return value",
            Item::FunctionName => "function name",
            Item::If => "'if'",
            Item::While => "'while'",
            Item::For => "'for'",
            Item::Condition => "condition",
            Item::ForClause => "for clauses",
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
