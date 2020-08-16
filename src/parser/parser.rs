use crate::common::ast;
use crate::common::operator::{InfixOperator, Precedence, PrefixOperator};
use crate::common::span::CodePosition;
use crate::common::token::{SpannedToken, Token};
use std::iter::Peekable;

pub struct Parser<T>
where
    T: Iterator<Item = SpannedToken>,
{
    tokens: Peekable<T>,
}

#[derive(Debug)]
pub enum Error {
    WentBeyondEof,
    ExpectedTokenAt(Token, CodePosition, Token),
    ExpectedExprAt(CodePosition, Token),
}

pub type ParseResult<T> = Result<T, Error>;

impl<T> Parser<T>
where
    T: Iterator<Item = SpannedToken>,
{
    pub fn new(tokens: T) -> Self {
        Parser {
            tokens: tokens.peekable(),
        }
    }

    // ---- Simple token-based operations ----

    /// Returns the current token
    fn peek_token(&mut self) -> ParseResult<&SpannedToken> {
        match self.tokens.peek() {
            Some(t) => Ok(&t),
            None => Err(Error::WentBeyondEof),
        }
    }

    /// Returns the current token and advances the stream
    fn take_token(&mut self) -> ParseResult<SpannedToken> {
        match self.tokens.next() {
            Some(t) => Ok(t),
            None => Err(Error::WentBeyondEof),
        }
    }

    /// Advances the stream, erroring if we're at EOF
    fn bump(&mut self) -> ParseResult<()> {
        match self.take_token()?.token {
            Token::EndOfFile => Err(Error::WentBeyondEof),
            _ => Ok(()),
        }
    }

    /// Checks whether or not the current token matches the given token
    fn check(&mut self, t: Token) -> ParseResult<bool> {
        return Ok(self.peek_token()?.token == t);
    }

    /// Checks whether or not the current token matches the given token,
    /// and if so, consumes it, returning true.
    fn try_eat(&mut self, t: Token) -> ParseResult<bool> {
        if self.check(t)? {
            self.bump()?;
            return Ok(true);
        }
        return Ok(false);
    }

    /// Consumes a token, asserting that it equals the expected token
    fn eat(&mut self, expected: Token) -> ParseResult<()> {
        let (token, span) = self.take_token()?.split();

        if token == expected {
            Ok(())
        } else {
            Err(Error::ExpectedTokenAt(expected, span.start_pos, token))
        }
    }

    // ---- parsing methods ----

    pub fn parse_expression(&mut self) -> ParseResult<ast::Expr> {
        // We use pratt parsing to deal with this
        self.pratt_parse(Precedence::Lowest)
    }

    fn pratt_parse(&mut self, min_precedence: Precedence) -> ParseResult<ast::Expr> {
        // We parse the first operand, taking care of prefix expressions
        let (token, span) = self.take_token()?.split();
        let mut lhs = match token {
            // Literals
            Token::Number(n) => ast::Expr::NumberLiteral(n),
            Token::True => ast::Expr::BooleanLiteral(true),
            Token::False => ast::Expr::BooleanLiteral(false),
            Token::String(s) => ast::Expr::StringLiteral(s),
            Token::Nil => ast::Expr::NilLiteral,
            // Parentheses
            Token::LeftParen => {
                let expr = self.parse_expression()?;
                self.eat(Token::RightParen)?;
                expr
            }
            // Well it had better be a prefix operator then
            t => match PrefixOperator::try_from_token(&t) {
                Some(op) => {
                    let expr = self.pratt_parse(op.precedence())?;
                    ast::Expr::Prefix(op, Box::new(expr))
                }
                None => return Err(Error::ExpectedExprAt(span.start_pos, t)),
            },
        };

        // Now we start consuming infix operators
        loop {
            // Is it an infix operator?
            let token = &self.peek_token()?.token;
            if let Some(op) = InfixOperator::try_from_token(token) {
                // Since all operators are left-associative, we should treat equal precedence
                // as insufficient.
                if op.precedence() <= min_precedence {
                    break;
                }

                // Grab the operator and rhs and fold them into the new lhs
                self.bump()?;
                let rhs = self.pratt_parse(op.precedence())?;
                lhs = ast::Expr::Infix(op, Box::new(lhs), Box::new(rhs));
                continue;
            }

            break;
        }

        Ok(lhs)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::common::ast::Expr;
    use crate::common::operator::{InfixOperator, PrefixOperator};
    use crate::common::token::SpannedToken;
    use crate::lexer::Lexer;

    fn lex_source(source: &str) -> Vec<SpannedToken> {
        let lexer = Lexer::new(source);
        lexer.iter().map(|r| r.unwrap()).collect()
    }

    #[test]
    fn expression_parsing() {
        fn parse_expression(source: &str) -> Expr {
            let mut parser = Parser::new(lex_source(source).into_iter());
            parser.parse_expression().unwrap()
        }

        assert_eq!(
            parse_expression("3+4"),
            Expr::Infix(
                InfixOperator::Add,
                Box::new(Expr::NumberLiteral(3)),
                Box::new(Expr::NumberLiteral(4))
            )
        );

        assert_eq!(
            parse_expression("3 + 1 * 5 - 4"),
            Expr::Infix(
                InfixOperator::Subtract,
                Box::new(Expr::Infix(
                    InfixOperator::Add,
                    Box::new(Expr::NumberLiteral(3)),
                    Box::new(Expr::Infix(
                        InfixOperator::Multiply,
                        Box::new(Expr::NumberLiteral(1)),
                        Box::new(Expr::NumberLiteral(5))
                    ))
                )),
                Box::new(Expr::NumberLiteral(4))
            )
        );

        assert_eq!(
            parse_expression("-3 * (1 + 2)"),
            Expr::Infix(
                InfixOperator::Multiply,
                Box::new(Expr::Prefix(
                    PrefixOperator::Negate,
                    Box::new(Expr::NumberLiteral(3))
                )),
                Box::new(Expr::Infix(
                    InfixOperator::Add,
                    Box::new(Expr::NumberLiteral(1)),
                    Box::new(Expr::NumberLiteral(2))
                ))
            )
        );
    }
}
