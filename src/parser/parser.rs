use crate::common::ast;
use crate::common::constants::{MAX_NUMBER_ARGS, THIS_STR};
use crate::common::operator::{InfixOperator, LogicalOperator, Precedence, PrefixOperator};
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
    ExpectedTokenAt(Token, CodePosition, Token),
    ExpectedExprAt(CodePosition, Token),
    ExpectedIdentifier(CodePosition),
    ExpectedLValue(CodePosition),
    TooManyArgs(CodePosition),
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
    fn peek_token(&mut self) -> SpannedToken {
        match self.tokens.peek() {
            Some(t) => t.clone(),
            None => panic!("Went beyond EOF"),
        }
    }

    /// Returns the current token and advances the stream
    fn take_token(&mut self) -> SpannedToken {
        match self.tokens.next() {
            Some(t) => t,
            None => panic!("Went beyond EOF"),
        }
    }

    /// Advances the stream, erroring if we're at EOF
    fn bump(&mut self) {
        match self.take_token().token {
            Token::EndOfFile => panic!("Bumped at EOF"),
            _ => (),
        }
    }

    /// Checks whether or not the current token matches the given token
    fn check(&mut self, t: Token) -> bool {
        return self.peek_token().token == t;
    }

    /// Checks whether or not the current token matches the given token,
    /// and if so, consumes it, returning true.
    fn try_eat(&mut self, t: Token) -> bool {
        if self.check(t) {
            self.bump();
            return true;
        }
        return false;
    }

    /// Same as try_eat, but returns an error if the token doesn't match.
    fn eat(&mut self, expected: Token) -> ParseResult<()> {
        let (token, span) = self.peek_token().split();

        if token == expected {
            self.bump();
            Ok(())
        } else {
            Err(Error::ExpectedTokenAt(expected, span.start_pos, token))
        }
    }

    // ---- parsing methods ----

    pub fn parse_all(mut self) -> Vec<ParseResult<ast::Stmt>> {
        let mut stmts = vec![];

        while !self.check(Token::EndOfFile) {
            let stmt = self.parse_declaration();
            // TODO: synchronize here
            stmts.push(stmt);
        }

        return stmts;
    }

    fn parse_declaration(&mut self) -> ParseResult<ast::Stmt> {
        let (token, _) = self.peek_token().split();
        match token {
            Token::Var => {
                self.bump();
                let name = self.parse_identifier()?;
                let expr = if self.try_eat(Token::Equals) {
                    self.parse_expression()?
                } else {
                    ast::Expr::NilLiteral
                };
                self.eat(Token::Semicolon)?;

                Ok(ast::Stmt::VariableDecl(name, expr))
            }
            Token::Fun => {
                let fn_data = self.parse_function_data(true)?;
                Ok(ast::Stmt::FunctionDecl(fn_data))
            }
            Token::Class => self.parse_class_decl(),
            _ => self.parse_statement(),
        }
    }

    fn parse_function_data(&mut self, leading_fun: bool) -> ParseResult<ast::FunctionData> {
        if leading_fun {
            self.eat(Token::Fun)?;
        }
        let name = self.parse_identifier()?;
        let params = self.parse_fn_params()?;
        let body = self.parse_block_statement()?;

        let fn_data = ast::FunctionData::new(name, params, body);
        Ok(fn_data)
    }

    fn parse_class_decl(&mut self) -> ParseResult<ast::Stmt> {
        self.eat(Token::Class)?;
        let name = self.parse_identifier()?;

        self.eat(Token::LeftBrace)?;

        let mut methods = vec![];
        while !self.try_eat(Token::RightBrace) {
            let method_data = self.parse_function_data(false)?;
            methods.push(method_data);
        }

        Ok(ast::Stmt::ClassDecl(name, methods))
    }

    fn parse_statement(&mut self) -> ParseResult<ast::Stmt> {
        let (token, _) = self.peek_token().split();
        match token {
            Token::Print => {
                self.bump();
                let expr = self.parse_expression()?;
                self.eat(Token::Semicolon)?;
                Ok(ast::Stmt::Print(expr))
            }
            Token::If => self.parse_if_else_statement(),
            Token::While => self.parse_while_statement(),
            Token::For => self.parse_for_statement(),
            Token::Return => self.parse_return_statement(),
            Token::LeftBrace => self.parse_block_statement(),
            _ => {
                let expr = self.parse_expression()?;
                self.eat(Token::Semicolon)?;
                Ok(ast::Stmt::Expression(expr))
            }
        }
    }

    fn parse_if_else_statement(&mut self) -> ParseResult<ast::Stmt> {
        self.eat(Token::If)?;
        self.eat(Token::LeftParen)?;
        let condition = self.parse_expression()?;
        self.eat(Token::RightParen)?;

        let body = Box::new(self.parse_statement()?);
        let else_body = if self.try_eat(Token::Else) {
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        Ok(ast::Stmt::IfElse(condition, body, else_body))
    }

    fn parse_while_statement(&mut self) -> ParseResult<ast::Stmt> {
        self.eat(Token::While)?;
        self.eat(Token::LeftParen)?;
        let condition = self.parse_expression()?;
        self.eat(Token::RightParen)?;

        let body = self.parse_statement()?;

        Ok(ast::Stmt::While(condition, Box::new(body)))
    }

    fn parse_for_statement(&mut self) -> ParseResult<ast::Stmt> {
        self.eat(Token::For)?;
        self.eat(Token::LeftParen)?;

        // Figure out the three parts of the loop; each is optional
        let initializer = if self.try_eat(Token::Semicolon) {
            None
        } else if self.check(Token::Var) {
            Some(self.parse_declaration()?)
        } else {
            let expr = self.parse_expression()?;
            self.eat(Token::Semicolon)?;
            Some(ast::Stmt::Expression(expr))
        };

        let condition = if self.check(Token::Semicolon) {
            ast::Expr::BooleanLiteral(true)
        } else {
            self.parse_expression()?
        };
        self.eat(Token::Semicolon)?;

        let increment = if self.check(Token::RightParen) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.eat(Token::RightParen)?;

        // Now we read the body, and modify it so that it has the semantics of
        // the for-loop.
        let mut body = self.parse_statement()?;

        if let Some(increment) = increment {
            body = ast::Stmt::Block(vec![body, ast::Stmt::Expression(increment)]);
        }
        body = ast::Stmt::While(condition, Box::new(body));
        if let Some(initializer) = initializer {
            body = ast::Stmt::Block(vec![initializer, body]);
        }

        Ok(body)
    }

    fn parse_return_statement(&mut self) -> ParseResult<ast::Stmt> {
        self.eat(Token::Return)?;
        let expr = if !self.check(Token::Semicolon) {
            self.parse_expression()?
        } else {
            ast::Expr::NilLiteral
        };

        self.eat(Token::Semicolon)?;
        Ok(ast::Stmt::Return(expr))
    }

    fn parse_block_statement(&mut self) -> ParseResult<ast::Stmt> {
        let mut stmts = vec![];

        self.eat(Token::LeftBrace)?;
        while !self.try_eat(Token::RightBrace) {
            stmts.push(self.parse_declaration()?);
        }

        Ok(ast::Stmt::Block(stmts))
    }

    fn parse_expression(&mut self) -> ParseResult<ast::Expr> {
        // We use pratt parsing to deal with expressions
        self.pratt_parse(Precedence::Lowest)
    }

    fn pratt_parse(&mut self, min_precedence: Precedence) -> ParseResult<ast::Expr> {
        // We parse the first operand, taking care of prefix expressions
        let (token, span) = self.take_token().split();
        let mut lhs = match token {
            // Literals
            Token::Number(n) => ast::Expr::NumberLiteral(n),
            Token::True => ast::Expr::BooleanLiteral(true),
            Token::False => ast::Expr::BooleanLiteral(false),
            Token::String(s) => ast::Expr::StringLiteral(s),
            Token::Nil => ast::Expr::NilLiteral,
            Token::Identifier(name) => ast::Expr::Variable(ast::VariableRef::new(name)),
            Token::This => ast::Expr::This(ast::VariableRef::new(THIS_STR.to_owned())),
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
            let (token, span) = self.peek_token().split();
            let span = span.to_owned(); // so we can drop the mutable borrow above

            if let Some(op) = InfixOperator::try_from_token(&token) {
                // Since arithmetic and equality operators are left-associative,
                // we should treat equal precedence as insufficient.
                if op.precedence() <= min_precedence {
                    break;
                }

                // Grab the operator and rhs and fold them into the new lhs
                self.bump();
                let rhs = self.pratt_parse(op.precedence())?;
                lhs = ast::Expr::Infix(op, Box::new(lhs), Box::new(rhs));
                continue;
            }

            // Is it a logic operator?
            if let Some(op) = LogicalOperator::try_from_token(&token) {
                // Logic operators are left-associative
                if op.precedence() <= min_precedence {
                    break;
                }

                self.bump();
                let rhs = self.pratt_parse(op.precedence())?;
                lhs = ast::Expr::Logical(op, Box::new(lhs), Box::new(rhs));
                continue;
            }

            // Is it assignment?
            if let Token::Equals = token {
                // Assignment is right-associative, so we recurse on equal precedence
                if Precedence::Assignment < min_precedence {
                    break;
                }

                // Grab the operator and parse the RHS
                self.bump();
                let rhs = self.pratt_parse(Precedence::Assignment)?;
                let rhs_box = Box::new(rhs);

                // The LHS must be converted to the appropriate format
                let temp = match lhs {
                    ast::Expr::Variable(var) => {
                        ast::Expr::Assignment(ast::VariableRef::new(var.name), rhs_box)
                    }
                    ast::Expr::Get(expr, property) => ast::Expr::Set(expr, property, rhs_box),
                    _ => return Err(Error::ExpectedLValue(span.start_pos)),
                };
                lhs = temp; // temp needed to appease the borrow checker
                continue;
            }

            // Is it a function call?
            if let Token::LeftParen = token {
                // Calling is left-associative
                if Precedence::Call < min_precedence {
                    break;
                }

                // Don't parse the parentheses, it'll get consumed by this function
                let arguments = self.parse_fn_args()?;

                lhs = ast::Expr::Call(Box::new(lhs), arguments);
                continue;
            }

            // Is it a dot?
            if let Token::Dot = token {
                // Property access is also left-associative
                if Precedence::Property < min_precedence {
                    break;
                }

                // Grab the operator
                self.bump();

                // THe RHS must be an identifier
                let rhs = self.parse_identifier()?;
                lhs = ast::Expr::Get(Box::new(lhs), rhs);
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    fn parse_identifier(&mut self) -> ParseResult<String> {
        let (token, span) = self.take_token().split();
        match token {
            Token::Identifier(name) => Ok(name.clone()),
            _ => Err(Error::ExpectedIdentifier(span.start_pos)),
        }
    }

    fn parse_fn_args(&mut self) -> ParseResult<Vec<ast::Expr>> {
        self.eat(Token::LeftParen)?;
        let mut args = vec![];

        // Check for the zero-argument case
        if self.try_eat(Token::RightParen) {
            return Ok(args);
        }

        // There must be at least one argument
        args.push(self.parse_expression()?);

        while !self.try_eat(Token::RightParen) {
            self.eat(Token::Comma)?;
            args.push(self.parse_expression()?);
        }

        // lox has a maximum number of arguments it supports
        if args.len() >= MAX_NUMBER_ARGS {
            let span = self.peek_token().span;
            return Err(Error::TooManyArgs(span.start_pos));
        }

        Ok(args)
    }

    fn parse_fn_params(&mut self) -> ParseResult<Vec<String>> {
        self.eat(Token::LeftParen)?;
        let mut args = vec![];

        // Check for the zero-argument case
        if self.try_eat(Token::RightParen) {
            return Ok(args);
        }

        // There must be at least one argument
        args.push(self.parse_identifier()?);

        while !self.try_eat(Token::RightParen) {
            self.eat(Token::Comma)?;
            args.push(self.parse_identifier()?);
        }

        // lox has a maximum number of arguments it supports
        if args.len() >= MAX_NUMBER_ARGS {
            let span = self.peek_token().span;
            return Err(Error::TooManyArgs(span.start_pos));
        }

        Ok(args)
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
