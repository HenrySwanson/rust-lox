use crate::common::ast;
use crate::common::constants::{MAX_NUMBER_ARGS, SUPER_STR, THIS_STR};
use crate::common::operator::{InfixOperator, LogicalOperator, Precedence, PrefixOperator};
use crate::common::span::{CodePosition, Span};
use crate::common::token::{SpannedToken, Token};
use std::iter::Peekable;

pub struct Parser<T>
where
    T: Iterator<Item = SpannedToken>,
{
    tokens: Peekable<T>,
    prev_span: Span,
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
            prev_span: Span::dummy(),
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
            Some(t) => {
                self.prev_span = t.span;
                t
            }
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
        let token = self.peek_token();

        if token.token == expected {
            self.bump();
            Ok(())
        } else {
            Err(Error::ExpectedTokenAt(expected, token.span.lo, token.token))
        }
    }

    fn current_span(&mut self) -> Span {
        self.peek_token().span
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
        let token = self.peek_token();
        let lo = token.span;

        match token.token {
            Token::Var => {
                self.bump();
                let name = self.parse_identifier()?;
                let expr = if self.try_eat(Token::Equals) {
                    self.parse_expression()?
                } else {
                    mk_expr(from_lit(ast::Literal::Nil), Span::dummy())
                };
                self.eat(Token::Semicolon)?;

                Ok(mk_stmt(
                    ast::StmtKind::VariableDecl(name, expr),
                    lo.to(self.prev_span),
                ))
            }
            Token::Fun => {
                let fn_data = self.parse_function_data(true)?;
                Ok(mk_stmt(
                    ast::StmtKind::FunctionDecl(fn_data),
                    lo.to(self.prev_span),
                ))
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
        let lo = self.current_span();

        self.eat(Token::Class)?;
        let name = self.parse_identifier()?;

        let superclass = if self.try_eat(Token::LeftAngle) {
            Some(ast::VariableRef::new(self.parse_identifier()?))
        } else {
            None
        };

        self.eat(Token::LeftBrace)?;

        let mut methods = vec![];
        while !self.try_eat(Token::RightBrace) {
            let method_data = self.parse_function_data(false)?;
            methods.push(method_data);
        }

        Ok(mk_stmt(
            ast::StmtKind::ClassDecl(name, superclass, methods),
            lo.to(self.prev_span),
        ))
    }

    fn parse_statement(&mut self) -> ParseResult<ast::Stmt> {
        let token = self.peek_token();
        let lo = token.span;

        match token.token {
            Token::Print => {
                self.bump();
                let expr = self.parse_expression()?;
                self.eat(Token::Semicolon)?;
                Ok(mk_stmt(ast::StmtKind::Print(expr), lo.to(self.prev_span)))
            }
            Token::If => self.parse_if_else_statement(),
            Token::While => self.parse_while_statement(),
            Token::For => self.parse_for_statement(),
            Token::Return => self.parse_return_statement(),
            Token::LeftBrace => self.parse_block_statement(),
            _ => {
                let expr = self.parse_expression()?;
                self.eat(Token::Semicolon)?;
                Ok(mk_stmt(
                    ast::StmtKind::Expression(expr),
                    lo.to(self.prev_span),
                ))
            }
        }
    }

    fn parse_if_else_statement(&mut self) -> ParseResult<ast::Stmt> {
        let lo = self.current_span();

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

        Ok(mk_stmt(
            ast::StmtKind::IfElse(condition, body, else_body),
            lo.to(self.prev_span),
        ))
    }

    fn parse_while_statement(&mut self) -> ParseResult<ast::Stmt> {
        let lo = self.current_span();

        self.eat(Token::While)?;
        self.eat(Token::LeftParen)?;
        let condition = self.parse_expression()?;
        self.eat(Token::RightParen)?;

        let body = self.parse_statement()?;

        Ok(mk_stmt(
            ast::StmtKind::While(condition, Box::new(body)),
            lo.to(self.prev_span),
        ))
    }

    fn parse_for_statement(&mut self) -> ParseResult<ast::Stmt> {
        // TODO: this one's harder
        let lo = self.current_span();

        self.eat(Token::For)?;
        self.eat(Token::LeftParen)?;

        // Figure out the three parts of the loop; each is optional
        let initializer = if self.try_eat(Token::Semicolon) {
            None
        } else if self.check(Token::Var) {
            Some(self.parse_declaration()?)
        } else {
            let lo = self.current_span();
            let expr = self.parse_expression()?;
            self.eat(Token::Semicolon)?;
            Some(mk_stmt(
                ast::StmtKind::Expression(expr),
                lo.to(self.prev_span),
            ))
        };

        let condition = if self.check(Token::Semicolon) {
            mk_expr(from_lit(ast::Literal::Boolean(true)), Span::dummy())
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

        // Should I use dummy spans here, or try to build reasonable approximations?
        if let Some(increment) = increment {
            let increment_stmt = mk_stmt(ast::StmtKind::Expression(increment), Span::dummy());
            body = mk_stmt(
                ast::StmtKind::Block(vec![body, increment_stmt]),
                Span::dummy(),
            );
        }
        body = mk_stmt(
            ast::StmtKind::While(condition, Box::new(body)),
            Span::dummy(),
        );
        if let Some(initializer) = initializer {
            body = mk_stmt(
                ast::StmtKind::Block(vec![initializer, body]),
                lo.to(self.prev_span),
            );
        }

        Ok(body)
    }

    fn parse_return_statement(&mut self) -> ParseResult<ast::Stmt> {
        let lo = self.current_span();

        self.eat(Token::Return)?;
        let expr = if !self.check(Token::Semicolon) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.eat(Token::Semicolon)?;
        Ok(mk_stmt(ast::StmtKind::Return(expr), lo.to(self.prev_span)))
    }

    fn parse_block_statement(&mut self) -> ParseResult<ast::Stmt> {
        let lo = self.current_span();

        let mut stmts = vec![];

        self.eat(Token::LeftBrace)?;
        while !self.try_eat(Token::RightBrace) {
            stmts.push(self.parse_declaration()?);
        }

        // TODO replace mk_stmt with with_span
        Ok(mk_stmt(ast::StmtKind::Block(stmts), lo.to(self.prev_span)))
    }

    fn parse_expression(&mut self) -> ParseResult<ast::Expr> {
        // We use pratt parsing to deal with expressions
        self.pratt_parse(Precedence::Lowest)
    }

    fn pratt_parse(&mut self, min_precedence: Precedence) -> ParseResult<ast::Expr> {
        // We parse the first operand, taking care of prefix expressions
        let SpannedToken { token, span } = self.peek_token();

        // Check if it's a prefix expression
        let mut lhs = match PrefixOperator::try_from_token(&token) {
            Some(op) => {
                self.bump();
                let expr = self.pratt_parse(op.precedence())?;
                mk_expr(
                    ast::ExprKind::Prefix(op, Box::new(expr)),
                    span.to(self.prev_span),
                )
            }
            None => self.parse_primary()?,
        };

        // Now we start consuming infix operators
        loop {
            let token = self.peek_token().token;

            // Is it an infix operator?
            if let Some(op) = InfixOperator::try_from_token(&token) {
                // Since arithmetic and equality operators are left-associative,
                // we should treat equal precedence as insufficient.
                if op.precedence() <= min_precedence {
                    break;
                }

                // Grab the operator and rhs and fold them into the new lhs
                self.bump();
                let rhs = self.pratt_parse(op.precedence())?;
                let new_span = lhs.span.to(rhs.span);
                lhs = mk_expr(
                    ast::ExprKind::Infix(op, Box::new(lhs), Box::new(rhs)),
                    new_span,
                );
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
                let new_span = lhs.span.to(rhs.span);
                lhs = mk_expr(
                    ast::ExprKind::Logical(op, Box::new(lhs), Box::new(rhs)),
                    new_span,
                );
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
                let new_span = lhs.span.to(rhs.span);
                let rhs_box = Box::new(rhs);

                // The LHS must be converted to the appropriate format
                let new_kind = match lhs.kind {
                    ast::ExprKind::Variable(var) => {
                        ast::ExprKind::Assignment(ast::VariableRef::new(var.name), rhs_box)
                    }
                    ast::ExprKind::Get(expr, property) => {
                        ast::ExprKind::Set(expr, property, rhs_box)
                    }
                    _ => return Err(Error::ExpectedLValue(span.lo)),
                };
                lhs = mk_expr(new_kind, new_span);
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
                let span = lhs.span.to(self.prev_span);

                lhs = mk_expr(ast::ExprKind::Call(Box::new(lhs), arguments), span);
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

                // The RHS must be an identifier
                let rhs = self.parse_identifier()?;
                let span = lhs.span.to(self.prev_span);

                lhs = mk_expr(ast::ExprKind::Get(Box::new(lhs), rhs), span);
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    fn parse_primary(&mut self) -> ParseResult<ast::Expr> {
        let token = self.take_token();
        let lo = token.span;

        let expr_kind = match token.token {
            // Literals
            Token::Number(n) => from_lit(ast::Literal::Number(n)),
            Token::True => from_lit(ast::Literal::Boolean(true)),
            Token::False => from_lit(ast::Literal::Boolean(false)),
            Token::String(s) => from_lit(ast::Literal::Str(s)),
            Token::Nil => from_lit(ast::Literal::Nil),
            // Other things
            Token::Identifier(name) => ast::ExprKind::Variable(ast::VariableRef::new(name)),
            Token::This => {
                let var = ast::VariableRef::new(THIS_STR.to_owned());
                ast::ExprKind::This(var)
            }
            Token::Super => {
                let var = ast::VariableRef::new(SUPER_STR.to_owned());
                self.eat(Token::Dot)?;
                let method_name = self.parse_identifier()?;
                ast::ExprKind::Super(var, method_name)
            }
            // Parentheses
            Token::LeftParen => {
                let expr = self.parse_expression()?;
                self.eat(Token::RightParen)?;
                return Ok(expr);
            }
            t => return Err(Error::ExpectedExprAt(lo.lo, t)),
        };
        Ok(mk_expr(expr_kind, lo.to(self.prev_span)))
    }

    fn parse_identifier(&mut self) -> ParseResult<String> {
        let token = self.take_token();
        match token.token {
            Token::Identifier(name) => Ok(name.clone()),
            _ => Err(Error::ExpectedIdentifier(token.span.lo)),
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
            let span = self.current_span();
            return Err(Error::TooManyArgs(span.lo));
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
            let span = self.current_span();
            return Err(Error::TooManyArgs(span.lo));
        }

        Ok(args)
    }
}

fn from_lit(lit: ast::Literal) -> ast::ExprKind {
    ast::ExprKind::Literal(lit)
}

fn mk_stmt(kind: ast::StmtKind, span: Span) -> ast::Stmt {
    ast::Stmt::new(kind, span)
}

fn mk_expr(kind: ast::ExprKind, span: Span) -> ast::Expr {
    ast::Expr::new(kind, span)
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::common::ast::{Expr, Literal};
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
            ExprKind::Infix(
                InfixOperator::Add,
                Box::new(ExprKind::Literal(Literal::Number(3))),
                Box::new(ExprKind::Literal(Literal::Number(4)))
            )
        );

        assert_eq!(
            parse_expression("3 + 1 * 5 - 4"),
            ExprKind::Infix(
                InfixOperator::Subtract,
                Box::new(ExprKind::Infix(
                    InfixOperator::Add,
                    Box::new(ExprKind::Literal(Literal::Number(3))),
                    Box::new(ExprKind::Infix(
                        InfixOperator::Multiply,
                        Box::new(ExprKind::Literal(Literal::Number(1))),
                        Box::new(ExprKind::Literal(Literal::Number(5)))
                    ))
                )),
                Box::new(ExprKind::Literal(Literal::Number(4)))
            )
        );

        assert_eq!(
            parse_expression("-3 * (1 + 2)"),
            ExprKind::Infix(
                InfixOperator::Multiply,
                Box::new(ExprKind::Prefix(
                    PrefixOperator::Negate,
                    Box::new(ExprKind::Literal(Literal::Number(3)))
                )),
                Box::new(ExprKind::Infix(
                    InfixOperator::Add,
                    Box::new(ExprKind::Literal(Literal::Number(1))),
                    Box::new(ExprKind::Literal(Literal::Number(2)))
                ))
            )
        );
    }
}
