use super::ast;
use super::errs::{Error, ParseResult, MAX_NUMBER_ARGS};
use super::lexer::Lexer;
use super::precedence::{InfixOperator, Precedence};
use super::span::Span;
use super::token::{SpannedToken, Token};

pub struct Parser<'a> {
    source: &'a str,
    lexer: Lexer<'a>,

    current: SpannedToken,  // first unconsumed token
    previous: SpannedToken, // last consumed token

    errors: Vec<Error>,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Self {
        let fake_token = SpannedToken {
            token: Token::Error("<internal parser token>".to_owned()),
            span: Span::dummy(),
        };

        let mut parser = Parser {
            source,
            lexer: Lexer::new(source),
            current: fake_token.clone(),
            previous: fake_token,
            errors: vec![],
        };

        // "Prime the pump" and return
        // If we get an error token, that's okay, we'll pick it up with
        // `check` and `try_eat`.
        std::mem::drop(parser.bump());
        parser
    }

    // ---- Simple token-based operations ----

    /// Advances the stream by one token
    fn bump(&mut self) -> ParseResult<()> {
        // No-clone trick: swap current into previous, replace current
        std::mem::swap(&mut self.previous, &mut self.current);
        self.current = self.lexer.next_token();

        if let Token::Error(e) = &self.current.token {
            Err(Error::IllegalToken(self.current.span, e.clone()))
        } else {
            Ok(())
        }
    }

    /// Checks whether or not the current token matches the given token
    fn check(&self, t: Token) -> bool {
        self.current.token == t
    }

    /// Checks whether or not the current token matches the given token,
    /// and if so, consumes it, returning true.
    fn try_eat(&mut self, t: Token) -> bool {
        if self.check(t) {
            self.bump().expect("Why are you matching error tokens???");
            true
        } else {
            false
        }
    }

    /// Same as try_eat, but returns an error if the token doesn't match.
    fn eat(&mut self, expected: Token) -> ParseResult<()> {
        // TODO: take in a closure for producing the error. the closure takes the SpannedToken
        // that didn't match.
        self.bump()?;

        if self.previous.token == expected {
            Ok(())
        } else {
            Err(Error::ExpectedTokenAt(
                expected,
                self.previous.span,
                self.previous.token.clone(),
            ))
        }
    }

    // ---- parsing methods ----

    pub fn parse_all(mut self) -> Result<ast::Tree, Vec<Error>> {
        // TODO: the changes in synchronize affect this code. re-think it

        let mut stmts = vec![];

        while !self.check(Token::EndOfFile) {
            if let Some(stmt) = self.parse_spanned_declaration() {
                stmts.push(stmt);
            }
        }

        if self.errors.is_empty() {
            Ok(ast::Tree { statements: stmts })
        } else {
            Err(self.errors)
        }
    }

    fn synchronize(&mut self) {
        // Consume until we see a semicolon or EOF

        // Special case: if the erroneous token we consumed is a semicolon,
        // we're already synced.
        let mut synchronization_point = self.previous.token == Token::Semicolon;

        while !synchronization_point {
            let current_token = &self.current.token;

            if matches!(current_token, Token::Semicolon | Token::EndOfFile) {
                synchronization_point = true;
            }

            // Always consume the token
            std::mem::drop(self.bump());
        }
    }

    fn emit_error(&mut self, err: Error) {
        self.errors.push(err);
    }

    fn parse_spanned_declaration(&mut self) -> Option<ast::Stmt> {
        let lo = self.current.span;
        let decl = self.parse_declaration_nospan();
        let hi = self.previous.span;

        match decl {
            Ok(stmt) => Some(ast::Stmt {
                kind: stmt,
                span: lo.to(hi),
            }),
            Err(err) => {
                self.emit_error(err);
                self.synchronize();
                None
            }
        }
    }

    fn parse_declaration_nospan(&mut self) -> ParseResult<ast::StmtKind> {
        match self.current.token {
            Token::Var => {
                self.bump()?;
                let name = self.parse_identifier()?;
                let expr = if self.try_eat(Token::Equals) {
                    self.parse_expression()?
                } else {
                    mk_expr(from_lit(ast::Literal::Nil), Span::dummy())
                };
                self.eat(Token::Semicolon)?;

                Ok(ast::StmtKind::VariableDecl(name, expr))
            }
            Token::Fun => {
                let fn_data = self.parse_function_data(true)?;
                Ok(ast::StmtKind::FunctionDecl(fn_data))
            }
            Token::Class => self.parse_class_decl(),
            _ => self.parse_statement_nospan(),
        }
    }

    fn parse_function_data(&mut self, leading_fun: bool) -> ParseResult<ast::FunctionDecl> {
        if leading_fun {
            self.eat(Token::Fun)?;
        }
        let name = self.parse_identifier()?;
        let params = self.parse_fn_params()?;

        self.eat(Token::LeftBrace)
            .map_err(|_| Error::FunctionBodyStart(self.previous.span))?;
        let stmts = self.parse_braced_statement_tail()?;

        let fn_data = ast::FunctionDecl::new(name, params, stmts);
        Ok(fn_data)
    }

    fn parse_class_decl(&mut self) -> ParseResult<ast::StmtKind> {
        self.eat(Token::Class)?;
        let name = self.parse_identifier()?;

        let superclass = if self.try_eat(Token::LeftAngle) {
            Some(self.parse_identifier()?)
        } else {
            None
        };

        self.eat(Token::LeftBrace)?;

        let mut methods = vec![];
        while !self.try_eat(Token::RightBrace) {
            let method_data = self.parse_function_data(false)?;
            methods.push(method_data);
        }

        Ok(ast::StmtKind::ClassDecl(name, superclass, methods))
    }

    fn parse_spanned_statement(&mut self) -> ParseResult<ast::Stmt> {
        let lo = self.current.span;
        let stmt = self.parse_statement_nospan()?;
        let hi = self.previous.span;

        Ok(ast::Stmt {
            kind: stmt,
            span: lo.to(hi),
        })
    }

    fn parse_statement_nospan(&mut self) -> ParseResult<ast::StmtKind> {
        match self.current.token {
            Token::Print => {
                self.bump()?;
                let expr = self.parse_expression()?;
                self.eat(Token::Semicolon)?;
                Ok(ast::StmtKind::Print(expr))
            }
            Token::If => self.parse_if_else_statement(),
            Token::While => self.parse_while_statement(),
            Token::For => self.parse_for_statement(),
            Token::Return => self.parse_return_statement(),
            Token::LeftBrace => {
                self.bump()?;
                let stmts = self.parse_braced_statement_tail()?;
                Ok(ast::StmtKind::Block(stmts))
            }
            _ => {
                let expr = self.parse_expression()?;
                self.eat(Token::Semicolon)
                    .map_err(|_| Error::SemiAfterExpression(self.previous.span))?;
                Ok(ast::StmtKind::Expression(expr))
            }
        }
    }

    fn parse_if_else_statement(&mut self) -> ParseResult<ast::StmtKind> {
        self.eat(Token::If)?;
        self.eat(Token::LeftParen)?;
        let condition = self.parse_expression()?;
        self.eat(Token::RightParen)?;

        let body = Box::new(self.parse_spanned_statement()?);
        let else_body = if self.try_eat(Token::Else) {
            Some(Box::new(self.parse_spanned_statement()?))
        } else {
            None
        };

        Ok(ast::StmtKind::IfElse(condition, body, else_body))
    }

    fn parse_while_statement(&mut self) -> ParseResult<ast::StmtKind> {
        self.eat(Token::While)?;
        self.eat(Token::LeftParen)?;
        let condition = self.parse_expression()?;
        self.eat(Token::RightParen)?;

        let body = self.parse_spanned_statement()?;

        Ok(ast::StmtKind::While(condition, Box::new(body)))
    }

    fn parse_for_statement(&mut self) -> ParseResult<ast::StmtKind> {
        // TODO: this one's very messy, can we make it less ridiculous?
        let lo = self.current.span;

        self.eat(Token::For)?;
        self.eat(Token::LeftParen)?;

        // Figure out the three parts of the loop; each is optional
        let initializer = if self.try_eat(Token::Semicolon) {
            None
        } else if self.check(Token::Var) {
            self.parse_spanned_declaration()
        } else {
            let lo = self.current.span;
            let expr = self.parse_expression()?;
            self.eat(Token::Semicolon)?;
            Some(mk_stmt(
                ast::StmtKind::Expression(expr),
                lo.to(self.previous.span),
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
        let mut body = self.parse_spanned_statement()?;

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
                lo.to(self.previous.span),
            );
        }

        // TODO lol this is dumb
        Ok(body.kind)
    }

    fn parse_return_statement(&mut self) -> ParseResult<ast::StmtKind> {
        self.eat(Token::Return)?;
        let expr = if !self.check(Token::Semicolon) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.eat(Token::Semicolon)?;
        Ok(ast::StmtKind::Return(expr))
    }

    fn parse_braced_statement_tail(&mut self) -> ParseResult<Vec<ast::Stmt>> {
        let mut stmts = vec![];

        while !self.check(Token::RightBrace) && !self.check(Token::EndOfFile) {
            if let Some(stmt) = self.parse_spanned_declaration() {
                stmts.push(stmt);
            }
        }

        if self.current.token != Token::EndOfFile {
            // This must be a `}`, because of the loop condition.
            self.eat(Token::RightBrace).unwrap();
            Ok(stmts)
        } else {
            Err(Error::UnclosedBrace(self.current.span))
        }
    }

    fn parse_expression(&mut self) -> ParseResult<ast::Expr> {
        // We use pratt parsing to deal with expressions. Because of this,
        // we don't have a parse_expression_nospan.
        self.pratt_parse(Precedence::Lowest)
    }

    fn pratt_parse(&mut self, min_precedence: Precedence) -> ParseResult<ast::Expr> {
        // We parse the first operand, which may start with a prefix expression
        let prefix_op = match &self.current.token {
            Token::Bang => Some(ast::UnaryOperator::LogicalNot),
            Token::Minus => Some(ast::UnaryOperator::Negate),
            _ => None,
        };

        let mut lhs = match prefix_op {
            Some(op) => {
                let lo = self.current.span;
                self.bump()?;
                let expr = self.pratt_parse(Precedence::Unary)?;
                mk_expr(
                    ast::ExprKind::UnaryOp(op, Box::new(expr)),
                    lo.to(self.previous.span),
                )
            }
            None => self.parse_primary()?,
        };

        // Now we start consuming infix operators
        while let Some(op) = InfixOperator::try_from_token(&self.current.token) {
            // Check the precedence of this operator -- if it binds more weakly than
            // our current precedence, then our expression is over, and we should
            // break out so that our caller can process it.
            if !op.exceeds(min_precedence) {
                break;
            }

            let op_span = self.current.span;

            // Consume the operator token.
            // (for Call, we call parse_fn_args later, we shouldn't consume the
            // left parenthesis. This is gross though. :\ TODO: fix)
            if op != InfixOperator::Call {
                self.bump()?;
            }

            // Stash some Copy variables for later
            let precedence = op.precedence();
            let lhs_span = lhs.span;

            // Now we have to do different things depending on the operation
            let new_lhs = match op {
                InfixOperator::Arithequal(op) => {
                    let rhs = self.pratt_parse(precedence)?;
                    ast::ExprKind::BinOp(op, Box::new(lhs), Box::new(rhs))
                }
                InfixOperator::Logical(op) => {
                    let rhs = self.pratt_parse(precedence)?;
                    ast::ExprKind::Logical(op, Box::new(lhs), Box::new(rhs))
                }
                InfixOperator::Assignment => {
                    let rhs_box = Box::new(self.pratt_parse(precedence)?);
                    // When we consumed the LHS we thought it was an expression, but
                    // it's actually an lvalue! So we must convert it into a different
                    // AST node.
                    match lhs.kind {
                        ast::ExprKind::Variable(var) => ast::ExprKind::Assignment(var, rhs_box),
                        ast::ExprKind::Get(expr, property) => {
                            ast::ExprKind::Set(expr, property, rhs_box)
                        }
                        _ => {
                            return Err(Error::InvalidAssignment(op_span));
                        }
                    }
                }
                InfixOperator::Call => {
                    // Don't parse the parentheses, it'll get consumed by this function
                    let arguments = self.parse_fn_args()?;
                    ast::ExprKind::Call(Box::new(lhs), arguments)
                }
                InfixOperator::Property => {
                    // The RHS must be an identifier
                    let rhs = self
                        .parse_identifier()
                        .map_err(|_| Error::ExpectPropertyName(self.previous.span))?;
                    ast::ExprKind::Get(Box::new(lhs), rhs)
                }
            };

            // Update our current tree node
            lhs = mk_expr(new_lhs, lhs_span.to(self.previous.span));
        }

        Ok(lhs)
    }

    fn parse_primary(&mut self) -> ParseResult<ast::Expr> {
        // Consume a token
        self.bump()?;

        let lo = self.previous.span;

        let expr_kind = match &self.previous.token {
            // Literals
            Token::Number(n) => from_lit(ast::Literal::Number(*n)),
            Token::True => from_lit(ast::Literal::Boolean(true)),
            Token::False => from_lit(ast::Literal::Boolean(false)),
            Token::String(s) => from_lit(ast::Literal::Str(s.to_owned())),
            Token::Nil => from_lit(ast::Literal::Nil),
            // Other things
            Token::Identifier(name) => ast::ExprKind::Variable(name.to_owned()),
            Token::This => ast::ExprKind::This,
            Token::Super => {
                self.eat(Token::Dot)
                    .map_err(|_| Error::ExpectSuperDot(self.previous.span))?;
                let method_name = self
                    .parse_identifier()
                    .map_err(|_| Error::ExpectSuperMethod(self.previous.span))?;
                ast::ExprKind::Super(method_name)
            }
            // Parentheses
            Token::LeftParen => {
                let expr = self.parse_expression()?;
                self.eat(Token::RightParen)?;
                return Ok(expr);
            }
            t => return Err(Error::ExpectedExprAt(lo, t.clone())),
        };
        Ok(mk_expr(expr_kind, lo.to(self.previous.span)))
    }

    fn parse_identifier(&mut self) -> ParseResult<String> {
        self.bump()?;
        match &self.previous.token {
            Token::Identifier(name) => Ok(name.to_owned()),
            _ => Err(Error::ExpectedIdentifier(self.previous.span)),
        }
    }

    fn parse_comma_sep<T, F>(&mut self, elt_parser: F) -> ParseResult<Vec<(T, Span)>>
    where
        F: Fn(&mut Parser<'src>) -> ParseResult<T>,
    {
        self.eat(Token::LeftParen)?;
        let mut args = vec![];

        // Check for the zero-argument case
        if self.try_eat(Token::RightParen) {
            return Ok(args);
        }

        // There must be at least one argument
        let lo = self.current.span;
        let elt = elt_parser(self)?;
        let hi = self.previous.span;
        args.push((elt, lo.to(hi)));

        while !self.try_eat(Token::RightParen) {
            self.eat(Token::Comma)?;

            let lo = self.current.span;
            let elt = elt_parser(self)?;
            let hi = self.previous.span;
            args.push((elt, lo.to(hi)));
        }

        Ok(args)
    }

    fn parse_fn_args(&mut self) -> ParseResult<Vec<ast::Expr>> {
        let args = self.parse_comma_sep(Self::parse_expression)?;
        if let Some(extra_args) = args.get(MAX_NUMBER_ARGS..) {
            for (_, span) in extra_args.iter() {
                self.emit_error(Error::TooManyArgs(*span));
            }
        }

        // We've already recorded the errors, so it's fine to return this even if it's
        // slightly out of spec.
        Ok(args.into_iter().map(|(arg, span)| arg).collect())
    }

    fn parse_fn_params(&mut self) -> ParseResult<Vec<String>> {
        let params = self.parse_comma_sep(Self::parse_identifier)?;
        if let Some(extra_params) = params.get(MAX_NUMBER_ARGS..) {
            for (_, span) in extra_params.iter() {
                self.emit_error(Error::TooManyParams(*span));
            }
        }

        // We've already recorded the errors, so it's fine to return this even if it's
        // slightly out of spec.
        Ok(params.into_iter().map(|(param, span)| param).collect())
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

    #[test]
    fn expression_parsing() {
        fn parse_expression(source: &str) -> ast::Expr {
            let mut parser = Parser::new(source);
            parser.parse_expression().unwrap()
        }

        assert_eq!(parse_expression("3+4").lispy_string(), "(+ 3 4)");

        assert_eq!(
            parse_expression("3 + 1 * 5 - 4").lispy_string(),
            "(- (+ 3 (* 1 5)) 4)"
        );

        assert_eq!(
            parse_expression("-3 * (1 + 2)").lispy_string(),
            "(* (- 3) (+ 1 2))"
        );
    }
}
