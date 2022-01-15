use super::super::errs::{Error, ErrorKind, Item, ParseResult, MAX_NUMBER_ARGS};
use super::super::lexer::Lexer;
use super::super::lexer::{SpannedToken, Token};
use super::super::span::Span;
use super::ast;
use super::precedence::{InfixOperator, Precedence};

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

        Parser {
            source,
            lexer: Lexer::new(source),
            current: fake_token.clone(),
            previous: fake_token,
            errors: vec![],
        }
    }

    // ---- Simple token-based operations ----

    /// Advances the stream by one token
    fn bump(&mut self) -> ParseResult<()> {
        // No-clone trick: swap current into previous, replace current
        std::mem::swap(&mut self.previous, &mut self.current);
        self.current = self.lexer.next_token();

        if let Token::Error(e) = &self.current.token {
            Err(Error {
                span: self.current.span,
                kind: ErrorKind::InvalidToken(e.clone()),
            })
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
    fn eat(&mut self, expected: Token, error: ErrorKind) -> ParseResult<()> {
        self.bump()?;

        if self.previous.token == expected {
            Ok(())
        } else {
            Err(Error {
                span: self.previous.span,
                kind: error,
            })
        }
    }

    fn expect(&mut self, expected: Token) {
        assert_eq!(self.current.token, expected);
        std::mem::drop(self.bump());
    }

    // ---- parsing methods ----

    pub fn parse_all(mut self) -> Result<ast::Tree, Vec<Error>> {
        // TODO: the changes in synchronize affect this code. re-think it

        // "Prime the pump", so that self.current points at a real token.
        // Unfortunately self.previous will still point at the fake token
        // generated in the constructor, but that's fine, I think.
        // It depends on if we do lookbehind at the beginning
        // of a parse. Which we don't, but it's not enforced.)
        if let Err(e) = self.bump() {
            self.emit_error(e);
            self.synchronize();
        };

        let mut stmts = vec![];

        while !self.check(Token::EndOfFile) {
            if let Some(stmt) = self.parse_declaration_with_recovery() {
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

    fn parse_declaration_with_recovery(&mut self) -> Option<ast::Stmt> {
        match self.parse_declaration() {
            Ok(stmt) => Some(stmt),
            Err(err) => {
                self.emit_error(err);
                self.synchronize();
                None
            }
        }
    }

    fn parse_declaration(&mut self) -> ParseResult<ast::Stmt> {
        // TODO: include Error as a possible node, instead of using None
        let lo = self.current.span;
        let stmt_kind = match self.current.token {
            Token::Var => self.parse_variable_decl()?,
            Token::Fun => {
                // parse_function_data doesn't expect the `fun`, b/c of methods
                self.bump()?;
                let fn_data = self.parse_function_data()?;
                ast::StmtKind::FunctionDecl(fn_data)
            }
            Token::Class => self.parse_class_decl()?,
            _ => return self.parse_statement(),
        };

        Ok(ast::Stmt {
            kind: stmt_kind,
            span: lo.to(self.previous.span),
        })
    }

    fn parse_variable_decl(&mut self) -> ParseResult<ast::StmtKind> {
        self.expect(Token::Var);
        let name = self.parse_identifier(ErrorKind::ExpectedIdentifier)?;
        let expr = if self.try_eat(Token::Equals) {
            self.parse_expression()?
        } else {
            mk_expr(from_lit(ast::Literal::Nil), Span::dummy())
        };
        self.eat(
            Token::Semicolon,
            ErrorKind::ExpectAfter(";", Item::VariableDecl),
        )?;

        Ok(ast::StmtKind::VariableDecl(name, expr))
    }

    fn parse_function_data(&mut self) -> ParseResult<ast::FunctionDecl> {
        let name = self.parse_identifier(ErrorKind::ExpectedIdentifier)?;
        let params = self.parse_fn_params()?;

        self.eat(
            Token::LeftBrace,
            ErrorKind::ExpectBefore("{", Item::FunctionBody),
        )?;
        let stmts = self.parse_braced_statement_tail()?;

        let fn_data = ast::FunctionDecl::new(name, params, stmts);
        Ok(fn_data)
    }

    fn parse_class_decl(&mut self) -> ParseResult<ast::StmtKind> {
        self.expect(Token::Class);
        let name = self.parse_identifier(ErrorKind::ExpectedIdentifier)?;

        let superclass = if self.try_eat(Token::LeftAngle) {
            let superclass_name = self.parse_identifier(ErrorKind::ExpectSuperclassName)?;
            Some(superclass_name)
        } else {
            None
        };

        self.eat(
            Token::LeftBrace,
            ErrorKind::ExpectBefore("{", Item::ClassBody),
        )?;

        let mut methods = vec![];
        while !self.try_eat(Token::RightBrace) {
            let method_data = self.parse_function_data()?;
            methods.push(method_data);
        }

        Ok(ast::StmtKind::ClassDecl(name, superclass, methods))
    }

    fn parse_statement(&mut self) -> ParseResult<ast::Stmt> {
        let lo = self.current.span;
        let stmt_kind = match self.current.token {
            Token::Print => {
                self.bump()?;
                let expr = self.parse_expression()?;
                self.eat(
                    Token::Semicolon,
                    ErrorKind::ExpectAfter(";", Item::PrintValue),
                )?;
                ast::StmtKind::Print(expr)
            }
            Token::If => self.parse_if_else_statement()?,
            Token::While => self.parse_while_statement()?,
            Token::For => self.parse_for_statement()?,
            Token::Return => self.parse_return_statement()?,
            Token::LeftBrace => {
                self.bump()?;
                let stmts = self.parse_braced_statement_tail()?;
                ast::StmtKind::Block(stmts)
            }
            _ => self.parse_expression_statement()?,
        };

        Ok(ast::Stmt {
            kind: stmt_kind,
            span: lo.to(self.previous.span),
        })
    }

    fn parse_expression_statement(&mut self) -> ParseResult<ast::StmtKind> {
        let expr = self.parse_expression()?;
        self.eat(
            Token::Semicolon,
            ErrorKind::ExpectAfter(";", Item::Expression),
        )?;
        Ok(ast::StmtKind::Expression(expr))
    }

    fn parse_if_else_statement(&mut self) -> ParseResult<ast::StmtKind> {
        self.expect(Token::If);
        self.eat(Token::LeftParen, ErrorKind::ExpectAfter("(", Item::If))?;
        let condition = self.parse_expression()?;
        self.eat(
            Token::RightParen,
            ErrorKind::ExpectAfter(")", Item::Condition),
        )?;

        let body = Box::new(self.parse_statement()?);
        let else_body = if self.try_eat(Token::Else) {
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        Ok(ast::StmtKind::IfElse(condition, body, else_body))
    }

    fn parse_while_statement(&mut self) -> ParseResult<ast::StmtKind> {
        self.expect(Token::While);
        self.eat(Token::LeftParen, ErrorKind::ExpectAfter("(", Item::While))?;
        let condition = self.parse_expression()?;
        self.eat(
            Token::RightParen,
            ErrorKind::ExpectAfter(")", Item::Condition),
        )?;

        let body = self.parse_statement()?;

        Ok(ast::StmtKind::While(condition, Box::new(body)))
    }

    fn parse_for_statement(&mut self) -> ParseResult<ast::StmtKind> {
        self.expect(Token::For);

        self.eat(Token::LeftParen, ErrorKind::ExpectAfter("(", Item::For))?;

        // Figure out the three parts of the loop; each is optional
        let initializer = if self.try_eat(Token::Semicolon) {
            None
        } else if self.check(Token::Var) {
            let lo = self.current.span;
            let stmt = mk_stmt(self.parse_variable_decl()?, lo.to(self.previous.span));
            Some(Box::new(stmt))
        } else {
            let lo = self.current.span;
            let stmt = mk_stmt(
                self.parse_expression_statement()?,
                lo.to(self.previous.span),
            );
            Some(Box::new(stmt))
        };

        let condition = if self.check(Token::Semicolon) {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };

        self.eat(
            Token::Semicolon,
            ErrorKind::ExpectAfter(";", Item::Condition),
        )?;

        let increment = if self.check(Token::RightParen) {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };

        self.eat(
            Token::RightParen,
            ErrorKind::ExpectAfter(")", Item::ForClause),
        )?;

        let body = Box::new(self.parse_statement()?);

        Ok(ast::StmtKind::For(initializer, condition, increment, body))
    }

    fn parse_return_statement(&mut self) -> ParseResult<ast::StmtKind> {
        self.expect(Token::Return);
        let expr = if !self.check(Token::Semicolon) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.eat(
            Token::Semicolon,
            ErrorKind::ExpectAfter(";", Item::ReturnValue),
        )?;
        Ok(ast::StmtKind::Return(expr))
    }

    fn parse_braced_statement_tail(&mut self) -> ParseResult<Vec<ast::Stmt>> {
        let mut stmts = vec![];

        while !self.check(Token::RightBrace) && !self.check(Token::EndOfFile) {
            if let Some(stmt) = self.parse_declaration_with_recovery() {
                stmts.push(stmt);
            }
        }

        if self.current.token != Token::EndOfFile {
            // This must be a `}`, because of the loop condition.
            self.expect(Token::RightBrace);
            Ok(stmts)
        } else {
            Err(Error {
                span: self.current.span,
                kind: ErrorKind::UnclosedBrace,
            })
        }
    }

    fn parse_expression(&mut self) -> ParseResult<ast::Expr> {
        // We use pratt parsing to deal with expressions.
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
                            return Err(Error {
                                span: op_span,
                                kind: ErrorKind::InvalidAssignment,
                            });
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
                    let rhs = self.parse_identifier(ErrorKind::ExpectPropertyName)?;
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
            Token::Identifier(name) => {
                ast::ExprKind::Variable(ast::Identifier::new(name.to_owned(), lo))
            }
            Token::This => ast::ExprKind::This,
            Token::Super => {
                self.eat(Token::Dot, ErrorKind::ExpectSuperDot)?;
                let method_name = self.parse_identifier(ErrorKind::ExpectSuperMethod)?;
                ast::ExprKind::Super(method_name)
            }
            // Parentheses
            Token::LeftParen => {
                let expr = self.parse_expression()?;
                self.eat(
                    Token::RightParen,
                    ErrorKind::ExpectAfter(";", Item::Expression),
                )?;
                return Ok(expr);
            }
            t => {
                return Err(Error {
                    span: lo,
                    kind: ErrorKind::ExpectedExprAt(t.clone()),
                })
            }
        };
        Ok(mk_expr(expr_kind, lo.to(self.previous.span)))
    }

    fn parse_identifier(&mut self, error: ErrorKind) -> ParseResult<ast::Identifier> {
        self.bump()?;
        match &self.previous.token {
            Token::Identifier(name) => {
                Ok(ast::Identifier::new(name.to_owned(), self.previous.span))
            }
            _ => Err(Error {
                span: self.previous.span,
                kind: error,
            }),
        }
    }

    fn parse_comma_sep_tail<T, F>(&mut self, elt_parser: F) -> ParseResult<Vec<T>>
    where
        F: Fn(&mut Parser<'src>) -> ParseResult<T>,
    {
        let mut elements = vec![];

        // Check for the zero-argument case
        if self.try_eat(Token::RightParen) {
            return Ok(elements);
        }

        // There must be at least one argument
        elements.push(elt_parser(self)?);

        while !self.try_eat(Token::RightParen) {
            self.eat(Token::Comma, ErrorKind::ExpectCommaBetween)?;

            elements.push(elt_parser(self)?);
        }

        Ok(elements)
    }

    fn parse_fn_args(&mut self) -> ParseResult<Vec<ast::Expr>> {
        self.expect(Token::LeftParen);

        let args = self.parse_comma_sep_tail(Self::parse_expression)?;
        if let Some(extra_args) = args.get(MAX_NUMBER_ARGS..) {
            for arg in extra_args.iter() {
                self.emit_error(Error {
                    span: arg.span,
                    kind: ErrorKind::TooManyArgs,
                });
            }
        }

        // We've already recorded the errors, so it's fine to return this even if it's
        // slightly out of spec.
        Ok(args)
    }

    fn parse_fn_params(&mut self) -> ParseResult<Vec<ast::Identifier>> {
        self.eat(
            Token::LeftParen,
            ErrorKind::ExpectAfter("(", Item::FunctionName),
        )?;

        let params =
            self.parse_comma_sep_tail(|this| this.parse_identifier(ErrorKind::ExpectedIdentifier))?;
        if let Some(extra_params) = params.get(MAX_NUMBER_ARGS..) {
            for ident in extra_params.iter() {
                self.emit_error(Error {
                    span: ident.span,
                    kind: ErrorKind::TooManyParams,
                });
            }
        }

        // We've already recorded the errors, so it's fine to return this even if it's
        // slightly out of spec.
        Ok(params)
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
            parser.bump().unwrap();
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
