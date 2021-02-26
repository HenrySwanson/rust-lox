use super::ast;
use super::constants::{INIT_STR, SUPER_STR, THIS_STR};

use crate::frontend::ast as frontend_ast;
use crate::frontend::span::Span;

use std::collections::HashMap;

type Scope = HashMap<String, VariableState>;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum VariableState {
    Initialized,
    Uninitialized,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum FunctionContext {
    Global,
    Function,
    Method,
    Initializer,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ClassContext {
    Global,
    Class,
    Subclass,
}

pub struct Resolver {
    scopes: Vec<Scope>,
    function_ctx: FunctionContext,
    class_ctx: ClassContext,
}

#[derive(Debug)]
pub enum Error {
    UsedInOwnInitializer(String),
    RedefineLocalVar(String),
    ReturnAtTopLevel,
    ThisOutsideClass,
    ReturnInInitializer,
    InheritFromSelf,
    SuperOutsideSubclass,
}

pub type ResolveResult<T> = Result<T, Error>;

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            scopes: vec![],
            function_ctx: FunctionContext::Global,
            class_ctx: ClassContext::Global,
        }
    }

    pub fn resolve(mut self, tree: frontend_ast::Tree) -> ResolveResult<ast::Tree> {
        let mut statements = vec![];
        for stmt in tree.statements.iter() {
            // TODO like parser, collect errors and report many
            statements.push(self.resolve_statement(stmt)?);
        }

        Ok(ast::Tree { statements })
    }

    fn resolve_statement(&mut self, stmt: &frontend_ast::Stmt) -> ResolveResult<ast::Stmt> {
        let kind = match &stmt.kind {
            frontend_ast::StmtKind::Expression(expr) => {
                ast::StmtKind::Expression(self.resolve_expression(expr)?)
            }
            frontend_ast::StmtKind::Print(expr) => {
                ast::StmtKind::Print(self.resolve_expression(expr)?)
            }
            frontend_ast::StmtKind::VariableDecl(name, expr) => {
                // Declare, resolve, define
                if self.is_already_defined(&name) {
                    return Err(Error::RedefineLocalVar(name.to_owned()));
                }

                self.declare(name);
                let expr = self.resolve_expression(expr)?;
                self.define(name);
                ast::StmtKind::VariableDecl(name.clone(), expr)
            }
            frontend_ast::StmtKind::Block(stmts) => {
                self.push_scope();

                let mut resolved_stmts = Vec::with_capacity(stmts.len());
                for stmt in stmts.iter() {
                    resolved_stmts.push(self.resolve_statement(stmt)?);
                }

                self.pop_scope();
                ast::StmtKind::Block(resolved_stmts)
            }
            frontend_ast::StmtKind::IfElse(cond, body, else_body) => {
                let cond = self.resolve_expression(cond)?;
                let body = Box::new(self.resolve_statement(body.as_ref())?);
                let else_body = match else_body {
                    Some(s) => Some(Box::new(self.resolve_statement(s)?)),
                    None => None,
                };
                ast::StmtKind::IfElse(cond, body, else_body)
            }
            frontend_ast::StmtKind::While(cond, body) => {
                let cond = self.resolve_expression(cond)?;
                let body = Box::new(self.resolve_statement(body)?);
                ast::StmtKind::While(cond, body)
            }
            frontend_ast::StmtKind::FunctionDecl(fn_data) => {
                let fn_data = self.resolve_function(fn_data, FunctionContext::Function)?;
                ast::StmtKind::FunctionDecl(fn_data)
            }
            frontend_ast::StmtKind::Return(expr) => {
                let return_expr = match self.function_ctx {
                    // Returns not allowed outside functions
                    FunctionContext::Global => return Err(Error::ReturnAtTopLevel),
                    // Returns in initializers are a little different. They _need_ to be
                    // empty returns, and unlike normal empty returns, they return `this`
                    // instead of `nil`.
                    FunctionContext::Initializer => match expr {
                        Some(_) => return Err(Error::ReturnInInitializer),
                        None => ast::Expr {
                            kind: ast::ExprKind::This(ast::VariableRef {
                                name: THIS_STR.to_owned(),
                                hops: self.lookup_variable(THIS_STR),
                            }),
                            span: Span::dummy(),
                        },
                    },
                    // Functions and methods are easy, just replace empty returns with `return nil`.
                    _ => match expr {
                        Some(expr) => self.resolve_expression(expr)?,
                        None => ast::Expr {
                            kind: ast::ExprKind::Literal(ast::Literal::Nil),
                            span: Span::dummy(),
                        },
                    },
                };

                ast::StmtKind::Return(return_expr)
            }
            frontend_ast::StmtKind::ClassDecl(name, superclass, methods) => {
                // Define the class in the current scope
                self.define(&name);

                // Resolve the superclass if it exists
                let resolved_superclass = match superclass {
                    Some(superclass) => {
                        if superclass == name {
                            return Err(Error::InheritFromSelf);
                        }
                        Some(self.resolve_variable(&superclass))
                    }
                    None => None,
                };

                // If applicable, begin a new scope, defining super
                if superclass.is_some() {
                    self.push_scope();
                    self.define(SUPER_STR);
                }

                // In all cases, begin our own scope, defining this
                self.push_scope();
                self.define(THIS_STR);

                // Stash our old class context
                let prev_ctx = self.class_ctx;
                self.class_ctx = if superclass.is_some() {
                    ClassContext::Subclass
                } else {
                    ClassContext::Class
                };

                let mut resolved_methods = Vec::with_capacity(methods.len());
                for method_data in methods.iter() {
                    let function_ctx = if method_data.name == INIT_STR {
                        FunctionContext::Initializer
                    } else {
                        FunctionContext::Method
                    };
                    resolved_methods.push(self.resolve_function(method_data, function_ctx)?);
                }

                // Restore everything
                self.class_ctx = prev_ctx;
                self.pop_scope();
                if superclass.is_some() {
                    self.pop_scope();
                }

                ast::StmtKind::ClassDecl(name.clone(), resolved_superclass, resolved_methods)
            }
        };

        Ok(ast::Stmt {
            kind,
            span: stmt.span.clone(),
        })
    }

    fn resolve_expression(&mut self, expr: &frontend_ast::Expr) -> ResolveResult<ast::Expr> {
        let kind = match &expr.kind {
            frontend_ast::ExprKind::Literal(lit) => ast::ExprKind::Literal(lit.clone()),
            frontend_ast::ExprKind::BinOp(op, lhs, rhs) => {
                let lhs = Box::new(self.resolve_expression(lhs)?);
                let rhs = Box::new(self.resolve_expression(rhs)?);
                ast::ExprKind::BinOp(*op, lhs, rhs)
            }
            frontend_ast::ExprKind::UnaryOp(op, subexpr) => {
                let subexpr = Box::new(self.resolve_expression(subexpr)?);
                ast::ExprKind::UnaryOp(*op, subexpr)
            }
            frontend_ast::ExprKind::Logical(op, lhs, rhs) => {
                let lhs = Box::new(self.resolve_expression(lhs)?);
                let rhs = Box::new(self.resolve_expression(rhs)?);
                ast::ExprKind::Logical(*op, lhs, rhs)
            }
            frontend_ast::ExprKind::Variable(var) => {
                // Check if we're in the middle of initializing ourselves
                if self.is_during_initializer(&var) {
                    return Err(Error::UsedInOwnInitializer(var.to_owned()));
                }
                ast::ExprKind::Variable(self.resolve_variable(&var))
            }
            frontend_ast::ExprKind::Assignment(var, subexpr) => {
                let var = self.resolve_variable(&var);
                let subexpr = Box::new(self.resolve_expression(subexpr)?);
                ast::ExprKind::Assignment(var, subexpr)
            }
            frontend_ast::ExprKind::Call(callee, arg_exprs) => {
                let callee = Box::new(self.resolve_expression(callee)?);
                let arg_exprs: Result<Vec<_>, _> = arg_exprs
                    .iter()
                    .map(|e| self.resolve_expression(e))
                    .collect();
                ast::ExprKind::Call(callee, arg_exprs?)
            }
            frontend_ast::ExprKind::Get(subexpr, property) => {
                let subexpr = Box::new(self.resolve_expression(subexpr)?);
                ast::ExprKind::Get(subexpr, property.clone())
            }
            frontend_ast::ExprKind::Set(subexpr, property, value) => {
                let subexpr = Box::new(self.resolve_expression(subexpr)?);
                let value = Box::new(self.resolve_expression(value)?);
                ast::ExprKind::Set(subexpr, property.clone(), value)
            }
            frontend_ast::ExprKind::This => {
                if self.class_ctx == ClassContext::Global {
                    return Err(Error::ThisOutsideClass);
                }
                ast::ExprKind::This(self.resolve_variable(THIS_STR))
            }
            frontend_ast::ExprKind::Super(method_name) => {
                if self.class_ctx != ClassContext::Subclass {
                    return Err(Error::SuperOutsideSubclass);
                }
                ast::ExprKind::Super(self.resolve_variable(SUPER_STR), method_name.clone())
            }
        };

        Ok(ast::Expr {
            kind,
            span: expr.span.clone(),
        })
    }
    // TODO we could just implement this as a method on the ast components
    fn resolve_function(
        &mut self,
        fn_data: &frontend_ast::FunctionDecl,
        ctx: FunctionContext,
    ) -> ResolveResult<ast::FunctionDecl> {
        // Define eagerly, so that the function can refer to itself recursively.
        self.define(&fn_data.name);

        // Push a new scope, save the previous function context, and apply
        // the new one.
        self.push_scope();
        let prev_ctx = self.function_ctx;
        self.function_ctx = ctx;

        // Define parameters
        for name in fn_data.params.iter() {
            self.define(name);
        }

        let resolved_body = Box::new(self.resolve_statement(&fn_data.body)?);

        // Reverse the previous steps
        self.function_ctx = prev_ctx;
        self.pop_scope();

        Ok(ast::FunctionDecl {
            name: fn_data.name.clone(),
            params: fn_data.params.clone(),
            body: resolved_body,
        })
    }

    fn resolve_variable(&self, name: &str) -> ast::VariableRef {
        ast::VariableRef {
            name: name.to_owned(),
            hops: self.lookup_variable(name),
        }
    }

    // Returns true if we're in the middle of initializing variable `name`.
    fn is_during_initializer(&self, name: &str) -> bool {
        let state = self.scopes.last().map(|s| s.get(name)).flatten();
        state.copied() == Some(VariableState::Uninitialized)
    }

    // Returns true if we've already defined this variable in the current scope
    fn is_already_defined(&self, name: &str) -> bool {
        match self.scopes.last() {
            Some(scope) => scope.contains_key(name),
            None => false,
        }
    }

    fn lookup_variable(&self, name: &str) -> ast::VHops {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name) {
                return ast::VHops::Local(i);
            }
        }
        ast::VHops::Global
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &str) {
        self.set_variable_state(name, VariableState::Uninitialized)
    }

    fn define(&mut self, name: &str) {
        self.set_variable_state(name, VariableState::Initialized)
    }

    fn set_variable_state(&mut self, name: &str, value: VariableState) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_owned(), value);
        }
    }
}
