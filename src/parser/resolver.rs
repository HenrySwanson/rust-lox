use crate::common::ast;
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
}

pub struct Resolver {
    scopes: Vec<Scope>,
    function_ctx: FunctionContext,
}

#[derive(Debug)]
pub enum Error {
    UsedInOwnInitializer(String),
    RedefineLocalVar(String),
    ReturnAtTopLevel,
}

pub type ResolveResult<T> = Result<T, Error>;

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            scopes: vec![],
            function_ctx: FunctionContext::Global,
        }
    }

    pub fn resolve_root(&mut self, stmt: &mut ast::Stmt) -> ResolveResult<()> {
        self.scopes = vec![];
        self.resolve_statement(stmt)
    }

    fn resolve_statement(&mut self, stmt: &mut ast::Stmt) -> ResolveResult<()> {
        match stmt {
            ast::Stmt::Expression(expr) => self.resolve_expression(expr)?,
            ast::Stmt::Print(expr) => self.resolve_expression(expr)?,
            ast::Stmt::VariableDecl(name, expr) => {
                // Declare, resolve, define
                if self.is_already_defined(&name) {
                    return Err(Error::RedefineLocalVar(name.to_owned()));
                }

                self.declare(name);
                self.resolve_expression(expr)?;
                self.define(name);
            }
            ast::Stmt::Block(stmts) => {
                self.push_scope();
                for stmt in stmts.iter_mut() {
                    self.resolve_statement(stmt)?;
                }
                self.pop_scope();
            }
            ast::Stmt::IfElse(cond, body, else_body) => {
                self.resolve_expression(cond)?;
                self.resolve_statement(body)?;
                // TODO is there a better way to do this?
                if let Some(ref mut else_body) = **else_body {
                    self.resolve_statement(else_body)?
                }
            }
            ast::Stmt::While(cond, body) => {
                self.resolve_expression(cond)?;
                self.resolve_statement(body)?;
            }
            ast::Stmt::FunctionDecl(name, params, body) => {
                // Define eagerly, so that the function can refer to itself recursively.
                self.define(name);

                // Push a new scope, save the previous function context, and apply
                // the new one.
                self.push_scope();
                let prev_ctx = self.function_ctx;
                self.function_ctx = FunctionContext::Function;

                // Define parameters
                for name in params.iter() {
                    self.define(name)
                }

                self.resolve_statement(body)?;

                // Reverse the previous steps
                self.function_ctx = prev_ctx;
                self.pop_scope();
            }
            ast::Stmt::Return(expr) => {
                if self.function_ctx == FunctionContext::Global {
                    return Err(Error::ReturnAtTopLevel);
                }
                self.resolve_expression(expr)?
            }
        }
        Ok(())
    }

    fn resolve_expression(&mut self, expr: &mut ast::Expr) -> ResolveResult<()> {
        match expr {
            ast::Expr::NumberLiteral(_)
            | ast::Expr::BooleanLiteral(_)
            | ast::Expr::StringLiteral(_)
            | ast::Expr::NilLiteral => (),
            ast::Expr::Infix(_, lhs, rhs) => {
                self.resolve_expression(lhs)?;
                self.resolve_expression(rhs)?;
            }
            ast::Expr::Prefix(_, subexpr) => self.resolve_expression(subexpr)?,
            ast::Expr::Logical(_, lhs, rhs) => {
                self.resolve_expression(lhs)?;
                self.resolve_expression(rhs)?;
            }
            ast::Expr::Variable(var) => {
                // Check if we're in the middle of initializing ourselves
                if self.is_during_initializer(&var.name) {
                    return Err(Error::UsedInOwnInitializer(var.name.to_owned()));
                }
                self.resolve_local_variable(var);
            }
            ast::Expr::Assignment(var, subexpr) => {
                self.resolve_expression(subexpr)?;
                self.resolve_local_variable(var);
            }
            ast::Expr::Call(callee, arg_exprs) => {
                self.resolve_expression(callee)?;
                for a in arg_exprs.iter_mut() {
                    self.resolve_expression(a)?;
                }
            }
        }
        Ok(())
    }

    // Returns true if we're in the middle of initializing variable `name`.
    fn is_during_initializer(&self, name: &str) -> bool {
        if let Some(scope) = self.scopes.last() {
            match scope.get(name) {
                Some(VariableState::Uninitialized) => {
                    return true;
                }
                _ => (),
            }
        }
        return false;
    }

    // Returns true if we've already defined this variable in the current scope
    fn is_already_defined(&self, name: &str) -> bool {
        if let Some(scope) = self.scopes.last() {
            return scope.contains_key(name);
        }
        return false;
    }

    fn resolve_local_variable(&self, var: &mut ast::VariableRef) {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(&var.name) {
                var.hops = Some(i);
                return;
            }
        }
        // TODO do i want to use None for globals?
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
        match self.scopes.last_mut() {
            Some(scope) => {
                scope.insert(name.to_owned(), value);
            }
            None => (),
        }
    }
}
