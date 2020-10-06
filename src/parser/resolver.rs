use crate::common::ast;
use crate::common::constants::{INIT_STR, SUPER_STR, THIS_STR};

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

    pub fn resolve_root(&mut self, stmt: &mut ast::Stmt) -> ResolveResult<()> {
        self.scopes = vec![];
        self.resolve_statement(stmt)
    }

    fn resolve_statement(&mut self, stmt: &mut ast::Stmt) -> ResolveResult<()> {
        match &mut stmt.kind {
            ast::StmtKind::Expression(expr) => self.resolve_expression(expr)?,
            ast::StmtKind::Print(expr) => self.resolve_expression(expr)?,
            ast::StmtKind::VariableDecl(name, expr) => {
                // Declare, resolve, define
                if self.is_already_defined(&name) {
                    return Err(Error::RedefineLocalVar(name.to_owned()));
                }

                self.declare(name);
                self.resolve_expression(expr)?;
                self.define(name);
            }
            ast::StmtKind::Block(stmts) => {
                self.push_scope();
                for stmt in stmts.iter_mut() {
                    self.resolve_statement(stmt)?;
                }
                self.pop_scope();
            }
            ast::StmtKind::IfElse(cond, body, else_body) => {
                self.resolve_expression(cond)?;
                self.resolve_statement(body)?;
                if let Some(else_body) = else_body {
                    self.resolve_statement(else_body)?
                }
            }
            ast::StmtKind::While(cond, body) => {
                self.resolve_expression(cond)?;
                self.resolve_statement(body)?;
            }
            ast::StmtKind::FunctionDecl(fn_data) => {
                self.resolve_function(fn_data, FunctionContext::Function)?
            }
            ast::StmtKind::Return(expr) => {
                if self.function_ctx == FunctionContext::Global {
                    return Err(Error::ReturnAtTopLevel);
                }

                if self.function_ctx == FunctionContext::Initializer && expr.is_some() {
                    return Err(Error::ReturnInInitializer);
                }

                if let Some(expr) = expr {
                    self.resolve_expression(expr)?;
                }
            }
            ast::StmtKind::ClassDecl(name, superclass, methods) => {
                // Define the class in the current scope
                self.define(&name);

                // Resolve the superclass if it exists
                if let Some(superclass) = superclass {
                    if superclass.name == **name {
                        return Err(Error::InheritFromSelf);
                    }
                    self.resolve_local_variable(superclass);
                }

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

                for method_data in methods.iter_mut() {
                    // TODO move initializer detection to the parser?
                    let ctx = if method_data.name == INIT_STR {
                        FunctionContext::Initializer
                    } else {
                        FunctionContext::Method
                    };
                    self.resolve_function(method_data, ctx)?;
                }

                // Restore everything
                self.class_ctx = prev_ctx;
                self.pop_scope();
                if superclass.is_some() {
                    self.pop_scope();
                }
            }
        }
        Ok(())
    }

    fn resolve_expression(&mut self, expr: &mut ast::Expr) -> ResolveResult<()> {
        match &mut expr.kind {
            ast::ExprKind::Literal(_) => (),
            ast::ExprKind::Infix(_, lhs, rhs) => {
                self.resolve_expression(lhs)?;
                self.resolve_expression(rhs)?;
            }
            ast::ExprKind::Prefix(_, subexpr) => self.resolve_expression(subexpr)?,
            ast::ExprKind::Logical(_, lhs, rhs) => {
                self.resolve_expression(lhs)?;
                self.resolve_expression(rhs)?;
            }
            ast::ExprKind::Variable(var) => {
                // Check if we're in the middle of initializing ourselves
                if self.is_during_initializer(&var.name) {
                    return Err(Error::UsedInOwnInitializer(var.name.to_owned()));
                }
                self.resolve_local_variable(var);
            }
            ast::ExprKind::Assignment(var, subexpr) => {
                self.resolve_expression(subexpr)?;
                self.resolve_local_variable(var);
            }
            ast::ExprKind::Call(callee, arg_exprs) => {
                self.resolve_expression(callee)?;
                for a in arg_exprs.iter_mut() {
                    self.resolve_expression(a)?;
                }
            }
            ast::ExprKind::Get(subexpr, _) => {
                self.resolve_expression(subexpr)?;
            }
            ast::ExprKind::Set(subexpr, _, value) => {
                self.resolve_expression(subexpr)?;
                self.resolve_expression(value)?;
            }
            ast::ExprKind::This(var) => {
                if self.class_ctx == ClassContext::Global {
                    return Err(Error::ThisOutsideClass);
                }
                self.resolve_local_variable(var);
            }
            ast::ExprKind::Super(var, _) => {
                if self.class_ctx != ClassContext::Subclass {
                    return Err(Error::SuperOutsideSubclass);
                }
                self.resolve_local_variable(var);
            }
        }
        Ok(())
    }

    fn resolve_function(
        &mut self,
        fn_data: &mut ast::FunctionDecl,
        ctx: FunctionContext,
    ) -> ResolveResult<()> {
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

        self.resolve_statement(&mut fn_data.body)?;

        // Reverse the previous steps
        self.function_ctx = prev_ctx;
        self.pop_scope();

        Ok(())
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
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_owned(), value);
        }
    }
}
