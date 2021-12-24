use super::ast;
use super::builtins::get_builtins;
use super::class::LoxClassPtr;
use super::constants::{INIT_STR, SUPER_STR, THIS_STR};
use super::environment::Environment;
use super::errs::{Error, RuntimeResult};
use super::function::LoxFunctionPtr;
use super::object::Object;

use std::collections::HashMap;

pub struct Interpreter<W> {
    pub env: Environment,
    pub globals: Environment,
    pub output_sink: W,
}

impl Interpreter<std::io::Stdout> {
    pub fn new() -> Self {
        Interpreter::new_with_output(std::io::stdout())
    }
}

impl<W: std::io::Write> Interpreter<W> {
    pub fn new_with_output(output_sink: W) -> Self {
        let env = Environment::new();
        for builtin in get_builtins().into_iter() {
            let name = builtin.name().to_owned();
            let obj = Object::BuiltInFunction(builtin);
            env.define(name, obj);
        }
        let globals = env.clone();

        Interpreter {
            env,
            globals,
            output_sink,
        }
    }

    pub fn swap_environment(&mut self, mut env: Environment) -> Environment {
        std::mem::swap(&mut self.env, &mut env);
        env // which is now the old self.env
    }

    pub fn eval_statements(&mut self, stmts: &[ast::Stmt]) -> RuntimeResult<()> {
        for stmt in stmts.iter() {
            self.eval_statement(stmt)?;
        }
        Ok(())
    }

    pub fn eval_statement(&mut self, stmt: &ast::Stmt) -> RuntimeResult<()> {
        match &stmt.kind {
            ast::StmtKind::Expression(expr) => {
                self.eval_expression(expr)?;
            }
            ast::StmtKind::Print(expr) => {
                let value = self.eval_expression(expr)?;
                writeln!(&mut self.output_sink, "{:?}", value).expect("Unable to write to output");
            }
            ast::StmtKind::IfElse(cond, body, else_body) => {
                self.eval_if_else(cond, body, else_body.as_deref())?
            }
            ast::StmtKind::While(cond, body) => self.eval_while(cond, body)?,
            ast::StmtKind::VariableDecl(name, expr) => {
                let value = self.eval_expression(expr)?;
                self.env.define(name.clone(), value);
            }
            ast::StmtKind::Block(stmts) => self.eval_block(stmts)?,
            ast::StmtKind::FunctionDecl(fn_data) => {
                self.env.define(
                    fn_data.name.clone(),
                    Object::LoxFunction(self.make_fn_ptr(fn_data, false)),
                );
            }
            ast::StmtKind::Return(expr) => {
                let value = self.eval_expression(expr)?;
                return Err(Error::Return(value));
            }
            ast::StmtKind::ClassDecl(name, superclass_name, method_defs) => {
                // Look up the super class
                let superclass_cls = match superclass_name {
                    None => None,
                    Some(class) => match self.lookup_local_var(class)? {
                        Object::LoxClass(class) => Some(class),
                        obj => return Err(Error::NotAClass(obj)),
                    },
                };

                // If there's a superclass, stash it in a new environment
                let original_env = self.env.clone();
                if let Some(superclass_cls) = &superclass_cls {
                    let superclass_obj = Object::LoxClass(superclass_cls.clone());

                    self.env = Environment::with_enclosing(&self.env);
                    self.env.define(SUPER_STR.to_owned(), superclass_obj);
                }

                // Define the class and its methods
                let mut methods = HashMap::new();
                for m in method_defs.iter() {
                    methods.insert(m.name.clone(), self.make_fn_ptr(m, true));
                }

                // Pop the possible env, and define the class in the original env
                self.env = original_env;

                let class = LoxClassPtr::new(name.clone(), superclass_cls, methods);
                self.env.define(name.clone(), Object::LoxClass(class));
            }
        }

        Ok(())
    }

    fn eval_if_else(
        &mut self,
        condition: &ast::Expr,
        body: &ast::Stmt,
        else_body: Option<&ast::Stmt>,
    ) -> RuntimeResult<()> {
        if self.eval_expression(condition)?.is_truthy() {
            return self.eval_statement(body);
        }

        if let Some(else_body) = else_body {
            return self.eval_statement(else_body);
        }

        Ok(())
    }

    fn eval_while(&mut self, condition: &ast::Expr, body: &ast::Stmt) -> RuntimeResult<()> {
        while self.eval_expression(condition)?.is_truthy() {
            self.eval_statement(body)?;
        }

        Ok(())
    }

    fn eval_block(&mut self, stmts: &[ast::Stmt]) -> RuntimeResult<()> {
        // Grab old scope and make new one
        let prev = self.env.clone();
        self.env = Environment::with_enclosing(&prev);

        for stmt in stmts.iter() {
            match self.eval_statement(stmt) {
                Ok(_) => {}
                Err(e) => {
                    self.env = prev;
                    return Err(e);
                }
            }
        }

        self.env = prev;
        Ok(())
    }

    fn eval_expression(&mut self, expr: &ast::Expr) -> RuntimeResult<Object> {
        match &expr.kind {
            ast::ExprKind::Literal(lit) => Ok(self.eval_literal(lit)),
            ast::ExprKind::BinOp(op, lhs, rhs) => self.eval_infix_operator(*op, lhs, rhs),
            ast::ExprKind::UnaryOp(op, expr) => self.eval_prefix_operator(*op, expr),
            ast::ExprKind::Logical(op, lhs, rhs) => self.eval_logical_operator(*op, lhs, rhs),
            ast::ExprKind::Variable(var) => self.lookup_local_var(var),
            ast::ExprKind::Assignment(var, expr) => {
                let value = self.eval_expression(expr)?;
                match var.hops {
                    ast::VHops::Local(hops) => self.env.set_at(hops, &var.name, value.clone())?,
                    ast::VHops::Global => self.globals.set(&var.name, value.clone())?,
                }
                Ok(value)
            }
            ast::ExprKind::Call(callee, args) => self.eval_function_call(callee, args),
            ast::ExprKind::Get(subexpr, property) => self.eval_property_access(subexpr, property),
            ast::ExprKind::Set(subexpr, property, value) => {
                self.eval_property_mutation(subexpr, property, value)
            }
            ast::ExprKind::This(var) => self.lookup_local_var(var),
            ast::ExprKind::Super(var, method_name) => {
                // This one's frisky. We need to look up the method in the superclass,
                // and bind it to the instance object.
                let superclass = match self.lookup_local_var(var)? {
                    Object::LoxClass(c) => c,
                    _ => panic!("super is not a class"),
                };

                let err_msg = "relationship between super and this broken";
                let this_depth = match var.hops {
                    ast::VHops::Global => panic!("{}", err_msg),
                    ast::VHops::Local(hops) => hops - 1,
                };
                let this = self.env.get_at(this_depth, THIS_STR).expect(err_msg);
                match superclass.find_method(method_name) {
                    Some(method) => Ok(Object::LoxFunction(method.bind(this))),
                    None => Err(Error::NoSuchProperty(this, method_name.to_owned())),
                }
            }
        }
    }

    fn eval_literal(&self, lit: &ast::Literal) -> Object {
        match lit {
            ast::Literal::Number(n) => Object::Number(*n as i64),
            ast::Literal::Boolean(b) => Object::Boolean(*b),
            ast::Literal::Str(s) => Object::String(s.clone()),
            ast::Literal::Nil => Object::Nil,
        }
    }

    fn eval_infix_operator(
        &mut self,
        op: ast::BinaryOperator,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
    ) -> RuntimeResult<Object> {
        let lhs = self.eval_expression(lhs)?;
        let rhs = self.eval_expression(rhs)?;
        Object::apply_infix_op(op, lhs, rhs)
    }

    fn eval_prefix_operator(
        &mut self,
        op: ast::UnaryOperator,
        expr: &ast::Expr,
    ) -> RuntimeResult<Object> {
        let value = self.eval_expression(expr)?;
        Object::apply_prefix_op(op, value)
    }

    fn eval_logical_operator(
        &mut self,
        op: ast::LogicalOperator,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
    ) -> RuntimeResult<Object> {
        // The short-circuiting means we can't yet evaluate the RHS.
        let lhs = self.eval_expression(lhs)?;
        match op {
            ast::LogicalOperator::And if !lhs.is_truthy() => Ok(lhs),
            ast::LogicalOperator::Or if lhs.is_truthy() => Ok(lhs),
            _ => self.eval_expression(rhs),
        }
    }

    fn eval_function_call(
        &mut self,
        callee: &ast::Expr,
        arg_exprs: &[ast::Expr],
    ) -> RuntimeResult<Object> {
        let callee = self.eval_expression(callee)?;

        let mut args = Vec::with_capacity(arg_exprs.len());
        for expr in arg_exprs.iter() {
            args.push(self.eval_expression(expr)?);
        }

        callee.execute_call(args, self)
    }

    fn eval_property_access(&mut self, expr: &ast::Expr, property: &str) -> RuntimeResult<Object> {
        match self.eval_expression(expr)? {
            Object::LoxInstance(instance) => Ok(instance.get(property)?),
            obj => Err(Error::NotAnInstance(obj)),
        }
    }

    fn eval_property_mutation(
        &mut self,
        expr: &ast::Expr,
        property: &str,
        value: &ast::Expr,
    ) -> RuntimeResult<Object> {
        let instance = match self.eval_expression(expr)? {
            Object::LoxInstance(instance) => instance,
            obj => return Err(Error::NotAnInstance(obj)),
        };
        let value = self.eval_expression(value)?;
        instance.set(property, value.clone());

        Ok(value)
    }

    // ---- various helpers ----

    fn make_fn_ptr(&self, fn_data: &ast::FunctionDecl, is_method: bool) -> LoxFunctionPtr {
        let is_initializer = is_method && fn_data.name == INIT_STR;
        LoxFunctionPtr::new(fn_data.clone(), is_initializer, self.env.clone())
    }

    fn lookup_local_var(&self, var: &ast::VariableRef) -> RuntimeResult<Object> {
        match var.hops {
            ast::VHops::Local(hops) => self.env.get_at(hops, &var.name),
            ast::VHops::Global => self.globals.get(&var.name),
        }
    }
}
