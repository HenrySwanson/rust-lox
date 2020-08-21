use super::builtins::get_builtins;
use super::environment::Environment;
use super::errs::{Error, RuntimeResult};
use super::function::LoxFunctionPtr;
use super::object::Object;

use crate::common::ast;
use crate::common::operator::{InfixOperator, LogicalOperator, PrefixOperator};

use std::rc::Rc;

pub struct Interpreter {
    pub env: Environment,
    pub globals: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut env = Environment::new();
        for builtin in get_builtins().into_iter() {
            let name = builtin.name().to_owned();
            let obj = Object::BuiltInFunction(builtin);
            env.define(name, obj);
        }
        let globals = env.clone();

        Interpreter { env, globals }
    }

    pub fn swap_environment(&mut self, mut env: Environment) -> Environment {
        std::mem::swap(&mut self.env, &mut env);
        env // which is now the old self.env
    }

    pub fn eval_statements(&mut self, stmts: Vec<ast::Stmt>) -> RuntimeResult<()> {
        for stmt in stmts.iter() {
            self.eval_statement(stmt)?;
        }
        Ok(())
    }

    pub fn eval_statement(&mut self, stmt: &ast::Stmt) -> RuntimeResult<()> {
        match stmt {
            ast::Stmt::Expression(expr) => {
                self.eval_expression(expr)?;
                Ok(())
            }
            ast::Stmt::Print(expr) => {
                println!("[out] {:?}", self.eval_expression(expr)?);
                Ok(())
            }
            ast::Stmt::IfElse(cond, body, else_body) => self.eval_if_else(cond, body, else_body),
            ast::Stmt::While(cond, body) => self.eval_while(cond, body),
            ast::Stmt::VariableDecl(name, expr) => {
                let value = self.eval_expression(expr)?;
                self.env.define(name.clone(), value);
                Ok(())
            }
            ast::Stmt::Block(stmts) => self.eval_block(stmts),
            ast::Stmt::FunctionDecl(name, params, body) => {
                let func = LoxFunctionPtr::new(
                    name.clone(),
                    params.clone(),
                    *body.clone(),
                    self.env.clone(),
                );
                self.env.define(name.clone(), Object::LoxFunction(func));
                Ok(())
            }
            ast::Stmt::Return(expr) => {
                let value = self.eval_expression(expr)?;
                Err(Error::Return(value))
            }
        }
    }

    fn eval_if_else(
        &mut self,
        condition: &ast::Expr,
        body: &ast::Stmt,
        else_body: &Option<ast::Stmt>,
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

    fn eval_block(&mut self, stmts: &Vec<ast::Stmt>) -> RuntimeResult<()> {
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
        match expr {
            ast::Expr::NumberLiteral(n) => Ok(Object::Number(*n as i64)),
            ast::Expr::BooleanLiteral(b) => Ok(Object::Boolean(*b)),
            ast::Expr::StringLiteral(s) => Ok(Object::String(s.clone())),
            ast::Expr::NilLiteral => Ok(Object::Nil),
            ast::Expr::Infix(op, lhs, rhs) => self.eval_infix_operator(*op, lhs, rhs),
            ast::Expr::Prefix(op, expr) => self.eval_prefix_operator(*op, expr),
            ast::Expr::Logical(op, lhs, rhs) => self.eval_logical_operator(*op, lhs, rhs),
            ast::Expr::Variable(name) => self.env.get(name),
            ast::Expr::Assignment(name, expr) => {
                let value = self.eval_expression(expr)?;
                self.env.set(name.clone(), value.clone())?;
                Ok(value)
            }
            ast::Expr::Call(callee, args) => self.eval_function_call(callee, args),
        }
    }

    fn eval_infix_operator(
        &mut self,
        op: InfixOperator,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
    ) -> RuntimeResult<Object> {
        let lhs = self.eval_expression(lhs)?;
        let rhs = self.eval_expression(rhs)?;

        match op {
            InfixOperator::Add => match (lhs, rhs) {
                (Object::Number(a), Object::Number(b)) => Ok(Object::Number(a + b)),
                (Object::String(s), Object::String(t)) => Ok(Object::String(s + &t)),
                (lhs, rhs) => Err(Error::IllegalInfixOperation(op, lhs, rhs)),
            },
            InfixOperator::Subtract => numerical_binop(op, lhs, rhs, |a, b| Object::Number(a - b)),
            InfixOperator::Multiply => numerical_binop(op, lhs, rhs, |a, b| Object::Number(a * b)),
            // Division is special to avoid div by zero panic
            InfixOperator::Divide => match (lhs, rhs) {
                (Object::Number(a), Object::Number(b)) => {
                    if b != 0 {
                        Ok(Object::Number(a / b))
                    } else {
                        Err(Error::DivideByZero)
                    }
                }
                (lhs, rhs) => Err(Error::IllegalInfixOperation(op, lhs, rhs)),
            },
            InfixOperator::EqualTo => Ok(Object::Boolean(lhs == rhs)),
            InfixOperator::NotEqualTo => Ok(Object::Boolean(lhs != rhs)),
            InfixOperator::GreaterThan => {
                numerical_binop(op, lhs, rhs, |a, b| Object::Boolean(a > b))
            }
            InfixOperator::GreaterEq => {
                numerical_binop(op, lhs, rhs, |a, b| Object::Boolean(a >= b))
            }
            InfixOperator::LessThan => numerical_binop(op, lhs, rhs, |a, b| Object::Boolean(a < b)),
            InfixOperator::LessEq => numerical_binop(op, lhs, rhs, |a, b| Object::Boolean(a <= b)),
        }
    }

    fn eval_prefix_operator(
        &mut self,
        op: PrefixOperator,
        expr: &ast::Expr,
    ) -> RuntimeResult<Object> {
        let value = self.eval_expression(expr)?;

        match op {
            PrefixOperator::Negate => match value {
                Object::Number(n) => Ok(Object::Number(-n)),
                _ => Err(Error::IllegalPrefixOperation(op, value)),
            },
            PrefixOperator::LogicalNot => Ok(Object::Boolean(!value.is_truthy())),
        }
    }

    fn eval_logical_operator(
        &mut self,
        op: LogicalOperator,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
    ) -> RuntimeResult<Object> {
        // The short-circuiting means we can't yet evaluate the RHS.
        let lhs = self.eval_expression(lhs)?;
        match op {
            LogicalOperator::And if !lhs.is_truthy() => Ok(lhs),
            LogicalOperator::Or if lhs.is_truthy() => Ok(lhs),
            _ => self.eval_expression(rhs),
        }
    }

    fn eval_function_call(
        &mut self,
        callee: &ast::Expr,
        arg_exprs: &Vec<ast::Expr>,
    ) -> RuntimeResult<Object> {
        let callee = self.eval_expression(callee)?;

        let mut args = Vec::with_capacity(arg_exprs.len());
        for expr in arg_exprs.iter() {
            args.push(self.eval_expression(expr)?);
        }

        callee.execute_call(args, self)
    }
}

fn numerical_binop<F>(
    op: InfixOperator,
    lhs: Object,
    rhs: Object,
    closure: F,
) -> RuntimeResult<Object>
where
    F: Fn(i64, i64) -> Object,
{
    match (lhs, rhs) {
        (Object::Number(a), Object::Number(b)) => Ok(closure(a, b)),
        (lhs, rhs) => Err(Error::IllegalInfixOperation(op, lhs, rhs)),
    }
}
