use super::environment::Environment;
use super::errs::{Error, RuntimeResult};
use super::object::Object;
use crate::common::ast;
use crate::common::operator::{InfixOperator, PrefixOperator};

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Environment::new(),
        }
    }

    pub fn eval_statements(&mut self, stmts: Vec<ast::Stmt>) -> RuntimeResult<()> {
        for stmt in stmts.iter() {
            self.eval_statement(stmt)?;
        }
        Ok(())
    }

    fn eval_statement(&mut self, stmt: &ast::Stmt) -> RuntimeResult<()> {
        match stmt {
            ast::Stmt::Expression(expr) => {
                self.eval_expression(expr)?;
            }
            ast::Stmt::Print(expr) => {
                println!("[out] {:?}", self.eval_expression(expr)?);
            }
            ast::Stmt::VariableDecl(name, expr) => {
                let value = self.eval_expression(expr)?;
                self.env.define(name.clone(), value);
            }
        }

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
            ast::Expr::Variable(name) => self.env.get(name),
            ast::Expr::Assignment(name, expr) => {
                let value = self.eval_expression(expr)?;
                self.env.set(name.clone(), value.clone())?;
                Ok(value)
            }
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
            InfixOperator::Divide => numerical_binop(op, lhs, rhs, |a, b| Object::Number(a / b)),
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
