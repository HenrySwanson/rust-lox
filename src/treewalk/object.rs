use super::builtins::BuiltInFnPtr;
use super::class::{LoxClassPtr, LoxInstancePtr};
use super::errs::{Error, RuntimeResult};
use super::function::LoxFunctionPtr;
use super::interpreter::Interpreter;

use crate::common::operator::{InfixOperator, PrefixOperator};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Number(i64),
    Boolean(bool),
    String(String),
    Nil,
    BuiltInFunction(BuiltInFnPtr),
    LoxFunction(LoxFunctionPtr),
    LoxClass(LoxClassPtr),
    LoxInstance(LoxInstancePtr),
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Nil => false,
            Object::Boolean(false) => false,
            _ => true,
        }
    }

    pub fn execute_call(
        &self,
        args: Vec<Object>,
        interpreter: &mut Interpreter,
    ) -> RuntimeResult<Object> {
        match self {
            Object::BuiltInFunction(builtin) => builtin.execute_call(args, interpreter),
            Object::LoxFunction(func) => func.execute_call(args, interpreter),
            Object::LoxClass(class) => class.execute_call(args, interpreter),
            _ => Err(Error::NotACallable(self.clone())),
        }
    }

    pub fn apply_infix_op(op: InfixOperator, lhs: Object, rhs: Object) -> RuntimeResult<Object> {
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

    pub fn apply_prefix_op(op: PrefixOperator, value: Object) -> RuntimeResult<Object> {
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
