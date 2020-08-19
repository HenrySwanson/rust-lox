use super::builtins::BuiltInFn;
use super::errs::{Error, RuntimeResult};
use super::function::LoxFunction;
use super::interpreter::Interpreter;

use crate::common::ast;

use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Number(i64),
    Boolean(bool),
    String(String),
    Nil,
    // Rc so that cloning doesn't need to copy the fn
    BuiltInFunction(Rc<BuiltInFn>),
    LoxFunction(Rc<LoxFunction>),
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
            _ => Err(Error::NotACallable(self.clone())),
        }
    }
}
