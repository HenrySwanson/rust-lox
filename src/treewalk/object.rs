use super::builtins::BuiltInFn;
use super::errs::{Error, RuntimeResult};
use super::interpreter::Interpreter;

use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Number(i64),
    Boolean(bool),
    String(String),
    Nil,
    BuiltInFunction(Rc<BuiltInFn>), // Rc so that cloning doesn't need to copy the fn
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
            _ => Err(Error::NotACallable(self.clone())),
        }
    }
}
