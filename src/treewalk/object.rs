use super::builtins::BuiltInFnPtr;
use super::class::{LoxClassPtr, LoxInstancePtr};
use super::errs::{Error, RuntimeResult};
use super::function::LoxFunctionPtr;
use super::interpreter::Interpreter;

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
}
