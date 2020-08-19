use super::environment::Environment;
use super::errs::{Error, RuntimeResult};
use super::interpreter::Interpreter;
use super::object::Object;

use crate::common::ast;

use std::fmt;

#[derive(PartialEq, Eq, Clone)]
pub struct LoxFunction {
    name: String,
    params: Vec<String>,
    body: ast::Stmt,
}

impl LoxFunction {
    pub fn new(name: String, params: Vec<String>, body: ast::Stmt) -> Self {
        LoxFunction { name, params, body }
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }

    // TODO this is gross! can you pass around the environment explicitly maybe?
    pub fn execute_call(
        &self,
        args: Vec<Object>,
        interpreter: &mut Interpreter,
    ) -> RuntimeResult<Object> {
        if self.params.len() != args.len() {
            return Err(Error::WrongArity(self.params.len(), args.len()));
        }

        let mut env = Environment::with_enclosing(&interpreter.globals);

        for (param, arg) in self.params.iter().zip(args.into_iter()) {
            env.define(param.clone(), arg);
        }

        // Switch to the new environment and execute the statement body.
        // Return behaves like an error in that it propagates upwards
        // until we catch it. Catch it here.
        let old_env = interpreter.swap_environment(env);
        let result = match interpreter.eval_statement(&self.body) {
            Ok(_) => Ok(Object::Nil),
            Err(Error::Return(obj)) => Ok(obj),
            Err(e) => Err(e),
        };

        interpreter.swap_environment(old_env);
        result
    }
}

impl fmt::Debug for LoxFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<built-in {}>", self.name)
    }
}
