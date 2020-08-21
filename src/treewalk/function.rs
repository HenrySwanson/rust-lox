use super::environment::Environment;
use super::errs::{Error, RuntimeResult};
use super::interpreter::Interpreter;
use super::object::Object;

use crate::common::ast;

use std::fmt;
use std::rc::Rc;

struct LoxFunctionData {
    name: String,
    params: Vec<String>,
    body: ast::Stmt,
    closure: Environment,
}

#[derive(Clone)]
pub struct LoxFunctionPtr(Rc<LoxFunctionData>);

impl LoxFunctionPtr {
    pub fn new(name: String, params: Vec<String>, body: ast::Stmt, closure: Environment) -> Self {
        let data = LoxFunctionData {
            name,
            params,
            body,
            closure,
        };
        LoxFunctionPtr(Rc::new(data))
    }

    // TODO this is gross! can you pass around the environment explicitly maybe?
    pub fn execute_call(
        &self,
        args: Vec<Object>,
        interpreter: &mut Interpreter,
    ) -> RuntimeResult<Object> {
        if self.0.params.len() != args.len() {
            return Err(Error::WrongArity(self.0.params.len(), args.len()));
        }

        let mut env = Environment::with_enclosing(&self.0.closure);

        for (param, arg) in self.0.params.iter().zip(args.into_iter()) {
            env.define(param.clone(), arg);
        }

        // Switch to the new environment and execute the statement body.
        // Return behaves like an error in that it propagates upwards
        // until we catch it. Catch it here.
        let old_env = interpreter.swap_environment(env);
        let result = match interpreter.eval_statement(&self.0.body) {
            Ok(_) => Ok(Object::Nil),
            Err(Error::Return(obj)) => Ok(obj),
            Err(e) => Err(e),
        };

        interpreter.swap_environment(old_env);
        result
    }
}

impl fmt::Debug for LoxFunctionPtr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<function {}>", self.0.name)
    }
}

impl PartialEq<LoxFunctionPtr> for LoxFunctionPtr {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for LoxFunctionPtr {}
