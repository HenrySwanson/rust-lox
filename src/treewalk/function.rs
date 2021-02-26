use super::constants::THIS_STR;
use super::environment::Environment;
use super::errs::{Error, RuntimeResult};
use super::interpreter::Interpreter;
use super::object::Object;

use super::ast;

use std::fmt;
use std::rc::Rc;

struct LoxFunctionData {
    fn_data: ast::FunctionDecl,
    is_initializer: bool,
    closure: Environment,
}

#[derive(Clone)]
pub struct LoxFunctionPtr(Rc<LoxFunctionData>);

impl LoxFunctionPtr {
    pub fn new(fn_data: ast::FunctionDecl, is_initializer: bool, closure: Environment) -> Self {
        let data = LoxFunctionData {
            fn_data,
            is_initializer,
            closure,
        };
        LoxFunctionPtr(Rc::new(data))
    }

    pub fn arity(&self) -> usize {
        self.0.fn_data.params.len()
    }

    pub fn execute_call(
        &self,
        args: Vec<Object>,
        interpreter: &mut Interpreter,
    ) -> RuntimeResult<Object> {
        if self.arity() != args.len() {
            return Err(Error::WrongArity(self.arity(), args.len()));
        }

        let env = Environment::with_enclosing(&self.0.closure);

        for (param, arg) in self.0.fn_data.params.iter().zip(args.into_iter()) {
            env.define(param.clone(), arg);
        }

        // Switch to the new environment and execute the statement body.
        // Return behaves like an error in that it propagates upwards
        // until we catch it. Catch it here.
        let old_env = interpreter.swap_environment(env);
        let result = match interpreter.eval_statement(&self.0.fn_data.body) {
            Ok(_) => Ok(Object::Nil),
            Err(Error::Return(obj)) => Ok(obj),
            Err(e) => Err(e),
        };

        interpreter.swap_environment(old_env);

        // Now we throw errors, after swapping the env back in
        let result = result?;

        // Initializers are special kinds of functions, deal with them here
        if self.0.is_initializer {
            let this = self.0.closure.get(THIS_STR);
            let this = this.expect("`this` not found in bound method closure");
            Ok(this)
        } else {
            Ok(result)
        }
    }

    pub fn bind(&self, instance: Object) -> LoxFunctionPtr {
        // We need to create another environment, one containing "this"
        let new_env = Environment::with_enclosing(&self.0.closure);
        new_env.define(THIS_STR.to_owned(), instance);

        LoxFunctionPtr::new(self.0.fn_data.clone(), self.0.is_initializer, new_env)
    }
}

impl fmt::Debug for LoxFunctionPtr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<function {}>", self.0.fn_data.name)
    }
}

impl PartialEq<LoxFunctionPtr> for LoxFunctionPtr {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for LoxFunctionPtr {}
