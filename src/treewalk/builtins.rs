use super::errs::{Error, RuntimeResult};
use super::interpreter::Interpreter;
use super::object::Object;
use std::fmt;

/// Since we can't derive Eq on function pointers, and LLVM can do funny things
/// under the hood, we require that every built-in have a distinct name.
/// We have total control over the created built-ins, so this is achievable.

pub struct BuiltInFn {
    pub func: fn(Vec<Object>, &mut Interpreter) -> RuntimeResult<Object>,
    pub arity: usize,
    pub name: String,
}

impl BuiltInFn {
    pub fn name(&self) -> String {
        self.name.clone()
    }

    pub fn execute_call(
        &self,
        args: Vec<Object>,
        interpreter: &mut Interpreter,
    ) -> RuntimeResult<Object> {
        if self.arity == args.len() {
            (self.func)(args, interpreter)
        } else {
            Err(Error::WrongArity(self.arity, args.len()))
        }
    }
}

impl fmt::Debug for BuiltInFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<built-in {}>", self.name)
    }
}

impl PartialEq<BuiltInFn> for BuiltInFn {
    fn eq(&self, other: &BuiltInFn) -> bool {
        self.name == other.name
    }
}

impl Eq for BuiltInFn {}

pub fn get_builtins() -> Vec<BuiltInFn> {
    vec![BuiltInFn {
        func: clock,
        arity: 0,
        name: "clock".to_owned(),
    }]
}

fn clock(_args: Vec<Object>, _interpreter: &mut Interpreter) -> RuntimeResult<Object> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let duration = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("SystemTime before UNIX EPOCH!");

    return Ok(Object::Number(duration.as_secs() as i64));
}
