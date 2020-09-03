use super::errs::{Error, RuntimeResult};
use super::interpreter::Interpreter;
use super::object::Object;

use std::fmt;
use std::rc::Rc;

type FnType = fn(Vec<Object>, &mut Interpreter) -> RuntimeResult<Object>;

struct BuiltInFnData {
    name: String,
    func: FnType,
    arity: usize,
}

#[derive(Clone)]
pub struct BuiltInFnPtr(Rc<BuiltInFnData>);

impl BuiltInFnPtr {
    // Private: only we can define builtins
    fn new(name: &str, func: FnType, arity: usize) -> Self {
        let name = name.to_owned();
        let data = BuiltInFnData { name, func, arity };
        BuiltInFnPtr(Rc::new(data))
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }

    pub fn execute_call(
        &self,
        args: Vec<Object>,
        interpreter: &mut Interpreter,
    ) -> RuntimeResult<Object> {
        if self.0.arity == args.len() {
            (self.0.func)(args, interpreter)
        } else {
            Err(Error::WrongArity(self.0.arity, args.len()))
        }
    }
}

impl fmt::Debug for BuiltInFnPtr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<built-in {}>", self.0.name)
    }
}

impl PartialEq<BuiltInFnPtr> for BuiltInFnPtr {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for BuiltInFnPtr {}

pub fn get_builtins() -> Vec<BuiltInFnPtr> {
    vec![BuiltInFnPtr::new("clock", clock, 0)]
}

fn clock(_args: Vec<Object>, _interpreter: &mut Interpreter) -> RuntimeResult<Object> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let duration = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("SystemTime before UNIX EPOCH!");

    return Ok(Object::Number(duration.as_secs() as i64));
}
