use super::value::Value;
use std::rc::Rc;

// Native functions are immutable, and carry around a lot of data. We should intern them
// like strings. This also solves some issues with equality of functions.

pub type NativeFnType = fn(&[Value]) -> Result<Value, String>;

#[derive(Clone)]
pub struct NativeFunction {
    pub data: Rc<NativeFunctionData>,
}

pub struct NativeFunctionData {
    pub name: String,
    pub arity: usize,
    pub function: NativeFnType,
}

impl NativeFunction {
    pub fn new(name: &str, arity: usize, function: NativeFnType) -> Self {
        NativeFunction {
            data: Rc::new(NativeFunctionData {
                name: name.to_owned(),
                arity,
                function,
            }),
        }
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.data, &other.data)
    }
}

impl Eq for NativeFunction {}

// -- list of native functions

pub fn get_natives() -> &'static [(&'static str, usize, NativeFnType)] {
    &[("clock", 0, clock)]
}

fn clock(_args: &[Value]) -> Result<Value, String> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let duration = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("SystemTime before UNIX EPOCH!");

    Ok(Value::Number(duration.as_secs() as i64))
}
