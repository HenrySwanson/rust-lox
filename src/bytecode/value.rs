use std::fmt;

use super::chunk::Chunk;
use super::gc::{GcPtr, Traceable};
use super::string_interning::InternedString;

#[derive(Clone)]
pub enum Value {
    Number(i64), // TODO should be f64, just like Token::Number et al
    Boolean(bool),
    Nil,
    String(InternedString),
    Obj(GcPtr<HeapObject>),
}

pub struct LoxFunctionData {
    pub name: InternedString,
    pub arity: usize,
    pub chunk: Chunk,
}

pub type NativeFnType = fn(&[Value]) -> Result<Value, String>;

pub struct NativeFnData {
    pub name: InternedString,
    pub arity: usize,
    pub function: NativeFnType,
}

pub enum HeapObject {
    LoxFunction(LoxFunctionData),
    NativeFunction(NativeFnData),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil | Value::Boolean(false) => false,
            _ => true,
        }
    }
}

impl PartialEq<Value> for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;
        match (self, other) {
            (Number(n), Number(m)) => n == m,
            (Boolean(a), Boolean(b)) => a == b,
            (Nil, Nil) => true,
            (String(s), String(t)) => s == t,
            (Obj(x), Obj(y)) => x == y,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(n) => n.fmt(f),
            Value::Boolean(b) => b.fmt(f),
            Value::Nil => write!(f, "nil"),
            Value::String(s) => s.fmt(f),
            // TODO: make this more informative
            Value::Obj(handle) => write!(f, "(heap) {:?}", handle.raw_ptr),
        }
    }
}

impl LoxFunctionData {
    pub fn new(name: InternedString, arity: usize, chunk: Chunk) -> Self {
        LoxFunctionData { name, arity, chunk }
    }
}

impl Traceable for HeapObject {
    fn trace(&self) -> Vec<GcPtr<HeapObject>> {
        match self {
            _ => todo!(),
        }
    }
}
