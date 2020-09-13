use std::fmt;

use super::gc::{GcPtr, Traceable};

#[derive(Clone)]
pub enum Value {
    Number(i64), // TODO should be f64, just like Token::Number et al
    Boolean(bool),
    Nil,
    Obj(GcPtr<HeapObject>),
}

pub enum HeapObject {
    String(String),
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
            // TODO: make this not unsafe
            Value::Obj(handle) => write!(f, "(heap) {:?}", unsafe { &*handle.raw_ptr }),
        }
    }
}

impl PartialEq<HeapObject> for HeapObject {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (HeapObject::String(s), HeapObject::String(t)) => s == t,
        }
    }
}

impl Eq for HeapObject {}

impl fmt::Debug for HeapObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HeapObject::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

impl Traceable for HeapObject {
    fn trace(&self) -> Vec<GcPtr<HeapObject>> {
        match self {
            HeapObject::String(_) => vec![],
        }
    }
}
