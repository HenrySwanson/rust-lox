use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
    Number(i64), // TODO should be f64, just like Token::Number et al
    Boolean(bool),
    Nil,
    HeapObject(Rc<Object>),
}

pub enum Object {
    String(String),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil | Value::Boolean(false) => false,
            _ => true,
        }
    }

    pub fn make_heap_object(obj: Object) -> Self {
        Value::HeapObject(Rc::new(obj))
    }

    pub fn try_add(&self, other: &Self) -> Option<Value> {
        let obj = match (self, other) {
            (Value::Number(n), Value::Number(m)) => Value::Number(n + m),
            (Value::HeapObject(x), Value::HeapObject(y)) => match (x.deref(), y.deref()) {
                (Object::String(s), Object::String(t)) => {
                    let obj = Object::String(s.clone() + t);
                    Value::make_heap_object(obj)
                }
            },
            _ => return None,
        };

        Some(obj)
    }
}

impl PartialEq<Value> for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;
        match (self, other) {
            (Number(n), Number(m)) => n == m,
            (Boolean(a), Boolean(b)) => a == b,
            (Nil, Nil) => true,
            (HeapObject(x), HeapObject(y)) => x == y,
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
            Value::HeapObject(obj) => write!(f, "(heap) {:?}", obj),
        }
    }
}

impl PartialEq<Object> for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::String(s), Object::String(t)) => s == t,
        }
    }
}

impl Eq for Object {}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::String(s) => write!(f, "\"{}\"", s),
        }
    }
}
