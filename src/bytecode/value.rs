use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

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

pub type NativeFnType = fn(&[Value]) -> Result<Value, String>;

pub enum HeapObject {
    LoxClosure {
        // TODO pull this into a struct, so you can stuff it into a callframe
        name: InternedString,
        arity: usize,
        chunk: Rc<Chunk>,
        upvalues: Rc<Vec<LiveUpvalue>>,
    },
    NativeFunction {
        name: InternedString,
        arity: usize,
        function: NativeFnType,
    },
}

#[derive(Clone)]
pub struct LiveUpvalue {
    location: Rc<RefCell<UpvalueType>>,
}

#[derive(Clone)]
pub enum UpvalueType {
    Open(usize), // lives on the stack
    Closed(Value), // was popped off the stack
                 // this seems like a reference cycle risk, but there's no way to get
                 // back to an upvalue without going through a Gc<HeapObject>, allowing
                 // for garbage collection to claim it
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
            Value::Obj(handle) => write!(f, "(heap) {:?}", handle),
        }
    }
}

impl Traceable for HeapObject {
    // TODO return iterator instead? no heap allocation
    fn trace(&self) {
        match self {
            HeapObject::LoxClosure { .. } => {
                // nothing to do here (yet!)
            }
            HeapObject::NativeFunction { .. } => {
                // also nothing
            }
        }
    }
}

impl LiveUpvalue {
    pub fn new(idx: usize) -> Self {
        LiveUpvalue {
            location: Rc::new(RefCell::new(UpvalueType::Open(idx))),
        }
    }

    pub fn close(&self, value: Value) {
        self.location.replace(UpvalueType::Closed(value));
    }

    pub fn get_if_closed(&self) -> Result<Value, usize> {
        // Returns None if the fetch succeeded (i.e. this was a closed upvalue),
        // otherwise, returns the stack index to read from.
        match &*self.location.borrow() {
            UpvalueType::Open(idx) => Err(*idx),
            UpvalueType::Closed(value) => Ok(value.clone()),
        }
    }

    pub fn set_if_closed(&self, value: &Value) -> Result<(), usize> {
        // You'd think this would be a &mut self, but if we do that, then
        // the Rc<Vec< >> can't set anything :\
        // Same motivation as .close() really.
        match &mut *self.location.borrow_mut() {
            UpvalueType::Open(idx) => Err(*idx),
            UpvalueType::Closed(v) => {
                *v = value.clone();
                Ok(())
            }
        }
    }
}
