use std::cell::RefCell;
use std::collections::HashMap;
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
    LoxClass {
        name: InternedString,
    },
    LoxInstance {
        class: GcPtr<HeapObject>, // TODO is it time for multiple heaps?
        fields: HashMap<InternedString, Value>,
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

    pub fn mark_internals(&self) {
        match self {
            Value::Number(_) | Value::Boolean(_) | Value::Nil | Value::String(_) => {}
            Value::Obj(ptr) => ptr.mark(),
        }
    }

    pub fn try_into_heap_object(self) -> Option<GcPtr<HeapObject>> {
        match self {
            Value::Obj(gc_ptr) => Some(gc_ptr),
            _ => None,
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
            Value::Obj(handle) => match &*handle.try_borrow() {
                Some(obj) => obj.fmt(f),
                None => write!(f, "<garbage>"),
            },
        }
    }
}

impl fmt::Debug for HeapObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HeapObject::LoxClosure { name, .. } => write!(f, "<fn {}>", name),
            HeapObject::NativeFunction { name, .. } => write!(f, "<native fn {}>", name),
            HeapObject::LoxClass { name, .. } => write!(f, "{}", name),
            HeapObject::LoxInstance { class, .. } => write!(f, "{:?} instance", class.borrow()),
        }
    }
}

impl Traceable for HeapObject {
    fn trace(&self) {
        match self {
            HeapObject::LoxClosure { upvalues, .. } => {
                // A closed upvalue owns an object, which must be marked.
                for u in upvalues.iter() {
                    u.mark_internals();
                }
            }
            HeapObject::NativeFunction { .. } => {
                // nothing to do here
            }
            HeapObject::LoxClass { .. } => {
                // A class only has a name; nothing to do here
                // TODO revisit this
            }
            HeapObject::LoxInstance { class, fields, .. } => {
                // An instance has many things it can access
                class.mark();
                for f in fields.values() {
                    f.mark_internals();
                }
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

    pub fn get_open_idx(&self) -> Option<usize> {
        match &*self.location.borrow() {
            UpvalueType::Open(idx) => Some(*idx),
            UpvalueType::Closed(_) => None,
        }
    }

    pub fn mark_internals(&self) {
        match &*self.location.borrow() {
            UpvalueType::Open(_) => {}
            UpvalueType::Closed(value) => value.mark_internals(),
        }
    }
}
