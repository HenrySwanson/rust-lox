use std::cell::RefCell;
use std::collections::HashMap;

use std::rc::Rc;

use super::super::chunk::Chunk;
use super::gc::Gc;
use super::native::NativeFunction;
use super::super::string_interning::InternedString;

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    String(InternedString),
    Closure(Gc<LoxClosure>),
    NativeFunction(NativeFunction), // no GC in here
    Class(Gc<LoxClass>),
    Instance(Gc<LoxInstance>),
    BoundMethod(Gc<LoxBoundMethod>),
}

pub struct LoxClosure {
    pub name: InternedString,
    pub arity: usize,
    pub chunk: Rc<Chunk>,
    pub upvalues: Rc<[UpvalueRef]>,
}

pub struct LoxClass {
    pub name: InternedString,
    pub methods: HashMap<InternedString, Gc<LoxClosure>>,
}

pub struct LoxInstance {
    pub class: Gc<LoxClass>,
    pub fields: HashMap<InternedString, Value>,
}

pub struct LoxBoundMethod {
    pub receiver: Gc<LoxInstance>,
    pub closure: Gc<LoxClosure>,
}

// change names around
#[derive(Clone)]
pub struct UpvalueRef {
    data: Rc<RefCell<UpvalueData>>,
}

// This holds the actual value of an upvalue. If several functions capture
// the same variable, they'll each have UpvalueRefs that point to the same
// UpvalueOwner.
#[derive(Clone)]
pub enum UpvalueData {
    Open(usize),   // lives on the stack
    Closed(Value), // was popped off the stack, owned by this upvalue
}

pub enum PropertyLookup {
    Field(Value),
    Method(Gc<LoxClosure>),
    NotFound,
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Boolean(false))
    }
}

impl UpvalueRef {
    pub fn new_open(idx: usize) -> Self {
        UpvalueRef {
            data: Rc::new(RefCell::new(UpvalueData::Open(idx))),
        }
    }

    pub fn borrow(&self) -> std::cell::Ref<UpvalueData> {
        self.data.borrow()
    }

    pub fn borrow_mut(&mut self) -> std::cell::RefMut<UpvalueData> {
        self.data.borrow_mut()
    }

    pub fn close_over_value(&self, value: Value) {
        if let UpvalueData::Closed(_) = &*self.data.borrow() {
            panic!("Attempted to close over closed upvalue!");
        }
        self.data.replace(UpvalueData::Closed(value));
    }

    pub fn get_open_idx(&self) -> Option<usize> {
        match &*self.data.borrow() {
            UpvalueData::Open(idx) => Some(*idx),
            UpvalueData::Closed(_) => None,
        }
    }

    pub fn with_closed_value<F, T>(&self, function: F) -> Option<T>
    where
        F: Fn(&Value) -> T,
    {
        match &*self.data.borrow() {
            UpvalueData::Open(_) => None,
            UpvalueData::Closed(value) => Some(function(value)),
        }
    }
}
