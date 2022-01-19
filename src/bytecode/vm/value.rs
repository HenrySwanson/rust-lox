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
    // The outer Rc is because multiple copies of the same
    // closure may share the exact same upvalue list (e.g., when
    // calling a closure, we push a copy of it onto the stack).
    // The inner Gc is because a) the upvalues are on the heap
    // and b) we may need to mutate them.
    pub upvalues: Rc<[Gc<Upvalue>]>,
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

// This holds the actual value of an upvalue. If several functions capture
// the same variable, they'll each have Gcs that point to the same
// Upvalue.
#[derive(Clone)]
pub enum Upvalue {
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