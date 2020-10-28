use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use super::chunk::Chunk;
use super::gc::{GcPtr, Traceable};
use super::string_interning::InternedString;

#[derive(Clone, PartialEq, Eq)]
pub enum Value {
    Number(i64), // TODO should be f64, just like Token::Number et al
    Boolean(bool),
    Nil,
    String(InternedString),
    Closure(GcPtr<LoxClosure>),
    NativeFunction(NativeFunction), // no GC in here
    Class(GcPtr<LoxClass>),
    Instance(GcPtr<LoxInstance>),
    BoundMethod(GcPtr<LoxBoundMethod>),
}

pub struct LoxClosure {
    pub name: InternedString,
    pub arity: usize,
    pub chunk: Rc<Chunk>,
    pub upvalues: Rc<Vec<LiveUpvalue>>,
}

// Native functions are kinda tricky -- should be interned like Strings
// TODO move to native.rs

pub type NativeFnType = fn(&[Value]) -> Result<Value, String>;

#[derive(Clone)]
pub struct NativeFunction(pub Rc<NativeFunctionData>);

pub struct NativeFunctionData {
    pub name: InternedString,
    pub arity: usize,
    pub function: NativeFnType,
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for NativeFunction {}

// -- end native stuff

pub struct LoxClass {
    pub name: InternedString,
    pub methods: HashMap<InternedString, GcPtr<LoxClosure>>,
}

pub struct LoxInstance {
    pub class: GcPtr<LoxClass>,
    pub fields: HashMap<InternedString, Value>,
}

pub struct LoxBoundMethod {
    pub receiver: GcPtr<LoxInstance>,
    pub closure: GcPtr<LoxClosure>,
}

// change names around
#[derive(Clone)]
pub struct LiveUpvalue {
    location: Rc<RefCell<UpvalueType>>,
}

#[derive(Clone)]
pub enum UpvalueType {
    Open(usize),   // lives on the stack
    Closed(Value), // was popped off the stack, owned by this upvalue
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil | Value::Boolean(false) => false,
            _ => true,
        }
    }

    // TODO rename
    pub fn mark_internals(&self) {
        match self {
            Value::Number(_) | Value::Boolean(_) | Value::Nil | Value::String(_) => {}
            Value::Closure(ptr) => ptr.mark(),
            Value::NativeFunction(_) => {}
            Value::Class(ptr) => ptr.mark(),
            Value::Instance(ptr) => ptr.mark(),
            Value::BoundMethod(ptr) => ptr.mark(),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        macro_rules! fmt_or_garbage {
            ($handle:expr, $f:expr) => {
                match &*$handle.try_borrow() {
                    Some(obj) => obj.fmt($f),
                    None => write!($f, "<garbage>"),
                }
            };
        }

        match self {
            Value::Number(n) => n.fmt(f),
            Value::Boolean(b) => b.fmt(f),
            Value::Nil => write!(f, "nil"),
            Value::String(s) => s.fmt(f),
            Value::Closure(ptr) => fmt_or_garbage!(ptr, f),
            Value::NativeFunction(func) => func.fmt(f),
            Value::Class(ptr) => fmt_or_garbage!(ptr, f),
            Value::Instance(ptr) => fmt_or_garbage!(ptr, f),
            Value::BoundMethod(ptr) => fmt_or_garbage!(ptr, f),
        }
    }
}

impl fmt::Debug for LoxClosure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<native fn {}>", self.0.name)
    }
}

impl fmt::Debug for LoxClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Debug for LoxInstance {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "instance of {:?}", self.class.borrow())
    }
}

impl fmt::Debug for LoxBoundMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.closure.borrow().fmt(f)
    }
}

impl Traceable for LoxClosure {
    fn trace(&self) {
        // A closed upvalue owns an object, so we have to mark our upvalues
        for u in self.upvalues.iter() {
            u.mark_internals();
        }
    }
}

impl Traceable for LoxClass {
    fn trace(&self) {
        // Mark our methods
        for m in self.methods.values() {
            m.mark();
        }
    }
}

impl Traceable for LoxInstance {
    fn trace(&self) {
        // Mark our class and our fields (methods are marked through the class)
        self.class.mark();
        for f in self.fields.values() {
            f.mark_internals();
        }
    }
}

impl Traceable for LoxBoundMethod {
    fn trace(&self) {
        self.receiver.mark();
        self.closure.mark();
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
