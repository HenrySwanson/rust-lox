use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use super::chunk::Chunk;
use super::gc::{GcPtr, Traceable};
use super::native::NativeFunction;
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
    pub upvalues: Rc<Vec<UpvalueRef>>,
}

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
    Method(GcPtr<LoxClosure>),
    NotFound,
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
        write!(f, "<native fn {}>", self.data.name)
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

impl LoxInstance {
    pub fn lookup(&self, name: &InternedString) -> PropertyLookup {
        // Look up fields first, then methods
        if let Some(value) = self.fields.get(name) {
            return PropertyLookup::Field(value.clone());
        }

        if let Some(method_ptr) = self.class.borrow().methods.get(name) {
            return PropertyLookup::Method(method_ptr.clone());
        }

        return PropertyLookup::NotFound;
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

    pub fn mark_internals(&self) {
        match &*self.data.borrow() {
            UpvalueData::Open(_) => {} // nothing to do; the stack will mark it
            UpvalueData::Closed(value) => value.mark_internals(),
        }
    }
}
