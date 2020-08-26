use super::errs::{Error, RuntimeResult};
use super::function::LoxFunctionPtr;
use super::interpreter::Interpreter;
use super::object::Object;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

struct LoxClassData {
    name: String,
    methods: HashMap<String, LoxFunctionPtr>,
}

#[derive(Clone)]
pub struct LoxClassPtr(Rc<LoxClassData>);

pub struct LoxInstanceData {
    class: LoxClassPtr,
    // refcell is used so that multiple Rcs can change the data
    props: RefCell<HashMap<String, Object>>,
}

#[derive(Clone)]
pub struct LoxInstancePtr(Rc<LoxInstanceData>);

impl LoxClassPtr {
    pub fn new(name: String, methods: HashMap<String, LoxFunctionPtr>) -> Self {
        let data = LoxClassData { name, methods };
        LoxClassPtr(Rc::new(data))
    }

    pub fn execute_call(
        &self,
        args: Vec<Object>,
        _interpreter: &mut Interpreter,
    ) -> RuntimeResult<Object> {
        // Arity is always zero
        if args.len() != 0 {
            return Err(Error::WrongArity(0, args.len()));
        }

        let instance = LoxInstancePtr::new(self);
        Ok(Object::LoxInstance(instance))
    }
}

impl LoxInstancePtr {
    pub fn new(class: &LoxClassPtr) -> Self {
        let data = LoxInstanceData {
            class: class.clone(),
            props: RefCell::new(HashMap::new()),
        };
        LoxInstancePtr(Rc::new(data))
    }

    pub fn get(&self, name: &str) -> RuntimeResult<Object> {
        // Check if it's a property
        if let Some(value) = self.0.props.borrow().get(name) {
            return Ok(value.clone());
        }

        // Check if it's a method
        if let Some(method_ptr) = self.0.class.0.methods.get(name) {
            let self_as_instance = Object::LoxInstance(self.clone());
            let bound_method = method_ptr.clone().bind(self_as_instance);
            return Ok(Object::LoxFunction(bound_method));
        }

        let obj = Object::LoxInstance(self.clone());
        Err(Error::NoSuchProperty(obj, name.to_owned()))
    }

    pub fn set(&self, property: &str, value: Object) -> RuntimeResult<()> {
        self.0.props.borrow_mut().insert(property.to_owned(), value);
        Ok(())
    }
}

impl fmt::Debug for LoxClassPtr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<class {}>", self.0.name)
    }
}

impl fmt::Debug for LoxInstancePtr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<instance of {}>", self.0.class.0.name)
    }
}

impl PartialEq<LoxClassPtr> for LoxClassPtr {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl PartialEq<LoxInstancePtr> for LoxInstancePtr {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for LoxClassPtr {}

impl Eq for LoxInstancePtr {}
