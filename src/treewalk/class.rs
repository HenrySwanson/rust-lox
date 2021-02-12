use super::constants::INIT_STR;
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
    superclass: Option<LoxClassPtr>,
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
    pub fn new(
        name: String,
        superclass: Option<LoxClassPtr>,
        methods: HashMap<String, LoxFunctionPtr>,
    ) -> Self {
        let data = LoxClassData {
            name,
            superclass,
            methods,
        };
        LoxClassPtr(Rc::new(data))
    }

    pub fn arity(&self) -> usize {
        match self.0.methods.get(INIT_STR) {
            Some(init) => init.arity(),
            None => 0,
        }
    }

    pub fn execute_call(
        &self,
        args: Vec<Object>,
        interpreter: &mut Interpreter,
    ) -> RuntimeResult<Object> {
        // Check the arity
        if self.arity() != args.len() {
            return Err(Error::WrongArity(self.arity(), args.len()));
        }

        // Create instance and run the initializer
        let instance = LoxInstancePtr::new(self);
        if let Some(init) = instance.find_bound_method(INIT_STR) {
            init.execute_call(args, interpreter)?;
        }

        Ok(Object::LoxInstance(instance))
    }

    pub fn find_method(&self, name: &str) -> Option<LoxFunctionPtr> {
        let method_ptr = self.0.methods.get(name).cloned();
        match &self.0.superclass {
            Some(superclass) => method_ptr.or_else(|| superclass.find_method(name)),
            None => method_ptr,
        }
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
        if let Some(obj) = self.find_property(name) {
            return Ok(obj);
        }

        // Check if it's a method
        if let Some(method) = self.find_bound_method(name) {
            return Ok(method);
        }

        let obj = Object::LoxInstance(self.clone());
        Err(Error::NoSuchProperty(obj, name.to_owned()))
    }

    pub fn set(&self, property: &str, value: Object) {
        self.0.props.borrow_mut().insert(property.to_owned(), value);
    }

    fn find_property(&self, name: &str) -> Option<Object> {
        self.0.props.borrow().get(name).cloned()
    }

    fn find_bound_method(&self, name: &str) -> Option<Object> {
        match self.0.class.find_method(name) {
            Some(method_ptr) => {
                let self_as_instance = Object::LoxInstance(self.clone());
                let bound_method = method_ptr.bind(self_as_instance);

                Some(Object::LoxFunction(bound_method))
            }
            None => None,
        }
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
