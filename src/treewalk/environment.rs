use super::errs::{Error, RuntimeResult};
use super::object::Object;
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Environment {
    ptr: Rc<RefCell<EnvInterior>>,
}

struct EnvInterior {
    values: HashMap<String, Object>,
    enclosing: Option<Environment>,
}

impl Environment {
    pub fn new() -> Self {
        let interior = EnvInterior {
            values: HashMap::new(),
            enclosing: None,
        };
        Environment {
            ptr: Rc::new(RefCell::new(interior)),
        }
    }

    // Returns an environment that encloses this one
    pub fn with_enclosing(env: &Environment) -> Self {
        let interior = EnvInterior {
            values: HashMap::new(),
            enclosing: Some(env.clone()),
        };
        Environment {
            ptr: Rc::new(RefCell::new(interior)),
        }
    }

    /// Define (or redefine) a variable
    pub fn define(&mut self, name: String, value: Object) {
        self.ptr.borrow_mut().values.insert(name, value);
    }

    /// Set an existing variable
    pub fn set(&mut self, name: String, value: Object) -> RuntimeResult<()> {
        let mut interior = self.ptr.borrow_mut();

        if let Some(slot) = interior.values.get_mut(&name) {
            *slot = value;
            return Ok(());
        }

        match interior.enclosing {
            Some(ref mut env) => env.set(name, value),
            None => Err(Error::UndefinedVariable(name)),
        }
    }

    pub fn get(&self, name: &str) -> RuntimeResult<Object> {
        let interior = self.ptr.borrow();

        match interior.values.get(name) {
            Some(obj) => Ok(obj.clone()),
            None => match &interior.enclosing {
                Some(env) => env.get(name),
                None => Err(Error::UndefinedVariable(name.to_owned())),
            },
        }
    }
}
