use super::errs::{Error, RuntimeResult};
use super::object::Object;

use std::cell::RefCell;
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
    pub fn define(&self, name: String, value: Object) {
        self.ptr.borrow_mut().values.insert(name, value);
    }

    /// Set an existing variable
    pub fn set(&self, name: &str, value: Object) -> RuntimeResult<()> {
        match self.ptr.borrow_mut().values.get_mut(name) {
            Some(slot) => {
                *slot = value;
                Ok(())
            }
            None => Err(Error::UndefinedVariable(name.to_owned())),
        }
    }

    pub fn get(&self, name: &str) -> RuntimeResult<Object> {
        match self.ptr.borrow_mut().values.get(name) {
            Some(obj) => Ok(obj.clone()),
            None => Err(Error::UndefinedVariable(name.to_owned())),
        }
    }

    fn ancestor(&self, hops: usize) -> Environment {
        if hops == 0 {
            // Cloning is okay, because we'll create a new Rc to the same RefCell
            self.clone()
        } else {
            self.ptr
                .borrow()
                .enclosing
                .as_ref()
                .expect("Resolver error: hopped beyond global")
                .ancestor(hops - 1)
        }
    }

    pub fn set_at(&self, hops: usize, name: &str, value: Object) -> RuntimeResult<()> {
        self.ancestor(hops).set(name, value)
    }

    pub fn get_at(&mut self, hops: usize, name: &str) -> RuntimeResult<Object> {
        self.ancestor(hops).get(name)
    }
}
