use super::errs::{Error, RuntimeResult};
use super::object::Object;
use std::collections::HashMap;

pub struct Environment {
    values: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    /// Define (or redefine) a variable
    pub fn define(&mut self, name: String, value: Object) {
        self.values.insert(name, value);
    }

    /// Set an existing variable
    pub fn set(&mut self, name: String, value: Object) -> RuntimeResult<()> {
        use std::collections::hash_map::Entry;
        match self.values.entry(name) {
            Entry::Occupied(mut o) => {
                o.insert(value);
                Ok(())
            }
            Entry::Vacant(v) => Err(Error::UndefinedVariable(v.key().clone())),
        }
    }

    pub fn get(&self, name: &str) -> RuntimeResult<Object> {
        match self.values.get(name) {
            Some(obj) => Ok(obj.clone()),
            None => Err(Error::UndefinedVariable(name.to_owned())),
        }
    }
}
