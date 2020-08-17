#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Number(i64),
    Boolean(bool),
    String(String),
    Nil,
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Nil => false,
            Object::Boolean(false) => false,
            _ => true,
        }
    }
}
