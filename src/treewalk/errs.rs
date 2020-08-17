use super::object::Object;
use crate::common::operator::{InfixOperator, PrefixOperator};

#[derive(Debug)]
pub enum Error {
    IllegalInfixOperation(InfixOperator, Object, Object),
    IllegalPrefixOperation(PrefixOperator, Object),
    UndefinedVariable(String),
}

pub type RuntimeResult<T> = Result<T, Error>;
