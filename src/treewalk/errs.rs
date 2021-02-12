use super::object::Object;
use crate::frontend::ast::{BinaryOperator, UnaryOperator};

// TODO: separate Error and not-error (e.g. return)?
#[derive(Debug)]
pub enum Error {
    Return(Object), // Not really an error! But it acts a lot like one...
    IllegalBinOperation(BinaryOperator, Object, Object),
    IllegalUnaryOperation(UnaryOperator, Object),
    UndefinedVariable(String),
    DivideByZero,
    WrongArity(usize, usize),
    NotACallable(Object),
    NotAnInstance(Object),
    NotAClass(Object),
    NoSuchProperty(Object, String),
}

pub type RuntimeResult<T> = Result<T, Error>;
