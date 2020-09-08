#[derive(Debug)]
pub enum Error {
    InvalidOpcode(u8),
    DivideByZero,
    IncorrectOperandType,
    StackEmpty,
    InvalidStackIndex,
}

pub type VmResult<T> = Result<T, Error>;

// TODO how can i get the failed instruction in here?
