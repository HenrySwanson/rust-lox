use super::chunk::ChunkConstant;

#[derive(Debug)]
pub enum CompilerError {
    LocalAlreadyExists(String),
    LocalUsedInOwnInitializer(String),
    TooManyLocals,
    TooManyUpvalues,
}

// TODO how can i get the failed instruction in here?
#[derive(Debug)]
pub enum RuntimeError {
    InvalidOpcode(u8),
    DivideByZero,
    IncorrectOperandType,
    StackEmpty,
    InvalidStackIndex,
    UndefinedGlobal(String),
    NotACallable,
    WrongArity,
    NativeError(String),
    UntranslatableConstant(ChunkConstant),
}

pub type CompilerResult<T> = Result<T, CompilerError>;
pub type RuntimeResult<T> = Result<T, RuntimeError>;
