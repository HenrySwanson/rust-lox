use super::chunk::ChunkConstant;
use super::opcode::OpcodeError;

#[derive(Debug)]
pub enum CompilerError {
    LocalAlreadyExists(String),
    LocalUsedInOwnInitializer(String),
    TooManyConstants,
    TooManyLocals,
    TooManyUpvalues,
    SelfInherit(String),
}

// TODO how can i get the failed instruction in here?
#[derive(Debug)]
pub enum RuntimeError {
    InvalidOpcode(u8),
    DivideByZero,
    IncorrectOperandType,
    StackEmpty,
    StackOverflow,
    InvalidStackIndex,
    UndefinedGlobal(String),
    NotACallable,
    WrongArity,
    NativeError(String),
    UntranslatableConstant(ChunkConstant),
    BadUpvalue,
    NotAClass,
    NotAnInstance,
    NotAClosure,
    UndefinedProperty,
    ArgumentsToDefaultInitializer,
    InstructionOutOfBounds,
}

pub type CompilerResult<T> = Result<T, CompilerError>;
pub type RuntimeResult<T> = Result<T, RuntimeError>;

impl From<OpcodeError> for RuntimeError {
    fn from(e: OpcodeError) -> Self {
        match e {
            OpcodeError::UnrecognizedOpcode(byte) => RuntimeError::InvalidOpcode(byte),
            OpcodeError::OutOfBounds => RuntimeError::InstructionOutOfBounds,
            OpcodeError::BadUpvalueKind(_) => RuntimeError::BadUpvalue,
        }
    }
}
