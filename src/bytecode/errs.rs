use super::chunk::ChunkConstant;
use super::opcode::OpcodeError;

#[derive(Debug)]
pub enum CompilerError {
    LocalAlreadyExists(String),
    LocalUsedInOwnInitializer(String),
    TooManyConstants,
    TooManyLocals,
    TooManyUpvalues,
    JumpTooLong,
    SelfInherit(String),
}

// TODO how can i get the failed instruction in here?
#[derive(Debug)]
pub enum RuntimeError {
    InvalidOpcode(u8),
    DivideByZero,
    IncorrectOperandTypeAdd,
    NonNumericOperandBinary,
    NonNumericOperandUnary,
    StackUnderflow,
    StackOverflow,
    InvalidStackIndex(usize),
    InvalidStackDepth(usize),
    UndefinedGlobal(String),
    NotACallable,
    WrongArity(usize, usize),
    NativeError(String),
    UntranslatableConstant(ChunkConstant),
    BadUpvalue,
    NotAClass,
    NotAnInstance,
    NotAClosure,
    UndefinedProperty(String),
    BadSuperclass,
    BadFieldAccess,
    BadPropertyAccess,
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

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::InvalidOpcode(byte) => write!(f, "Unrecognized opcode {}.", byte),
            RuntimeError::DivideByZero => write!(f, "Cannot divide by zero."),
            RuntimeError::IncorrectOperandTypeAdd => {
                write!(f, "Operands must be two numbers or two strings.")
            }
            RuntimeError::NonNumericOperandBinary => write!(f, "Operands must be numbers."),
            RuntimeError::NonNumericOperandUnary => write!(f, "Operand must be a number."),
            RuntimeError::StackUnderflow => write!(f, "Stack underflow."),
            RuntimeError::StackOverflow => write!(f, "Stack overflow."),
            RuntimeError::InvalidStackIndex(idx) => {
                write!(f, "Invalid stack access at index {}.", idx)
            }
            RuntimeError::InvalidStackDepth(depth) => {
                write!(f, "Invalid stack access at depth {}.", depth)
            }
            RuntimeError::UndefinedGlobal(var_name) => {
                write!(f, "Undefined variable '{}'.", var_name)
            }
            RuntimeError::NotACallable => write!(f, "Can only call functions and classes."),
            RuntimeError::WrongArity(expected, actual) => {
                write!(f, "Expected {} arguments but got {}.", expected, actual)
            }
            RuntimeError::NativeError(err_msg) => {
                write!(f, "Error when running native function: {}", err_msg)
            }
            // TODO: some of these i need to write messages for
            RuntimeError::UntranslatableConstant(_) => write!(f, "{:?}", self),
            RuntimeError::BadUpvalue => write!(f, "{:?}", self),
            RuntimeError::NotAClass => write!(f, "{:?}", self),
            RuntimeError::NotAnInstance => write!(f, "Only instances have properties."),
            RuntimeError::NotAClosure => write!(f, "{:?}", self),
            RuntimeError::UndefinedProperty(prop_name) => {
                write!(f, "Undefined property '{}'.", prop_name)
            }
            RuntimeError::BadSuperclass => write!(f, "Superclass must be a class."),
            RuntimeError::BadFieldAccess => write!(f, "Only instances have fields."),
            RuntimeError::BadPropertyAccess => write!(f, "Only instances have properties."),
            RuntimeError::InstructionOutOfBounds => write!(f, "{:?}", self),
        }
    }
}

impl std::error::Error for RuntimeError {}
