use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(Clone, Copy, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    // Constants
    Constant,
    True,
    False,
    Nil,
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    // Logical
    Not,
    // Comparison
    Equal,
    GreaterThan,
    LessThan,
    // Variables
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    // Jumps
    Jump,
    JumpIfFalse,
    Loop,
    // Closures and Upvalues
    MakeClosure,
    GetUpvalue,
    SetUpvalue,
    CloseUpvalue,
    // Classes
    MakeClass,
    GetProperty,
    SetProperty,
    MakeMethod,
    // Other
    Call,
    Return,
    Print,
    Pop,
}

impl OpCode {
    pub fn arg_size_in_bytes(&self) -> Option<usize> {
        let arg_bytes = match self {
            OpCode::Constant => 1,
            OpCode::True | OpCode::False | OpCode::Nil => 0,
            OpCode::Add | OpCode::Subtract | OpCode::Multiply | OpCode::Divide | OpCode::Negate => {
                0
            }
            OpCode::Not => 0,
            OpCode::Equal | OpCode::GreaterThan | OpCode::LessThan => 0,
            OpCode::DefineGlobal | OpCode::GetGlobal | OpCode::SetGlobal => 1,
            OpCode::GetLocal | OpCode::SetLocal => 1,
            OpCode::Jump | OpCode::JumpIfFalse | OpCode::Loop => 2,
            OpCode::MakeClosure => return None, // variable-length
            OpCode::GetUpvalue | OpCode::SetUpvalue => 1,
            OpCode::CloseUpvalue => 0,
            OpCode::MakeClass => 1,
            OpCode::GetProperty | OpCode::SetProperty => 1,
            OpCode::MakeMethod => 1,
            OpCode::Call => 1,
            OpCode::Return | OpCode::Print | OpCode::Pop => 0,
        };

        Some(arg_bytes)
    }
}
