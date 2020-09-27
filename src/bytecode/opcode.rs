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
    // Other
    Return,
    Print,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
}

impl OpCode {
    pub fn num_operands(&self) -> usize {
        match self {
            OpCode::Constant => 1,
            OpCode::True | OpCode::False | OpCode::Nil => 0,
            OpCode::Add | OpCode::Subtract | OpCode::Multiply | OpCode::Divide | OpCode::Negate => {
                0
            }
            OpCode::Not => 0,
            OpCode::Equal | OpCode::GreaterThan | OpCode::LessThan => 0,
            OpCode::Return | OpCode::Print | OpCode::Pop => 0,
            OpCode::DefineGlobal | OpCode::GetGlobal | OpCode::SetGlobal => 1,
        }
    }
}
