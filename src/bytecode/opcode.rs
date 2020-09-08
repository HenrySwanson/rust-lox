use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(Clone, Copy, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Return,
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
}

impl OpCode {
    pub fn num_operands(&self) -> usize {
        match self {
            OpCode::Return => 0,
            OpCode::Constant => 1,
            OpCode::True | OpCode::False | OpCode::Nil => 0,
            OpCode::Add | OpCode::Subtract | OpCode::Multiply | OpCode::Divide | OpCode::Negate => {
                0
            }
            OpCode::Not => 0,
            OpCode::Equal | OpCode::GreaterThan | OpCode::LessThan => 0,
        }
    }
}
