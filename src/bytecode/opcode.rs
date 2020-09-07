use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(Clone, Copy, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Return,
    Constant,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
}

impl OpCode {
    pub fn num_operands(&self) -> usize {
        match self {
            OpCode::Return => 0,
            OpCode::Constant => 1,
            OpCode::Add | OpCode::Subtract | OpCode::Multiply | OpCode::Divide | OpCode::Negate => {
                0
            }
        }
    }
}
