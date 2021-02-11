use num_enum::{IntoPrimitive, TryFromPrimitive};

// TODO: rename to bytecode

pub type ConstantIdx = u8; // allow only 256 constants / chunk
pub const MAX_CONSTANTS: usize = 256;

// since the GET_LOCAL instruction takes a byte
pub type LocalIdx = u8;
pub const MAX_LOCALS: usize = 256;

pub type UpvalueIdx = u8;
pub const MAX_UPVALUES: usize = 256;

// Upvalues come in two types, and we record which one by writing an
// an additional byte.
pub const UPVALUE_KIND_RECURSIVE: u8 = 0;
pub const UPVALUE_KIND_IMMEDIATE: u8 = 1;

#[derive(Debug)]
pub enum OpcodeError {
    UnrecognizedOpcode(u8),
    OutOfBounds,
}

#[derive(Debug)]
pub enum RichOpcode {
    // Constants
    Constant(ConstantIdx),
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
    DefineGlobal(ConstantIdx),
    GetGlobal(ConstantIdx),
    SetGlobal(ConstantIdx),
    GetLocal(u8),
    SetLocal(u8),
    // Jumps
    Jump(u16),
    JumpIfFalse(u16),
    Loop(u16),
    // Closures and Upvalues
    MakeClosure(u8),
    GetUpvalue(u8),
    SetUpvalue(u8),
    CloseUpvalue,
    // Classes
    MakeClass(ConstantIdx),
    GetProperty(ConstantIdx),
    SetProperty(ConstantIdx),
    MakeMethod(ConstantIdx),
    Invoke(ConstantIdx, u8),
    Inherit,
    GetSuper(ConstantIdx),
    SuperInvoke(ConstantIdx, u8),
    // Other
    Call(u8),
    Return,
    Print,
    Pop,
}

#[derive(Debug, Clone, Copy, IntoPrimitive, TryFromPrimitive)]
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
    Invoke,
    Inherit,
    GetSuper,
    SuperInvoke,
    // Other
    Call,
    Return,
    Print,
    Pop,
}

impl RichOpcode {
    pub fn encode(code: &mut Vec<u8>, op: Self) {
        // helpers
        fn push_op(code: &mut Vec<u8>, opcode: OpCode) {
            code.push(opcode.into());
        }

        fn push_op_u8(code: &mut Vec<u8>, opcode: OpCode, byte: u8) {
            code.push(opcode.into());
            code.push(byte);
        }

        fn push_op_u16(code: &mut Vec<u8>, opcode: OpCode, short: u16) {
            let bytes = short.to_be_bytes();
            code.push(opcode.into());
            code.extend_from_slice(&bytes);
        }

        fn push_op_u8_u8(code: &mut Vec<u8>, opcode: OpCode, byte1: u8, byte2: u8) {
            code.push(opcode.into());
            code.push(byte1);
            code.push(byte2);
        }

        match op {
            RichOpcode::Constant(idx) => push_op_u8(code, OpCode::Constant, idx),
            RichOpcode::True => push_op(code, OpCode::True),
            RichOpcode::False => push_op(code, OpCode::False),
            RichOpcode::Nil => push_op(code, OpCode::Nil),
            RichOpcode::Add => push_op(code, OpCode::Add),
            RichOpcode::Subtract => push_op(code, OpCode::Subtract),
            RichOpcode::Multiply => push_op(code, OpCode::Multiply),
            RichOpcode::Divide => push_op(code, OpCode::Divide),
            RichOpcode::Negate => push_op(code, OpCode::Negate),
            RichOpcode::Not => push_op(code, OpCode::Not),
            RichOpcode::Equal => push_op(code, OpCode::Equal),
            RichOpcode::GreaterThan => push_op(code, OpCode::GreaterThan),
            RichOpcode::LessThan => push_op(code, OpCode::LessThan),
            RichOpcode::DefineGlobal(idx) => push_op_u8(code, OpCode::DefineGlobal, idx),
            RichOpcode::GetGlobal(idx) => push_op_u8(code, OpCode::GetGlobal, idx),
            RichOpcode::SetGlobal(idx) => push_op_u8(code, OpCode::SetGlobal, idx),
            RichOpcode::GetLocal(idx) => push_op_u8(code, OpCode::GetLocal, idx),
            RichOpcode::SetLocal(idx) => push_op_u8(code, OpCode::SetLocal, idx),
            RichOpcode::Jump(offset) => push_op_u16(code, OpCode::Jump, offset),
            RichOpcode::JumpIfFalse(offset) => push_op_u16(code, OpCode::JumpIfFalse, offset),
            RichOpcode::Loop(offset) => push_op_u16(code, OpCode::Loop, offset),
            RichOpcode::MakeClosure(idx) => push_op_u8(code, OpCode::MakeClosure, idx),
            RichOpcode::GetUpvalue(idx) => push_op_u8(code, OpCode::GetUpvalue, idx),
            RichOpcode::SetUpvalue(idx) => push_op_u8(code, OpCode::SetUpvalue, idx),
            RichOpcode::CloseUpvalue => push_op(code, OpCode::CloseUpvalue),
            RichOpcode::MakeClass(idx) => push_op_u8(code, OpCode::MakeClass, idx),
            RichOpcode::GetProperty(idx) => push_op_u8(code, OpCode::GetProperty, idx),
            RichOpcode::SetProperty(idx) => push_op_u8(code, OpCode::SetProperty, idx),
            RichOpcode::MakeMethod(idx) => push_op_u8(code, OpCode::MakeMethod, idx),
            RichOpcode::Invoke(idx, argc) => push_op_u8_u8(code, OpCode::Invoke, idx, argc),
            RichOpcode::Inherit => push_op(code, OpCode::Inherit),
            RichOpcode::GetSuper(idx) => push_op_u8(code, OpCode::GetSuper, idx),
            RichOpcode::SuperInvoke(idx, argc) => {
                push_op_u8_u8(code, OpCode::SuperInvoke, idx, argc)
            }
            RichOpcode::Call(argc) => push_op_u8(code, OpCode::Call, argc),
            RichOpcode::Return => push_op(code, OpCode::Return),
            RichOpcode::Print => push_op(code, OpCode::Print),
            RichOpcode::Pop => push_op(code, OpCode::Pop),
        }
    }

    pub fn decode(code: &[u8], offset: usize) -> Result<(RichOpcode, usize), OpcodeError> {
        use std::convert::{TryFrom, TryInto};

        // helpers
        macro_rules! read {
            ($opcode: ident) => {
                Ok((RichOpcode::$opcode, offset + 1))
            };
        }

        macro_rules! read_with_u8 {
            ($opcode: ident) => {
                match code.get(offset + 1) {
                    Some(byte) => {
                        let op = RichOpcode::$opcode(*byte);
                        Ok((op, offset + 2))
                    }
                    None => Err(OpcodeError::OutOfBounds),
                }
            };
        }

        macro_rules! read_with_u16 {
            ($opcode: ident) => {
                match code.get(offset + 1..offset + 3) {
                    Some(bytes) => {
                        let bytes: [u8; 2] = bytes.try_into().unwrap();
                        let short = u16::from_be_bytes(bytes);
                        let op = RichOpcode::$opcode(short);
                        Ok((op, offset + 3))
                    }
                    None => Err(OpcodeError::OutOfBounds),
                }
            };
        }

        macro_rules! read_with_u8_u8 {
            ($opcode: ident) => {
                match code.get(offset + 1..offset + 3) {
                    Some(bytes) => {
                        let op = RichOpcode::$opcode(bytes[0], bytes[1]);
                        Ok((op, offset + 3))
                    }
                    None => Err(OpcodeError::OutOfBounds),
                }
            };
        }

        let opcode = match code.get(offset).copied() {
            Some(byte) => match OpCode::try_from(byte) {
                Ok(x) => x,
                Err(_) => return Err(OpcodeError::UnrecognizedOpcode(byte)),
            },
            None => return Err(OpcodeError::OutOfBounds),
        };

        match opcode {
            OpCode::Constant => read_with_u8!(Constant),
            OpCode::True => read!(True),
            OpCode::False => read!(False),
            OpCode::Nil => read!(Nil),
            OpCode::Add => read!(Add),
            OpCode::Subtract => read!(Subtract),
            OpCode::Multiply => read!(Multiply),
            OpCode::Divide => read!(Divide),
            OpCode::Negate => read!(Negate),
            OpCode::Not => read!(Not),
            OpCode::Equal => read!(Equal),
            OpCode::GreaterThan => read!(GreaterThan),
            OpCode::LessThan => read!(LessThan),
            OpCode::DefineGlobal => read_with_u8!(DefineGlobal),
            OpCode::GetGlobal => read_with_u8!(GetGlobal),
            OpCode::SetGlobal => read_with_u8!(SetGlobal),
            OpCode::GetLocal => read_with_u8!(GetLocal),
            OpCode::SetLocal => read_with_u8!(SetLocal),
            OpCode::Jump => read_with_u16!(Jump),
            OpCode::JumpIfFalse => read_with_u16!(JumpIfFalse),
            OpCode::Loop => read_with_u16!(Loop),
            OpCode::MakeClosure => read_with_u8!(MakeClosure),
            OpCode::GetUpvalue => read_with_u8!(GetUpvalue),
            OpCode::SetUpvalue => read_with_u8!(SetUpvalue),
            OpCode::CloseUpvalue => read!(CloseUpvalue),
            OpCode::MakeClass => read_with_u8!(MakeClass),
            OpCode::GetProperty => read_with_u8!(GetProperty),
            OpCode::SetProperty => read_with_u8!(SetProperty),
            OpCode::MakeMethod => read_with_u8!(MakeMethod),
            OpCode::Invoke => read_with_u8_u8!(Invoke),
            OpCode::Inherit => read!(Inherit),
            OpCode::GetSuper => read_with_u8!(GetSuper),
            OpCode::SuperInvoke => read_with_u8_u8!(SuperInvoke),
            OpCode::Call => read_with_u8!(Call),
            OpCode::Return => read!(Return),
            OpCode::Print => read!(Print),
            OpCode::Pop => read!(Pop),
        }
    }
}
