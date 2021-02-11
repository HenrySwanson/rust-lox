use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::rc::Rc;

use super::opcode::{ConstantIdx, OpCode, OpcodeError, RichOpcode};
use super::string_interning::InternedString;

// Chunk constants are somewhat different from runtime values -- there's
// no recursion possible, and there's never heap allocation.
// We do allow string interning though, just for convenience.
#[derive(Clone)]
pub enum ChunkConstant {
    Number(u32),
    String(InternedString),
    FnTemplate {
        name: InternedString,
        arity: usize,
        chunk: Rc<Chunk>,
        upvalue_count: usize,
    },
}

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<ChunkConstant>,
    line_nos: Vec<usize>,
}

impl fmt::Debug for ChunkConstant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ChunkConstant::Number(n) => write!(f, "{}", n),
            ChunkConstant::String(s) => write!(f, "\"{}\"", s),
            ChunkConstant::FnTemplate { name, .. } => write!(f, "<fn {}>", name),
        }
    }
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: vec![],
            constants: vec![],
            line_nos: vec![],
        }
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn write_op(&mut self, op: RichOpcode, line_no: usize) {
        let l1 = self.len();
        RichOpcode::encode(&mut self.code, op);
        let l2 = self.len();

        let delta = l2 - l1;
        self.line_nos.reserve(delta);
        for _ in 0..delta {
            self.line_nos.push(line_no);
        }
    }

    pub fn write_u8(&mut self, byte: u8, line_no: usize) {
        self.code.push(byte);
        self.line_nos.push(line_no);
    }

    pub fn patch_u8(&mut self, offset: usize, byte: u8) -> Result<(), OpcodeError> {
        match self.code.get_mut(offset) {
            Some(slot) => {
                *slot = byte;
                Ok(())
            }
            None => Err(OpcodeError::OutOfBounds),
        }
    }

    pub fn patch_u16(&mut self, offset: usize, short: u16) -> Result<(), OpcodeError> {
        for (i, byte) in short.to_be_bytes().iter().copied().enumerate() {
            self.patch_u8(offset + i, byte)?;
        }
        Ok(())
    }

    pub fn try_read_op(&self, offset: usize) -> Result<(RichOpcode, usize), OpcodeError> {
        RichOpcode::decode(&self.code, offset)
    }

    pub fn read_u8(&self, idx: usize) -> u8 {
        self.code[idx]
    }

    pub fn read_u16(&self, idx: usize) -> u16 {
        let bytes = [self.code[idx], self.code[idx + 1]];
        u16::from_be_bytes(bytes)
    }

    pub fn add_constant(&mut self, constant: ChunkConstant) -> ConstantIdx {
        self.constants.push(constant);
        let idx = self.constants.len() - 1;
        idx.try_into().expect("Too many constants")
    }

    pub fn lookup_constant(&self, idx: ConstantIdx) -> ChunkConstant {
        self.constants[idx as usize].clone()
    }

    pub fn get_line_no(&self, idx: usize) -> usize {
        self.line_nos[idx]
    }

    // TODO: should this take a formatter???
    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);

        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_at(offset);
        }
    }

    pub fn disassemble_at(&self, offset: usize) -> usize {
        // Print byte offset and line number
        print!("{:04} ", offset);
        if offset == 0 || self.line_nos[offset] != self.line_nos[offset - 1] {
            print!("{:4} ", self.line_nos[offset]);
        } else {
            print!("   | ");
        }

        let val = self.code[offset];
        let instruction = match OpCode::try_from(val) {
            Ok(instruction) => instruction,
            Err(_) => {
                println!("Unknown opcode {}", val);
                return offset + 1;
            }
        };

        macro_rules! print_two {
            ($op:expr, $first:expr) => {
                println!("{:20} {:4}", $op, $first);
            };
        }

        macro_rules! print_three {
            ($op:expr, $first:expr, $second:expr) => {
                println!("{:20} {:04?} {:?}", $op, $first, $second);
            };
        }

        macro_rules! print_with_constant {
            ($op:expr) => {{
                let idx = self.read_u8(offset + 1);
                let constant = self.lookup_constant(idx);
                print_three!($op, idx, constant);
            };};
        }

        match instruction {
            // Constants
            OpCode::Constant => print_with_constant!("OP_CONSTANT"),
            OpCode::True => println!("OP_TRUE"),
            OpCode::False => println!("OP_FALSE"),
            OpCode::Nil => println!("OP_NIL"),
            // Arithmetic
            OpCode::Add => println!("OP_ADD"),
            OpCode::Subtract => println!("OP_SUBTRACT"),
            OpCode::Multiply => println!("OP_MULTIPLY"),
            OpCode::Divide => println!("OP_DIVIDE"),
            OpCode::Negate => println!("OP_NEGATE"),
            // Logic
            OpCode::Not => println!("OP_NOT"),
            // Comparison
            OpCode::Equal => println!("OP_EQUAL"),
            OpCode::GreaterThan => println!("OP_GREATER"),
            OpCode::LessThan => println!("OP_LESS"),
            // Variables
            OpCode::DefineGlobal => print_with_constant!("OP_DEFINE_GLOBAL"),
            OpCode::GetGlobal => print_with_constant!("OP_GET_GLOBAL"),
            OpCode::SetGlobal => print_with_constant!("OP_SET_GLOBAL"),
            OpCode::GetLocal => print_two!("OP_GET_LOCAL", self.read_u8(offset + 1)),
            OpCode::SetLocal => print_two!("OP_SET_LOCAL", self.read_u8(offset + 1)),
            // Jumps
            OpCode::Jump => print_two!("OP_JUMP", self.read_u16(offset + 1)),
            OpCode::JumpIfFalse => print_two!("OP_JUMP_IF_FALSE", self.read_u16(offset + 1)),
            OpCode::Loop => print_two!("OP_LOOP", self.read_u16(offset + 1)),
            // Closures and Upvalues
            OpCode::MakeClosure => {
                let idx = self.read_u8(offset + 1);
                let constant = self.lookup_constant(idx);
                let upvalue_count = match constant {
                    ChunkConstant::FnTemplate { upvalue_count, .. } => upvalue_count,
                    _ => todo!(),
                };

                print_three!("OP_MAKE_CLOSURE", idx, constant);
                for i in 0..upvalue_count {
                    let kind = self.read_u8(offset + 2 + 2 * i);
                    let idx = self.read_u8(offset + 2 + 2 * i + 1);
                    print!("{:37}: ", i);
                    match kind {
                        1 => println!("local   #{}", idx),
                        0 => println!("upvalue #{}", idx),
                        _ => println!("???     #{}", idx),
                    };
                }
            }
            OpCode::GetUpvalue => print_two!("OP_GET_UPVALUE", self.read_u8(offset + 1)),
            OpCode::SetUpvalue => print_two!("OP_SET_UPVALUE", self.read_u8(offset + 1)),
            OpCode::CloseUpvalue => println!("OP_CLOSE_UPVALUE"),
            // Classes
            OpCode::MakeClass => print_with_constant!("OP_MAKE_CLASS"),
            OpCode::GetProperty => print_with_constant!("OP_GET_PROPERTY"),
            OpCode::SetProperty => print_with_constant!("OP_SET_PROPERTY"),
            OpCode::MakeMethod => print_with_constant!("OP_MAKE_METHOD"),
            OpCode::Invoke => {
                let idx = self.read_u8(offset + 1);
                let method_name = self.lookup_constant(idx);
                let num_args = self.read_u8(offset + 2);

                print_three!("OP_INVOKE", method_name, num_args);
            }
            OpCode::Inherit => println!("OP_INHERIT"),
            OpCode::GetSuper => print_with_constant!("OP_GET_SUPER"),
            OpCode::SuperInvoke => {
                let idx = self.read_u8(offset + 1);
                let method_name = self.lookup_constant(idx);
                let num_args = self.read_u8(offset + 2);

                print_three!("OP_SUPER_INVOKE", method_name, num_args);
            }
            // Other
            OpCode::Call => print_two!("OP_CALL", self.read_u8(offset + 1)),
            OpCode::Print => println!("OP_PRINT"),
            OpCode::Pop => println!("OP_POP"),
            OpCode::Return => println!("OP_RETURN"),
        };

        // Exactly one of these should be set
        let (_, new_offset) = RichOpcode::decode(&self.code, offset).unwrap();
        new_offset
    }
}
