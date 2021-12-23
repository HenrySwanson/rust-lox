use std::convert::TryInto;
use std::fmt;
use std::rc::Rc;

use super::opcode::{ConstantIdx, OpcodeError, RichOpcode, UpvalueAddr};
use super::string_interning::InternedString;
use super::errs::{CompilerError, CompilerResult};

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

    pub fn write_upvalue(&mut self, addr: UpvalueAddr, line_no: usize) {
        UpvalueAddr::encode(&mut self.code, addr);
        self.line_nos.push(line_no);
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

    pub fn try_read_upvalue(&self, offset: usize) -> Result<UpvalueAddr, OpcodeError> {
        UpvalueAddr::decode(&self.code, offset)
    }

    pub fn add_constant(&mut self, constant: ChunkConstant) -> CompilerResult<ConstantIdx> {
        self.constants.push(constant);
        let idx = self.constants.len() - 1;
        idx.try_into().map_err(|_| CompilerError::TooManyConstants)
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

        let (op, new_offset) = match RichOpcode::decode(&self.code, offset) {
            Ok(tuple) => tuple,
            Err(_) => {
                println!("Unparseable: {}", self.code[offset]);
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
            ($op:expr, $idx:expr) => {{
                let constant = self.lookup_constant($idx);
                print_three!($op, $idx, constant);
            };};
        }

        match op {
            // Constants
            RichOpcode::Constant(idx) => print_with_constant!("OP_CONSTANT", idx),
            RichOpcode::True => println!("OP_TRUE"),
            RichOpcode::False => println!("OP_FALSE"),
            RichOpcode::Nil => println!("OP_NIL"),
            // Arithmetic
            RichOpcode::Add => println!("OP_ADD"),
            RichOpcode::Subtract => println!("OP_SUBTRACT"),
            RichOpcode::Multiply => println!("OP_MULTIPLY"),
            RichOpcode::Divide => println!("OP_DIVIDE"),
            RichOpcode::Negate => println!("OP_NEGATE"),
            // Logic
            RichOpcode::Not => println!("OP_NOT"),
            // Comparison
            RichOpcode::Equal => println!("OP_EQUAL"),
            RichOpcode::GreaterThan => println!("OP_GREATER"),
            RichOpcode::LessThan => println!("OP_LESS"),
            // Variables
            RichOpcode::DefineGlobal(idx) => print_with_constant!("OP_DEFINE_GLOBAL", idx),
            RichOpcode::GetGlobal(idx) => print_with_constant!("OP_GET_GLOBAL", idx),
            RichOpcode::SetGlobal(idx) => print_with_constant!("OP_SET_GLOBAL", idx),
            RichOpcode::GetLocal(idx) => print_two!("OP_GET_LOCAL", idx),
            RichOpcode::SetLocal(idx) => print_two!("OP_SET_LOCAL", idx),
            // Jumps
            RichOpcode::Jump(distance) => print_two!("OP_JUMP", distance),
            RichOpcode::JumpIfFalse(distance) => print_two!("OP_JUMP_IF_FALSE", distance),
            RichOpcode::Loop(distance) => print_two!("OP_LOOP", distance),
            // Closures and Upvalues
            RichOpcode::MakeClosure(idx) => {
                let constant = self.lookup_constant(idx);
                let upvalue_count = match constant {
                    ChunkConstant::FnTemplate { upvalue_count, .. } => upvalue_count,
                    _ => todo!(),
                };

                print_three!("OP_MAKE_CLOSURE", idx, constant);
                for i in 0..upvalue_count {
                    print!("{:37}: ", i);
                    match UpvalueAddr::decode(&self.code, offset + 2 + 2 * i) {
                        Ok(addr) => match addr {
                            UpvalueAddr::Immediate(idx) => println!("local   #{}", idx),
                            UpvalueAddr::Recursive(idx) => println!("upvalue #{}", idx),
                        },
                        Err(_) => println!("???     #{}", idx),
                    }
                }
            }
            RichOpcode::GetUpvalue(idx) => print_two!("OP_GET_UPVALUE", idx),
            RichOpcode::SetUpvalue(idx) => print_two!("OP_SET_UPVALUE", idx),
            RichOpcode::CloseUpvalue => println!("OP_CLOSE_UPVALUE"),
            // Classes
            RichOpcode::MakeClass(idx) => print_with_constant!("OP_MAKE_CLASS", idx),
            RichOpcode::GetProperty(idx) => print_with_constant!("OP_GET_PROPERTY", idx),
            RichOpcode::SetProperty(idx) => print_with_constant!("OP_SET_PROPERTY", idx),
            RichOpcode::MakeMethod(idx) => print_with_constant!("OP_MAKE_METHOD", idx),
            RichOpcode::Invoke(idx, argc) => {
                let method_name = self.lookup_constant(idx);
                print_three!("OP_INVOKE", method_name, argc);
            }
            RichOpcode::Inherit => println!("OP_INHERIT"),
            RichOpcode::GetSuper(idx) => print_with_constant!("OP_GET_SUPER", idx),
            RichOpcode::SuperInvoke(idx, argc) => {
                let method_name = self.lookup_constant(idx);
                print_three!("OP_SUPER_INVOKE", method_name, argc);
            }
            // Other
            RichOpcode::Call(argc) => print_two!("OP_CALL", argc),
            RichOpcode::Print => println!("OP_PRINT"),
            RichOpcode::Pop => println!("OP_POP"),
            RichOpcode::Return => println!("OP_RETURN"),
        };

        new_offset
    }
}
