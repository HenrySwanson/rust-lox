use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::rc::Rc;

use super::opcode::OpCode;
use super::string_interning::InternedString;

pub type ConstantIdx = u8; // allow only 256 constants / chunk

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

    pub fn write_op(&mut self, op: OpCode, line_no: usize) {
        self.write_u8(op.into(), line_no)
    }

    pub fn try_read_op(&self, idx: usize) -> Result<OpCode, u8> {
        let byte = self.code[idx];

        // Convert byte to opcode
        OpCode::try_from(byte).map_err(|e| e.number)
    }

    pub fn write_u8(&mut self, byte: u8, line_no: usize) {
        self.code.push(byte);
        self.line_nos.push(line_no);
    }

    pub fn read_u8(&self, idx: usize) -> u8 {
        self.code[idx]
    }

    pub fn write_u16(&mut self, short: u16, line_no: usize) {
        // remember to write twice to line #s
        let bytes = short.to_be_bytes();
        self.write_u8(bytes[0], line_no);
        self.write_u8(bytes[1], line_no);
    }

    pub fn read_u16(&self, idx: usize) -> u16 {
        let bytes = [self.code[idx], self.code[idx + 1]];
        u16::from_be_bytes(bytes)
    }

    pub fn write_op_with_u8(&mut self, op: OpCode, byte: u8, line_no: usize) {
        self.write_op(op, line_no);
        self.write_u8(byte, line_no);
    }

    pub fn write_op_with_u16(&mut self, op: OpCode, short: u16, line_no: usize) {
        self.write_op(op, line_no);
        self.write_u16(short, line_no);
    }

    // instruction-operations that aren't quite "append data to end of code"

    pub fn emit_jump(&mut self, op: OpCode, line_no: usize) -> usize {
        self.write_op_with_u16(op, 0xffff, line_no);
        self.code.len() - 2 // -2 so that we return the location of the argument
    }

    pub fn patch_jump(&mut self, idx: usize) {
        // distance from (instruction after JMP) to here
        let jump_distance = self.code.len() - (idx + 2);

        // TODO: better error handling here...
        let jump_bytes = u16::try_from(jump_distance)
            .expect("Jump distance too large!")
            .to_be_bytes();

        self.code[idx] = jump_bytes[0];
        self.code[idx + 1] = jump_bytes[1];
    }

    pub fn emit_loop(&mut self, loop_start: usize, line_no: usize) {
        // distance from (instruction after JMP) to loop_start
        let jump_distance = (self.code.len() + 3) - loop_start;
        let jump_distance = u16::try_from(jump_distance).expect("Loop distance too large!");

        self.write_op_with_u16(OpCode::Loop, jump_distance, line_no);
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
                println!("{:20} {:04} {:?}", $op, $first, $second);
            };
        }

        let mut variable_args_size: Option<usize> = None;

        match instruction {
            // Constants
            OpCode::Constant => {
                let idx = self.read_u8(offset + 1);
                let constant = self.lookup_constant(idx);
                print_three!("OP_CONSTANT", idx, constant);
            }
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
            OpCode::DefineGlobal => {
                let idx = self.read_u8(offset + 1);
                let constant = self.lookup_constant(idx);
                print_three!("OP_DEFINE_GLOBAL", idx, constant);
            }
            OpCode::GetGlobal => {
                let idx = self.read_u8(offset + 1);
                let constant = self.lookup_constant(idx);
                print_three!("OP_GET_GLOBAL", idx, constant);
            }
            OpCode::SetGlobal => {
                let idx = self.read_u8(offset + 1);
                let constant = self.lookup_constant(idx);
                print_three!("OP_SET_GLOBAL", idx, constant);
            }
            OpCode::GetLocal => {
                let idx = self.read_u8(offset + 1);
                print_two!("OP_GET_LOCAL", idx);
            }
            OpCode::SetLocal => {
                let idx = self.read_u8(offset + 1);
                print_two!("OP_SET_LOCAL", idx);
            }
            // Jumps
            OpCode::Jump => {
                let distance = self.read_u16(offset + 1);
                print_two!("OP_JUMP", distance);
            }
            OpCode::JumpIfFalse => {
                let distance = self.read_u16(offset + 1);
                print_two!("OP_JUMP_IF_FALSE", distance);
            }
            OpCode::Loop => {
                let distance = self.read_u16(offset + 1);
                print_two!("OP_LOOP", distance);
            }
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
                variable_args_size = Some(1 + 2 * upvalue_count);
            }
            OpCode::GetUpvalue => {
                let idx = self.read_u8(offset + 1);
                print_two!("OP_GET_UPVALUE", idx);
            }
            OpCode::SetUpvalue => {
                let idx = self.read_u8(offset + 1);
                print_two!("OP_SET_UPVALUE", idx);
            }
            OpCode::CloseUpvalue => println!("OP_CLOSE_UPVALUE"),
            // Classes
            OpCode::MakeClass => {
                let idx = self.read_u8(offset + 1);
                let constant = self.lookup_constant(idx);
                print_three!("OP_MAKE_CLASS", idx, constant);
            }
            OpCode::GetProperty => {
                let idx = self.read_u8(offset + 1);
                let constant = self.lookup_constant(idx);
                print_three!("OP_GET_PROPERTY", idx, constant);
            }
            OpCode::SetProperty => {
                let idx = self.read_u8(offset + 1);
                let constant = self.lookup_constant(idx);
                print_three!("OP_SET_PROPERTY", idx, constant);
            }
            // Other
            OpCode::Call => print_two!("OP_CALL", self.read_u8(offset + 1)),
            OpCode::Print => println!("OP_PRINT"),
            OpCode::Pop => println!("OP_POP"),
            OpCode::Return => println!("OP_RETURN"),
        };

        // Exactly one of these should be set
        let arg_bytes = variable_args_size.xor(instruction.arg_size_in_bytes());

        // +1 for the instruction itself
        offset + 1 + arg_bytes.expect("Ill-defined argument size")
    }
}
