use super::opcode::OpCode;
use std::convert::{TryFrom, TryInto};

pub type Value = i64; // TODO should be f64, just like Token::Number et al
pub type ConstantIdx = u8; // allow only 256 constants / chunk

pub struct Chunk {
    pub code: Vec<u8>,
    constants: Vec<Value>,
    line_nos: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: vec![],
            constants: vec![],
            line_nos: vec![],
        }
    }

    pub fn write_byte(&mut self, byte: u8, line_no: usize) {
        self.code.push(byte);
        self.line_nos.push(line_no);
    }

    pub fn write_instruction(&mut self, instruction: OpCode, line_no: usize) {
        self.write_byte(instruction.into(), line_no)
    }

    pub fn add_constant(&mut self, constant: Value) -> ConstantIdx {
        self.constants.push(constant);
        let idx = self.constants.len() - 1;
        idx.try_into().expect("Too many constants")
    }

    pub fn read_constant(&self, idx: ConstantIdx) -> Value {
        self.constants[idx as usize]
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
            print!("{:04} ", self.line_nos[offset]);
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

        match instruction {
            OpCode::Return => println!("OP_RETURN"),
            OpCode::Constant => {
                let idx = self.code[offset + 1];
                let constant = self.read_constant(idx);
                println!("OP_CONSTANT {:4} {}", idx, constant);
            }
            OpCode::Add => println!("OP_ADD"),
            OpCode::Subtract => println!("OP_SUBTRACT"),
            OpCode::Multiply => println!("OP_MULTIPLY"),
            OpCode::Divide => println!("OP_DIVIDE"),
            OpCode::Negate => println!("OP_NEGATE"),
        };
        return offset + instruction.num_operands() + 1;
    }
}
