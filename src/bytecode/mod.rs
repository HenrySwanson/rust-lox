use num_enum::{IntoPrimitive, TryFromPrimitive};
use std::convert::{TryFrom, TryInto};

#[derive(Clone, Copy, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Return = 0,
    Constant = 1,
}

pub type Value = u32; // TODO should be f64, just like Token::Number et al
pub type ConstantIdx = u8; // allow only 256 constants / chunk

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    line_nos: Vec<usize>,
}

impl OpCode {
    fn num_operands(&self) -> usize {
        match self {
            OpCode::Return => 0,
            OpCode::Constant => 1,
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

    fn read_constant(&self, idx: ConstantIdx) -> Value {
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

    fn disassemble_at(&self, offset: usize) -> usize {
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
        };
        return offset + instruction.num_operands() + 1;
    }
}
