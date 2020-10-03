use std::convert::{TryFrom, TryInto};

use super::gc::GcStrong;
use super::opcode::OpCode;
use super::value::{HeapObject, Value};
use super::vm::VM;

pub type ConstantIdx = u8; // allow only 256 constants / chunk

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    line_nos: Vec<usize>,

    constant_roots: Vec<GcStrong<HeapObject>>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: vec![],
            constants: vec![],
            line_nos: vec![],
            constant_roots: vec![],
        }
    }

    // TODO: instruction-and-byte write?

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

    pub fn write_instruction_with_u16(&mut self, op: OpCode, short: u16, line_no: usize) {
        self.write_op(op, line_no);
        self.write_u16(short, line_no);
    }

    pub fn add_constant(&mut self, constant: Value) -> ConstantIdx {
        self.constants.push(constant);
        let idx = self.constants.len() - 1;
        idx.try_into().expect("Too many constants")
    }

    pub fn add_heap_constant(&mut self, obj: HeapObject, vm: &mut VM) -> ConstantIdx {
        // Hand over ownership to the heap
        let gc_handle = vm.push_to_heap(obj);

        // Add it to the constant table like any other
        let value = Value::Obj(gc_handle.downgrade());
        let idx = self.add_constant(value);

        // Stash the strong handle in the chunk, so it doesn't get garbage-collected
        self.constant_roots.push(gc_handle);

        return idx;
    }

    pub fn read_constant(&self, idx: ConstantIdx) -> Value {
        self.constants[idx as usize].clone()
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

        match instruction {
            // Constants
            OpCode::Constant => {
                let idx = self.code[offset + 1];
                let constant = self.read_constant(idx);
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
            // Other
            OpCode::Print => println!("OP_PRINT"),
            OpCode::Pop => println!("OP_POP"),
            OpCode::Return => println!("OP_RETURN"),
            OpCode::DefineGlobal => {
                let idx = self.code[offset + 1];
                let constant = self.read_constant(idx);
                print_three!("OP_DEFINE_GLOBAL", idx, constant);
            }
            OpCode::GetGlobal => {
                let idx = self.code[offset + 1];
                let constant = self.read_constant(idx);
                print_three!("OP_GET_GLOBAL", idx, constant);
            }
            OpCode::SetGlobal => {
                let idx = self.code[offset + 1];
                let constant = self.read_constant(idx);
                print_three!("OP_SET_GLOBAL", idx, constant);
            }
            OpCode::GetLocal => {
                let idx = self.code[offset + 1];
                print_two!("OP_GET_LOCAL", idx);
            }
            OpCode::SetLocal => {
                let idx = self.code[offset + 1];
                print_two!("OP_SET_LOCAL", idx);
            }
        };
        return offset + instruction.num_operands() + 1;
    }
}
