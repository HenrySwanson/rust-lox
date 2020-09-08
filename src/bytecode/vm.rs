use super::chunk::{Chunk, Value};
use super::opcode::OpCode;

use std::convert::TryFrom;

// TODO can this be converted into a build option?
const DEBUG_TRACE_EXECUTION: bool = false;

pub struct VM {
    stack: Vec<Value>,
}

#[derive(Debug)]
pub enum Error {
    InvalidOpcode(u8),
    DivideByZero,
}

pub type VmResult<T> = Result<T, Error>;

impl VM {
    pub fn new() -> Self {
        VM { stack: vec![] }
    }

    pub fn interpret(&mut self, chunk: &Chunk) -> VmResult<()> {
        self.run(chunk, 0)
    }

    fn run(&mut self, chunk: &Chunk, mut ip: usize) -> VmResult<()> {
        loop {
            if DEBUG_TRACE_EXECUTION {
                println!("          {:?}", self.stack);
                chunk.disassemble_at(ip);
            }

            // TODO get?
            let byte = chunk.code[ip];

            // Convert byte to opcode
            let op = match OpCode::try_from(byte) {
                Ok(op) => op,
                Err(_) => return Err(Error::InvalidOpcode(byte)),
            };

            match op {
                OpCode::Return => {
                    let value = self.pop();
                    println!("Return: {}", value);
                    return Ok(());
                }
                OpCode::Constant => {
                    // TODO just print for now
                    let constant = chunk.read_constant(chunk.code[ip + 1]);
                    self.push(constant);
                }
                OpCode::Add => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(lhs + rhs);
                }
                OpCode::Subtract => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(lhs - rhs);
                }
                OpCode::Multiply => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(lhs * rhs);
                }
                OpCode::Divide => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    if rhs != 0 {
                        self.push(lhs / rhs);
                    } else {
                        return Err(Error::DivideByZero);
                    }
                }
                OpCode::Negate => {
                    let val = -self.pop();
                    self.push(val);
                }
            }

            ip += op.num_operands() + 1;
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Popped empty stack!")
    }
}
