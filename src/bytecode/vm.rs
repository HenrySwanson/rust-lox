use std::convert::TryFrom;

use super::chunk::Chunk;
use super::errs::{Error, VmResult};
use super::gc::{GcHeap, GcStrong};
use super::opcode::OpCode;
use super::string_interning::{InternedString, StringInterner};
use super::value::{HeapObject, Value};

// TODO can this be converted into a build option?
const DEBUG_TRACE_EXECUTION: bool = false;

pub struct VM {
    stack: Vec<Value>,
    heap: GcHeap<HeapObject>,
    string_table: StringInterner,
}

impl VM {
    pub fn new() -> Self {
        VM {
            stack: vec![],
            heap: GcHeap::new(),
            string_table: StringInterner::new(),
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> VmResult<Value> {
        self.stack.pop().ok_or(Error::StackEmpty)
    }

    fn peek(&self, depth: usize) -> VmResult<Value> {
        let stack_size = self.stack.len();
        self.stack
            .get(stack_size - 1 - depth)
            .cloned()
            .ok_or(Error::InvalidStackIndex)
    }

    pub fn intern_string(&mut self, s: &str) -> InternedString {
        self.string_table.get_interned(s)
    }

    pub fn push_to_heap(&mut self, obj: HeapObject) -> GcStrong<HeapObject> {
        self.heap.insert(obj)
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
                    let value = self.pop()?;
                    println!("Return: {:?}", value);
                    return Ok(());
                }
                OpCode::Constant => {
                    let constant = chunk.read_constant(chunk.code[ip + 1]);
                    self.push(constant);
                }
                OpCode::True => self.push(Value::Boolean(true)),
                OpCode::False => self.push(Value::Boolean(false)),
                OpCode::Nil => self.push(Value::Nil),
                // Arithmetic
                OpCode::Add => {
                    let lhs = self.peek(1)?;
                    let rhs = self.peek(0)?;

                    let new_object = match self.try_add(lhs, rhs) {
                        Some(obj) => obj,
                        None => return Err(Error::IncorrectOperandType),
                    };

                    self.pop()?;
                    self.pop()?;
                    self.push(new_object);
                }
                OpCode::Subtract => self.arithmetic_binop(|a, b| a - b)?,
                OpCode::Multiply => self.arithmetic_binop(|a, b| a * b)?,
                OpCode::Divide => {
                    // Check for zero
                    if let Value::Number(n) = self.peek(0)? {
                        if n == 0 {
                            return Err(Error::DivideByZero);
                        }
                    }

                    self.arithmetic_binop(|a, b| a / b)?;
                }
                OpCode::Negate => match self.peek(0)? {
                    Value::Number(n) => {
                        self.pop()?;
                        self.push(Value::Number(-n));
                    }
                    _ => return Err(Error::IncorrectOperandType),
                },
                // Logical
                OpCode::Not => {
                    let value = self.pop()?;
                    self.push(Value::Boolean(!value.is_truthy()));
                }
                // Comparison
                OpCode::Equal => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    self.push(Value::Boolean(lhs == rhs));
                }
                OpCode::GreaterThan => self.comparison_binop(|a, b| a > b)?,
                OpCode::LessThan => self.comparison_binop(|a, b| a < b)?,
            }

            ip += op.num_operands() + 1;
        }
    }

    // ---- handy dandy helpers ----

    pub fn try_add(&mut self, lhs: Value, rhs: Value) -> Option<Value> {
        let value = match (lhs, rhs) {
            (Value::Number(n), Value::Number(m)) => Value::Number(n + m),
            (Value::String(s), Value::String(t)) => {
                // Gotta use as_ref to get a &str from the Rc<str>
                let new_string = s.as_ref().to_owned() + &t;
                let interned = self.string_table.get_interned(new_string);
                Value::String(interned)
            }
            _ => return None,
        };

        Some(value)
    }

    fn numerical_binop<F>(&mut self, closure: F) -> VmResult<()>
    where
        F: Fn(i64, i64) -> Value,
    {
        let lhs = self.peek(1)?;
        let rhs = self.peek(0)?;

        match (lhs, rhs) {
            (Value::Number(a), Value::Number(b)) => {
                let result = closure(a, b);
                self.pop()?;
                self.pop()?;
                self.push(result);
                Ok(())
            }
            (_, _) => Err(Error::IncorrectOperandType),
        }
    }

    fn arithmetic_binop<F>(&mut self, closure: F) -> VmResult<()>
    where
        F: Fn(i64, i64) -> i64,
    {
        self.numerical_binop(|a, b| Value::Number(closure(a, b)))
    }

    fn comparison_binop<F>(&mut self, closure: F) -> VmResult<()>
    where
        F: Fn(i64, i64) -> bool,
    {
        self.numerical_binop(|a, b| Value::Boolean(closure(a, b)))
    }
}
