use std::collections::HashMap;

use super::chunk::Chunk;
use super::errs::{RuntimeError, RuntimeResult};
use super::gc::{GcHeap, GcStrong};
use super::opcode::OpCode;
use super::string_interning::{InternedString, StringInterner};
use super::value::{HeapObject, Value};

// TODO can this be converted into a build option?
const DEBUG_TRACE_EXECUTION: bool = false;

struct CallFrame {
    ip: usize,
    base_ptr: usize,
    // could be a LoxFunctionData if we had a heterogenous heap...
    callable: GcStrong<HeapObject>,
}

pub struct VM {
    call_stack: Vec<CallFrame>,
    stack: Vec<Value>,
    heap: GcHeap<HeapObject>,
    string_table: StringInterner,
    globals: HashMap<InternedString, Value>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            call_stack: vec![],
            stack: vec![],
            heap: GcHeap::new(),
            string_table: StringInterner::new(),
            globals: HashMap::new(),
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> RuntimeResult<Value> {
        self.stack.pop().ok_or(RuntimeError::StackEmpty)
    }

    fn peek(&self, depth: usize) -> RuntimeResult<Value> {
        let stack_size = self.stack.len();
        self.stack
            .get(stack_size - 1 - depth)
            .cloned()
            .ok_or(RuntimeError::InvalidStackIndex)
    }

    pub fn intern_string(&mut self, s: &str) -> InternedString {
        self.string_table.get_interned(s)
    }

    pub fn insert_into_heap(&mut self, obj: HeapObject) -> GcStrong<HeapObject> {
        self.heap.insert(obj)
    }

    pub fn interpret(&mut self, main_fn: GcStrong<HeapObject>) -> RuntimeResult<()> {
        // TODO reset stack and call_stack

        // Make main() a real value and push it onto the stack
        self.push(Value::Obj(main_fn.downgrade()));

        // Make a frame for the invocation of main()
        // TODO extract?
        self.call_stack.push(CallFrame {
            ip: 0,
            base_ptr: 0,
            callable: main_fn,
        });

        // TODO print stack trace on failure
        self.run()
    }

    fn run(&mut self) -> RuntimeResult<()> {
        loop {
            let ip = self.frame().ip;
            let base_ptr = self.frame().base_ptr;

            if DEBUG_TRACE_EXECUTION {
                println!("STACK     {:?}", self.stack);
                println!("frame ptr = {}", self.frame().base_ptr);
                println!("ip = {}", self.frame().ip);
                self.chunk().disassemble_at(ip);
                println!("");
            }

            let mut jump_to: Option<usize> = None;

            let op = match self.chunk().try_read_op(ip) {
                Ok(op) => op,
                Err(byte) => return Err(RuntimeError::InvalidOpcode(byte)),
            };

            match op {
                // Constants
                OpCode::Constant => {
                    let idx = self.chunk().read_u8(ip + 1);
                    let constant = self.chunk().read_constant(idx);
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
                        None => return Err(RuntimeError::IncorrectOperandType),
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
                            return Err(RuntimeError::DivideByZero);
                        }
                    }

                    self.arithmetic_binop(|a, b| a / b)?;
                }
                OpCode::Negate => match self.peek(0)? {
                    Value::Number(n) => {
                        self.pop()?;
                        self.push(Value::Number(-n));
                    }
                    _ => return Err(RuntimeError::IncorrectOperandType),
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
                // Variables
                OpCode::DefineGlobal => {
                    let name = self.read_string(ip + 1);
                    let value = self.pop()?;
                    self.globals.insert(name, value);
                }
                OpCode::GetGlobal => {
                    let name = self.read_string(ip + 1);
                    let value = match self.globals.get(&name) {
                        Some(value) => value.clone(),
                        None => {
                            let name: String = (*name).to_owned();
                            return Err(RuntimeError::UndefinedGlobal(name));
                        }
                    };
                    self.push(value);
                }
                OpCode::SetGlobal => {
                    let name = self.read_string(ip + 1);
                    if !self.globals.contains_key(&name) {
                        let name: String = (*name).to_owned();
                        return Err(RuntimeError::UndefinedGlobal(name));
                    }
                    // don't pop; assignment may be inside other expressions
                    let value = self.peek(0)?;
                    self.globals.insert(name, value);
                }
                OpCode::GetLocal => {
                    let idx = self.chunk().read_u8(ip + 1) as usize;
                    let value = self.stack[base_ptr + idx].clone();
                    self.push(value);
                }
                OpCode::SetLocal => {
                    let value = self.pop()?;
                    let idx = self.chunk().read_u8(ip + 1) as usize;
                    self.stack[base_ptr + idx] = value;
                }
                // Jumps
                OpCode::Jump => {
                    let jump_by = usize::from(self.chunk().read_u16(ip + 1));
                    jump_to = Some(ip + 3 + jump_by); // starts at end of the jump instruction
                }
                OpCode::JumpIfFalse => {
                    if !self.peek(0)?.is_truthy() {
                        let jump_by = usize::from(self.chunk().read_u16(ip + 1));
                        jump_to = Some(ip + 3 + jump_by); // starts at end of the jump instruction
                    }
                }
                OpCode::Loop => {
                    let jump_by = usize::from(self.chunk().read_u16(ip + 1));
                    jump_to = Some(ip + 3 - jump_by); // starts at end of the jump instruction
                }
                // Other
                OpCode::Call => {
                    let arg_count: usize = self.chunk().read_u8(ip + 1).into();

                    // Fetch the object in the appropriate stack slot
                    let frame_start = self.stack.len() - (arg_count + 1);
                    let heap_obj_ptr = match &self.stack[frame_start] {
                        Value::Obj(gc_ptr) => gc_ptr.clone(),
                        _ => return Err(RuntimeError::NotACallable), // only callable objects are on the heap
                    };
                    let fn_data = match self.heap.get(&heap_obj_ptr) {
                        HeapObject::LoxFunction(fn_data) => fn_data,
                    };

                    // Check that the arity matches
                    if fn_data.arity != arg_count {
                        return Err(RuntimeError::WrongArity);
                    }

                    // Alright, we're ready to call the function. But before we do that, let's
                    // set the return address (i.e., frame.ip of the current frame) correctly.
                    self.frame_mut().ip += 2;

                    let frame = CallFrame {
                        ip: 0,
                        base_ptr: frame_start,
                        callable: self.heap.upgrade(heap_obj_ptr), // roots the function obj
                    };
                    self.call_stack.push(frame);

                    // Don't advance ip
                    jump_to = Some(self.frame().ip);
                }
                OpCode::Return => {
                    // Rescue the return value off the stack
                    let result = self.pop()?;
                    let frame = self.call_stack.pop().expect("Call stack empty!");

                    // Clear everything above and including the most recent frame ptr
                    self.stack.truncate(frame.base_ptr);

                    // Now check if there are any frames left
                    if self.call_stack.len() == 0 {
                        return Ok(()); // exit loop
                    } else {
                        self.push(result);
                    }

                    // Don't advance ip
                    jump_to = Some(self.frame().ip);
                }
                OpCode::Print => {
                    let value = self.pop()?;
                    println!("[out] {:?}", value);
                }
                OpCode::Pop => {
                    self.pop()?;
                }
            }

            // How to advance the IP? Check jump_to.
            self.frame_mut().ip = match jump_to {
                Some(n) => n,
                None => ip + op.arg_size_in_bytes() + 1, // +1 for the op itself
            }
        }
    }

    // ---- handy dandy helpers ----

    fn frame(&self) -> &CallFrame {
        match self.call_stack.last() {
            Some(frame) => frame,
            None => panic!("Call stack empty!"),
        }
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        match self.call_stack.last_mut() {
            Some(frame) => frame,
            None => panic!("Call stack empty!"),
        }
    }

    fn chunk(&self) -> &Chunk {
        let gc_ptr = &self.frame().callable;
        match self.heap.get(gc_ptr) {
            HeapObject::LoxFunction(fn_data) => &fn_data.chunk,
            _ => panic!("`callable` is not callable!"),
        }
    }

    fn try_add(&mut self, lhs: Value, rhs: Value) -> Option<Value> {
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

    fn numerical_binop<F>(&mut self, closure: F) -> RuntimeResult<()>
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
            (_, _) => Err(RuntimeError::IncorrectOperandType),
        }
    }

    fn arithmetic_binop<F>(&mut self, closure: F) -> RuntimeResult<()>
    where
        F: Fn(i64, i64) -> i64,
    {
        self.numerical_binop(|a, b| Value::Number(closure(a, b)))
    }

    fn comparison_binop<F>(&mut self, closure: F) -> RuntimeResult<()>
    where
        F: Fn(i64, i64) -> bool,
    {
        self.numerical_binop(|a, b| Value::Boolean(closure(a, b)))
    }

    fn read_string(&self, idx: usize) -> InternedString {
        let chunk = &self.chunk();
        match chunk.read_constant(chunk.read_u8(idx)) {
            Value::String(s) => s,
            _ => panic!("Global table contains non-string"),
        }
    }
}
