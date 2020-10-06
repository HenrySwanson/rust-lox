use std::collections::HashMap;

use super::chunk::{Chunk, ConstantIdx};
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

    pub fn interpret(&mut self, main_fn: GcStrong<HeapObject>) -> RuntimeResult<()> {
        // Reset computational state
        self.call_stack.clear();
        self.stack.clear();

        // Make main() a real value and push it onto the stack
        self.push(Value::Obj(main_fn.downgrade()));

        // Make a frame for the invocation of main()
        self.push_new_frame(0, main_fn);

        // Print stack trace on failure
        let result = self.run();
        if result.is_err() {
            for frame in self.call_stack.iter().rev() {
                let (fn_name, chunk) = match self.heap.get(&frame.callable) {
                    HeapObject::LoxFunction(fn_data) => (&fn_data.name, &fn_data.chunk),
                    _ => panic!("Call stack obj not callable"),
                };
                let line_no = chunk.get_line_no(frame.ip - 1);
                println!("[line {}] in {}", line_no, fn_name);
            }
        }

        result
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
                println!();
            }

            let op = match self.try_read_op() {
                Ok(op) => op,
                Err(byte) => return Err(RuntimeError::InvalidOpcode(byte)),
            };

            match op {
                // Constants
                OpCode::Constant => {
                    let idx = self.read_u8();
                    let constant = self.chunk().lookup_constant(idx);
                    self.push(constant);
                }
                OpCode::True => self.push(Value::Boolean(true)),
                OpCode::False => self.push(Value::Boolean(false)),
                OpCode::Nil => self.push(Value::Nil),
                // Arithmetic
                OpCode::Add => self.do_add()?,
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
                    let idx = self.read_u8();
                    let name = self.lookup_string(idx);
                    let value = self.pop()?;
                    self.globals.insert(name, value);
                }
                OpCode::GetGlobal => {
                    let idx = self.read_u8();
                    let name = self.lookup_string(idx);
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
                    let idx = self.read_u8();
                    let name = self.lookup_string(idx);
                    if !self.globals.contains_key(&name) {
                        let name: String = (*name).to_owned();
                        return Err(RuntimeError::UndefinedGlobal(name));
                    }
                    // don't pop; assignment may be inside other expressions
                    let value = self.peek(0)?;
                    self.globals.insert(name, value);
                }
                OpCode::GetLocal => {
                    let idx = self.read_u8() as usize;
                    let value = self.stack[base_ptr + idx].clone();
                    self.push(value);
                }
                OpCode::SetLocal => {
                    let value = self.pop()?;
                    let idx = self.read_u8() as usize;
                    self.stack[base_ptr + idx] = value;
                }
                // Jumps
                OpCode::Jump => {
                    let jump_by = usize::from(self.read_u16());
                    self.frame_mut().ip += jump_by;
                }
                OpCode::JumpIfFalse => {
                    let jump_by = usize::from(self.read_u16());
                    if !self.peek(0)?.is_truthy() {
                        self.frame_mut().ip += jump_by;
                    }
                }
                OpCode::Loop => {
                    let jump_by = usize::from(self.read_u16());
                    self.frame_mut().ip -= jump_by;
                }
                // Other
                OpCode::Call => {
                    let arg_count: usize = self.read_u8().into();

                    // Fetch the object in the appropriate stack slot
                    let callable_ptr = match &self.peek(arg_count)? {
                        Value::Obj(gc_ptr) => gc_ptr.clone(),
                        _ => return Err(RuntimeError::NotACallable), // only callable objects are on the heap
                    };
                    let fn_data = match self.heap.get(&callable_ptr) {
                        HeapObject::LoxFunction(fn_data) => fn_data,
                    };

                    // Check that the arity matches
                    if fn_data.arity != arg_count {
                        return Err(RuntimeError::WrongArity);
                    }

                    // Alright, we're ready to call the function.
                    self.push_new_frame(arg_count, self.heap.upgrade(callable_ptr));
                }
                OpCode::Return => {
                    // Rescue the return value off the stack
                    let result = self.pop_frame()?;

                    // Now check if there are any frames left
                    if self.call_stack.is_empty() {
                        return Ok(()); // exit loop
                    } else {
                        self.push(result);
                    }
                }
                OpCode::Print => {
                    let value = self.pop()?;
                    println!("[out] {:?}", value);
                }
                OpCode::Pop => {
                    self.pop()?;
                }
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

    fn read_u8(&mut self) -> u8 {
        let ip = self.frame_mut().ip;
        let byte = self.chunk().read_u8(ip);
        self.frame_mut().ip += 1;
        byte
    }

    fn read_u16(&mut self) -> u16 {
        let ip = self.frame_mut().ip;
        let short = self.chunk().read_u16(ip);
        self.frame_mut().ip += 2;
        short
    }

    fn try_read_op(&mut self) -> Result<OpCode, u8> {
        let ip = self.frame_mut().ip;
        let result = self.chunk().try_read_op(ip);
        self.frame_mut().ip += 1;
        result
    }

    fn push_new_frame(&mut self, arg_count: usize, callable: GcStrong<HeapObject>) {
        let new_frame = CallFrame {
            ip: 0,
            base_ptr: self.stack.len() - (arg_count + 1),
            callable,
        };
        self.call_stack.push(new_frame);
    }

    fn pop_frame(&mut self) -> RuntimeResult<Value> {
        // Rescue the return value off the stack
        let result = self.pop()?;
        let frame = self.call_stack.pop().expect("Call stack empty!");

        // Clear everything above and including the most recent frame ptr
        self.stack.truncate(frame.base_ptr);

        Ok(result)
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

    // -- other helpers --

    fn do_add(&mut self) -> RuntimeResult<()> {
        let lhs = self.peek(1)?;
        let rhs = self.peek(0)?;

        let value = match (lhs, rhs) {
            (Value::Number(n), Value::Number(m)) => Value::Number(n + m),
            (Value::String(s), Value::String(t)) => {
                // Gotta use as_ref to get a &str from the Rc<str>
                let new_string = s.as_ref().to_owned() + &t;
                let interned = self.string_table.get_interned(new_string);
                Value::String(interned)
            }
            _ => return Err(RuntimeError::IncorrectOperandType),
        };

        self.pop()?;
        self.pop()?;
        self.push(value);

        Ok(())
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

    fn lookup_string(&self, idx: ConstantIdx) -> InternedString {
        let chunk = &self.chunk();
        match chunk.lookup_constant(idx) {
            Value::String(s) => s,
            _ => panic!("Global table contains non-string"),
        }
    }
}
