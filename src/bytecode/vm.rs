use std::collections::HashMap;
use std::rc::Rc;

use super::chunk::{Chunk, ChunkConstant, ConstantIdx};
use super::errs::{RuntimeError, RuntimeResult};
use super::gc::{GcHeap, GcPtr};
use super::native;
use super::opcode::OpCode;
use super::string_interning::{InternedString, StringInterner};
use super::value::{HeapObject, Value};

struct CallFrame {
    ip: usize,
    base_ptr: usize,
    name: InternedString,
    chunk: Rc<Chunk>,
}

pub struct VM {
    call_stack: Vec<CallFrame>,
    stack: Vec<Value>,

    heap: GcHeap<HeapObject>,
    string_table: StringInterner,
    globals: HashMap<InternedString, Value>,
}

impl CallFrame {
    fn read_u8(&mut self) -> u8 {
        let byte = self.chunk.read_u8(self.ip);
        self.ip += 1;
        byte
    }

    fn read_u16(&mut self) -> u16 {
        let short = self.chunk.read_u16(self.ip);
        self.ip += 2;
        short
    }

    fn try_read_op(&mut self) -> Result<OpCode, u8> {
        let result = self.chunk.try_read_op(self.ip);
        self.ip += 1;
        result
    }
}

impl VM {
    pub fn new() -> Self {
        let mut vm = VM {
            call_stack: vec![],
            stack: vec![],
            heap: GcHeap::new(),
            string_table: StringInterner::new(),
            globals: HashMap::new(),
        };

        // Define natives
        for (name, arity, function) in native::get_natives().iter().copied() {
            let name = vm.intern_string(name);
            let native_fn = vm.make_heap_value(HeapObject::NativeFunction {
                name: name.clone(),
                arity,
                function,
            });

            vm.globals.insert(name, native_fn);
        }

        vm
    }

    pub fn borrow_string_table(&mut self) -> &mut StringInterner {
        &mut self.string_table
    }

    pub fn interpret(&mut self, main_chunk: Chunk) -> RuntimeResult<()> {
        // Reset computational state
        self.call_stack.clear();
        self.stack.clear();

        // Make main() a real value and push it onto the stack
        let main_name = self.intern_string("<main>");
        let main_fn = self.make_heap_value(HeapObject::LoxClosure {
            name: main_name,
            arity: 0,
            chunk: Rc::new(main_chunk),
        });

        self.push(main_fn);

        // Call main to start the program
        self.call(self.peek(0)?, 0)?;

        // Print stack trace on failure
        let result = self.run();
        if result.is_err() {
            for frame in self.call_stack.iter().rev() {
                let line_no = frame.chunk.get_line_no(frame.ip - 1);
                println!("[line {}] in {}", line_no, frame.name);
            }
        }

        result
    }

    fn run(&mut self) -> RuntimeResult<()> {
        loop {
            #[cfg(feature = "trace-execution")]
            {
                let frame = self.frame_mut();
                let ip = frame.ip;
                let base_ptr = frame.base_ptr;

                println!("STACK     {:?}", self.stack);
                println!(
                    "IP = {}, BP = {} ({:?})",
                    ip, base_ptr, self.stack[base_ptr]
                );
                self.frame_mut().chunk.disassemble_at(ip);
                println!();
            }

            let op = match self.frame_mut().try_read_op() {
                Ok(op) => op,
                Err(byte) => return Err(RuntimeError::InvalidOpcode(byte)),
            };

            match op {
                // Constants
                OpCode::Constant => {
                    let idx = self.frame_mut().read_u8();
                    let value = match self.frame().chunk.lookup_constant(idx) {
                        ChunkConstant::Number(n) => Value::Number(n.into()),
                        ChunkConstant::String(s) => Value::String(s),
                        c => return Err(RuntimeError::UntranslatableConstant(c)),
                    };

                    self.push(value);
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
                    let idx = self.frame_mut().read_u8();
                    let name = self.lookup_string(idx);
                    let value = self.pop()?;
                    self.globals.insert(name, value);
                }
                OpCode::GetGlobal => {
                    let idx = self.frame_mut().read_u8();
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
                    let idx = self.frame_mut().read_u8();
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
                    let base_ptr = self.frame().base_ptr;
                    let idx = self.frame_mut().read_u8() as usize;
                    let value = self.stack[base_ptr + idx].clone();
                    self.push(value);
                }
                OpCode::SetLocal => {
                    let base_ptr = self.frame().base_ptr;
                    let value = self.peek(0)?;
                    let idx = self.frame_mut().read_u8() as usize;
                    self.stack[base_ptr + idx] = value;
                }
                // Jumps
                OpCode::Jump => {
                    let jump_by = usize::from(self.frame_mut().read_u16());
                    self.frame_mut().ip += jump_by;
                }
                OpCode::JumpIfFalse => {
                    let jump_by = usize::from(self.frame_mut().read_u16());
                    if !self.peek(0)?.is_truthy() {
                        self.frame_mut().ip += jump_by;
                    }
                }
                OpCode::Loop => {
                    let jump_by = usize::from(self.frame_mut().read_u16());
                    self.frame_mut().ip -= jump_by;
                }
                // Closures and Upvalues
                OpCode::MakeClosure => {
                    // Load a constant; we can only proceed if it's a FnTemplate
                    let idx = self.frame_mut().read_u8();
                    let closure_obj = match self.frame().chunk.lookup_constant(idx) {
                        ChunkConstant::FnTemplate {
                            name,
                            arity,
                            chunk,
                            upvalue_count,
                        } => {
                            // Load the upvalue definitions TODO actually do something with these
                            for i in 0..upvalue_count {
                                let kind = self.frame_mut().read_u8();
                                let idx = self.frame_mut().read_u8();
                            }

                            HeapObject::LoxClosure {
                                name,
                                arity,
                                chunk: chunk.clone(),
                                // TODO upvalues!
                            }
                        }
                        _ => return Err(RuntimeError::NotACallable),
                    };

                    let closure_value = self.make_heap_value(closure_obj);
                    self.push(closure_value);
                }
                OpCode::GetUpvalue | OpCode::SetUpvalue => todo!(),
                // Other
                OpCode::Call => {
                    let arg_count: usize = self.frame_mut().read_u8().into();
                    self.call(self.peek(arg_count)?, arg_count)?;
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

    fn push_new_frame(&mut self, arg_count: usize, name: InternedString, chunk: Rc<Chunk>) {
        let new_frame = CallFrame {
            ip: 0,
            base_ptr: self.stack.len() - (arg_count + 1),
            name,
            chunk,
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

    pub fn insert_into_heap(&mut self, obj: HeapObject) -> GcPtr<HeapObject> {
        self.heap.insert(obj)
    }

    pub fn make_heap_value(&mut self, obj: HeapObject) -> Value {
        Value::Obj(self.insert_into_heap(obj))
    }

    pub fn call(&mut self, callee: Value, arg_count: usize) -> RuntimeResult<()> {
        // Only heap objects are callable, at present
        let heap_obj_ref = match &callee {
            Value::Obj(gc_ptr) => gc_ptr.borrow(),
            _ => return Err(RuntimeError::NotACallable),
        };

        // How we call it depends on the object -- do a big match
        match &*heap_obj_ref {
            HeapObject::LoxClosure { name, arity, chunk } => {
                // Check that the arity matches, and push the new frame
                if *arity != arg_count {
                    return Err(RuntimeError::WrongArity);
                }

                self.push_new_frame(arg_count, name.clone(), chunk.clone());
            }
            HeapObject::NativeFunction {
                arity, function, ..
            } => {
                if *arity != arg_count {
                    return Err(RuntimeError::WrongArity);
                }

                // Don't even do anything to the interpreter stack, just plug in the args
                let start_idx = self.stack.len() - arg_count;
                let arg_slice = &self.stack[start_idx..];

                let return_value = match function(arg_slice) {
                    Ok(v) => v,
                    Err(error_str) => return Err(RuntimeError::NativeError(error_str)),
                };

                // Strip off the args and the native fn, putting the return value on instead
                self.stack.truncate(start_idx - 1);
                self.push(return_value);
            }
        }

        Ok(())
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
        let chunk = &self.frame().chunk;
        match chunk.lookup_constant(idx) {
            ChunkConstant::String(s) => s,
            _ => panic!("Global table contains non-string"),
        }
    }
}
