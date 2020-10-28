use std::collections::HashMap;
use std::rc::Rc;

use super::chunk::{Chunk, ChunkConstant, ConstantIdx};
use super::compiler::Upvalue as UpvalueDef;
use super::errs::{RuntimeError, RuntimeResult};
use super::gc::{GcHeap, GcPtr};
use super::native;
use super::opcode::OpCode;
use super::string_interning::{InternedString, StringInterner};
use super::value::{
    LiveUpvalue, LoxBoundMethod, LoxClass, LoxClosure, LoxInstance, NativeFunction,
    NativeFunctionData, Value,
};

struct CallFrame {
    ip: usize,
    base_ptr: usize,
    name: InternedString,
    chunk: Rc<Chunk>,
    upvalues: Rc<Vec<LiveUpvalue>>,
}

pub struct VM {
    // Invocation data
    call_stack: Vec<CallFrame>,
    stack: Vec<Value>,
    open_upvalues: Vec<LiveUpvalue>,

    // Persistent data
    string_table: StringInterner,
    globals: HashMap<InternedString, Value>,
    closure_heap: GcHeap<LoxClosure>,
    class_heap: GcHeap<LoxClass>,
    instance_heap: GcHeap<LoxInstance>,
    bound_method_heap: GcHeap<LoxBoundMethod>,

    // Constant
    init_string: InternedString,
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

    fn try_read_upvalue(&mut self) -> Result<UpvalueDef, u8> {
        let kind = self.read_u8();
        match kind {
            1 => Ok(UpvalueDef::Immediate(self.read_u8())),
            0 => Ok(UpvalueDef::Recursive(self.read_u8())),
            _ => Err(kind),
        }
    }
}

impl VM {
    pub fn new() -> Self {
        let mut string_table = StringInterner::new();
        let init_string = string_table.get_interned("init");

        let mut vm = VM {
            call_stack: vec![],
            stack: vec![],
            open_upvalues: vec![],
            //
            string_table,
            globals: HashMap::new(),
            closure_heap: GcHeap::new(),
            class_heap: GcHeap::new(),
            instance_heap: GcHeap::new(),
            bound_method_heap: GcHeap::new(),
            //
            init_string,
        };

        // Define natives
        for (name, arity, function) in native::get_natives().iter().copied() {
            let name = vm.intern_string(name);
            let native_fn_data = NativeFunctionData {
                name: name.clone(),
                arity,
                function,
            };
            let native_fn = NativeFunction(Rc::new(native_fn_data));

            vm.globals.insert(name, Value::NativeFunction(native_fn));
        }

        vm
    }

    pub fn borrow_string_table(&mut self) -> &mut StringInterner {
        &mut self.string_table
    }

    pub fn interpret(&mut self, main_chunk: Chunk) -> RuntimeResult<()> {
        // Reset invocation state
        self.call_stack.clear();
        self.stack.clear();

        // Make main() a real value and push it onto the stack
        let main_name = self.intern_string("<main>");
        let main_fn = LoxClosure {
            name: main_name,
            arity: 0,
            chunk: Rc::new(main_chunk),
            upvalues: Rc::new(vec![]),
        };
        let main_fn = self.closure_heap.insert(main_fn);
        self.push(Value::Closure(main_fn));

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
            self.collect_garbage();

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
                    let (name, arity, chunk, upvalue_count) =
                        match self.frame().chunk.lookup_constant(idx) {
                            ChunkConstant::FnTemplate {
                                name,
                                arity,
                                chunk,
                                upvalue_count,
                            } => (name, arity, chunk, upvalue_count),
                            _ => return Err(RuntimeError::NotACallable),
                        };

                    // Read the upvalues
                    let mut upvalues = Vec::with_capacity(upvalue_count);
                    for _ in 0..upvalue_count {
                        let upvalue = match self
                            .frame_mut()
                            .try_read_upvalue()
                            .map_err(|_| RuntimeError::BadUpvalue)?
                        {
                            UpvalueDef::Immediate(idx) => {
                                // Immediate means the upvalue is a local of the parent.
                                // But we are (right now) in the frame of this closure's parent.
                                // Remember that idx is relative to the parent's frame.
                                let upvalue =
                                    LiveUpvalue::new(self.frame().base_ptr + idx as usize);
                                // Stash a copy in our open upvalue list
                                self.open_upvalues.push(upvalue.clone());
                                upvalue
                            }
                            UpvalueDef::Recursive(idx) => {
                                // Otherwise, it's one of our upvalues
                                self.frame().upvalues[idx as usize].clone()
                            }
                        };
                        upvalues.push(upvalue);
                    }

                    let closure = Value::Closure(self.closure_heap.insert(LoxClosure {
                        name,
                        arity,
                        chunk: chunk.clone(),
                        upvalues: Rc::new(upvalues),
                    }));

                    self.push(closure);
                }
                OpCode::GetUpvalue => {
                    let idx = self.frame_mut().read_u8() as usize;
                    let value = match self.frame().upvalues[idx].get_if_closed() {
                        Ok(v) => v,
                        Err(idx) => self.stack[idx].clone(),
                    };

                    self.push(value);
                }
                OpCode::SetUpvalue => {
                    let idx = self.frame_mut().read_u8() as usize;
                    let value = self.peek(0)?;

                    match self.frame().upvalues[idx].set_if_closed(&value) {
                        Ok(()) => {}
                        Err(idx) => self.stack[idx] = value,
                    }
                }
                OpCode::CloseUpvalue => {
                    self.close_upvalues(self.stack.len() - 1); // just the topmost element
                    self.pop()?;
                }
                // Classes
                OpCode::MakeClass => {
                    let idx = self.frame_mut().read_u8();
                    let name = self.lookup_string(idx);

                    let class_obj = LoxClass {
                        name,
                        methods: HashMap::new(),
                    };
                    let class_value = Value::Class(self.class_heap.insert(class_obj));
                    self.push(class_value);
                }
                OpCode::GetProperty => {
                    let idx = self.frame_mut().read_u8();
                    let name = self.lookup_string(idx);

                    let instance_ptr = match self.peek(0)? {
                        Value::Instance(ptr) => ptr,
                        _ => return Err(RuntimeError::NotAnInstance),
                    };
                    let instance = instance_ptr.borrow();

                    let value = match instance.fields.get(&name) {
                        Some(value) => value.clone(),
                        None => match instance.class.borrow().methods.get(&name) {
                            Some(method_ptr) => {
                                let bound_method = LoxBoundMethod {
                                    receiver: instance_ptr.clone(),
                                    closure: method_ptr.clone(),
                                };
                                Value::BoundMethod(self.bound_method_heap.insert(bound_method))
                            }
                            None => return Err(RuntimeError::UndefinedProperty),
                        },
                    };

                    // Pop the instance, push the bound method
                    self.pop()?;
                    self.push(value);
                }
                OpCode::SetProperty => {
                    let idx = self.frame_mut().read_u8();
                    let name = self.lookup_string(idx);

                    let value = self.peek(0)?;
                    let mut instance_ptr = match self.peek(1)? {
                        Value::Instance(ptr) => ptr,
                        _ => return Err(RuntimeError::NotAnInstance),
                    };
                    let mut instance = instance_ptr.borrow_mut();

                    instance.fields.insert(name, value.clone());

                    // Remove the instance from the stack, but leave the value
                    self.pop()?;
                    self.pop()?;
                    self.push(value);
                }
                OpCode::MakeMethod => {
                    let idx = self.frame_mut().read_u8();
                    let method_name = self.lookup_string(idx);

                    let method_ptr = match self.peek(0)? {
                        Value::Closure(ptr) => ptr,
                        _ => return Err(RuntimeError::NotACallable),
                    };
                    let mut class_ptr = match self.peek(1)? {
                        Value::Class(ptr) => ptr,
                        _ => return Err(RuntimeError::NotAClass),
                    };

                    class_ptr
                        .borrow_mut()
                        .methods
                        .insert(method_name, method_ptr);
                    self.pop()?; // pop just the method
                }
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

    fn push_new_frame(
        &mut self,
        arg_count: usize,
        name: InternedString,
        chunk: Rc<Chunk>,
        upvalues: Rc<Vec<LiveUpvalue>>,
    ) {
        let new_frame = CallFrame {
            ip: 0,
            base_ptr: self.stack.len() - (arg_count + 1),
            name,
            chunk,
            upvalues,
        };
        self.call_stack.push(new_frame);
    }

    fn pop_frame(&mut self) -> RuntimeResult<Value> {
        // Rescue the return value off the stack
        let result = self.pop()?;
        let frame = self.call_stack.pop().expect("Call stack empty!");

        // Close all the upvalues above and including the most recent frame ptr
        self.close_upvalues(frame.base_ptr);

        // Now we can clear the stack
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

    fn close_upvalues(&mut self, stack_idx: usize) {
        // Closes all upvalues corresponding to stack slots at or above the given index
        for upvalue in self.open_upvalues.iter() {
            match upvalue.get_open_idx() {
                Some(slot) if slot >= stack_idx => {
                    let value = self.stack[slot].clone();
                    upvalue.close(value);
                }
                _ => {}
            }
        }

        // Remove all closed upvalues
        self.open_upvalues.retain(|u| u.get_open_idx().is_some());
    }

    pub fn intern_string(&mut self, s: &str) -> InternedString {
        self.string_table.get_interned(s)
    }

    // TODO heap insert methods?

    pub fn call(&mut self, callee: Value, arg_count: usize) -> RuntimeResult<()> {
        // How we call it depends on the object -- do a big match
        match callee {
            Value::Closure(ptr) => self.call_closure(ptr, arg_count),
            Value::NativeFunction(func) => {
                if func.0.arity != arg_count {
                    return Err(RuntimeError::WrongArity);
                }

                // Don't even do anything to the interpreter stack, just plug in the args
                let start_idx = self.stack.len() - arg_count;
                let arg_slice = &self.stack[start_idx..];

                match (func.0.function)(arg_slice) {
                    Ok(return_value) => {
                        // Strip off the args and the native fn, putting the return value on instead
                        self.stack.truncate(start_idx - 1);
                        self.push(return_value);
                        Ok(())
                    }
                    Err(error_str) => Err(RuntimeError::NativeError(error_str)),
                }
            }
            Value::Class(class_ptr) => {
                // Create a "blank" instance
                let instance_value = Value::Instance(self.instance_heap.insert(LoxInstance {
                    class: class_ptr.clone(),
                    fields: HashMap::new(),
                }));

                // Inject it on the stack, underneath the arguments (where the class was).
                let slot_0 = self.stack.len() - arg_count - 1;
                self.stack[slot_0] = instance_value;

                // Then call the initializer, if it exists. The arguments will still be there,
                // and despite the initializer being a bound method in spirit, calling it as
                // a closure will work, since `this` is already in slot #0.
                if let Some(init) = class_ptr.borrow().methods.get(&self.init_string) {
                    self.call_closure(init.clone(), arg_count)?
                } else if arg_count > 0 {
                    return Err(RuntimeError::ArgumentsToDefaultInitializer);
                };

                // No need to clean the stack, that will happen when we hit a return.
                Ok(())
            }
            Value::BoundMethod(ptr) => {
                let bound_method = ptr.borrow();

                // Inject the receiver at slot #0 in the frame
                let slot_0 = self.stack.len() - arg_count - 1;
                self.stack[slot_0] = Value::Instance(bound_method.receiver.clone());
                self.call_closure(bound_method.closure.clone(), arg_count)
            }
            _ => Err(RuntimeError::NotACallable),
        }
    }

    // Nice to re-use this method
    fn call_closure(
        &mut self,
        closure_ptr: GcPtr<LoxClosure>,
        arg_count: usize,
    ) -> RuntimeResult<()> {
        let closure = closure_ptr.borrow();

        // Check that the arity matches, and push the new frame
        if closure.arity != arg_count {
            return Err(RuntimeError::WrongArity);
        }

        self.push_new_frame(
            arg_count,
            closure.name.clone(),
            closure.chunk.clone(),
            closure.upvalues.clone(),
        );

        Ok(())
    }

    fn collect_garbage(&mut self) {
        // Everything on the stack is reachable
        for value in self.stack.iter() {
            value.mark_internals();
        }

        // All globals are also reachable
        for value in self.globals.values() {
            value.mark_internals();
        }

        // Call frames contain reachable objects, but they should only contain
        // objects that are also reachable through the closure they correspond to,
        // which is on the stack and thus accessible.
        // Same with open upvalues.

        // Objects might point to strings, so let's kill the objects first, to
        // claim more strings. Order of heaps doesn't matter though.
        self.closure_heap.sweep();
        self.class_heap.sweep();
        self.instance_heap.sweep();
        self.bound_method_heap.sweep();
        self.string_table.clean();
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
