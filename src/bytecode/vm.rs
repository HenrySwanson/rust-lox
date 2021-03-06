use std::collections::HashMap;
use std::rc::Rc;

use super::chunk::{Chunk, ChunkConstant};
use super::errs::{RuntimeError, RuntimeResult};
use super::gc::{GcHeap, GcPtr};
use super::native;
use super::native::NativeFunction;
use super::opcode::{ConstantIdx, RichOpcode, UpvalueAddr};
use super::string_interning::{InternedString, StringInterner};
use super::value::{
    LoxBoundMethod, LoxClass, LoxClosure, LoxInstance, PropertyLookup, UpvalueData, UpvalueRef,
    Value,
};

const GC_PERIOD: u32 = 1000;

struct CallFrame {
    ip: usize,
    base_ptr: usize,
    name: InternedString,
    chunk: Rc<Chunk>,
    upvalues: Rc<[UpvalueRef]>,
}

// like a vector but guarded by RuntimeResults everywhere
#[derive(Debug)]
struct SafeStack<T> {
    stack: Vec<T>,
}

struct ObjectHeap {
    closure_heap: GcHeap<LoxClosure>,
    class_heap: GcHeap<LoxClass>,
    instance_heap: GcHeap<LoxInstance>,
    bound_method_heap: GcHeap<LoxBoundMethod>,
}

pub struct VM {
    // Invocation data
    stack: SafeStack<Value>,
    call_stack: Vec<CallFrame>,
    open_upvalues: Vec<UpvalueRef>,

    // Persistent data
    string_table: StringInterner,
    globals: HashMap<InternedString, Value>,
    object_heap: ObjectHeap,
}

impl Value {
    fn cast_to_class(&self) -> RuntimeResult<GcPtr<LoxClass>> {
        match self {
            Value::Class(ptr) => Ok(ptr.clone()),
            _ => Err(RuntimeError::NotAClass),
        }
    }

    fn cast_to_instance(&self) -> RuntimeResult<GcPtr<LoxInstance>> {
        match self {
            Value::Instance(ptr) => Ok(ptr.clone()),
            _ => Err(RuntimeError::NotAnInstance),
        }
    }

    fn cast_to_closure(&self) -> RuntimeResult<GcPtr<LoxClosure>> {
        match self {
            Value::Closure(ptr) => Ok(ptr.clone()),
            _ => Err(RuntimeError::NotAClosure),
        }
    }
}

impl<T> SafeStack<T> {
    fn new() -> Self {
        SafeStack { stack: vec![] }
    }

    fn len(&self) -> usize {
        self.stack.len()
    }

    fn get(&self, idx: usize) -> RuntimeResult<&T> {
        self.stack.get(idx).ok_or(RuntimeError::InvalidStackIndex)
    }

    fn set(&mut self, idx: usize, item: T) -> RuntimeResult<()> {
        match self.stack.get_mut(idx) {
            Some(slot) => {
                *slot = item;
                Ok(())
            }
            None => Err(RuntimeError::InvalidStackIndex),
        }
    }

    fn peek(&self, depth: usize) -> RuntimeResult<&T> {
        let stack_size = self.len();
        self.get(stack_size - 1 - depth)
    }

    fn set_back(&mut self, depth: usize, item: T) -> RuntimeResult<()> {
        let stack_size = self.len();
        self.set(stack_size - 1 - depth, item)
    }

    fn peek_n(&self, depth: usize) -> RuntimeResult<&[T]> {
        let start_idx = self.len() - depth;
        self.stack
            .get(start_idx..)
            .ok_or(RuntimeError::InvalidStackIndex)
    }

    fn push(&mut self, item: T) {
        self.stack.push(item)
    }

    fn pop(&mut self) -> RuntimeResult<T> {
        self.stack.pop().ok_or(RuntimeError::StackEmpty)
    }

    fn pop_n(&mut self, depth: usize) -> RuntimeResult<()> {
        if depth > self.len() {
            Err(RuntimeError::InvalidStackIndex)
        } else {
            let idx = self.len() - depth;
            self.truncate(idx);
            Ok(())
        }
    }

    fn truncate(&mut self, idx: usize) {
        self.stack.truncate(idx)
    }

    fn clear(&mut self) {
        self.stack.clear()
    }

    fn iter(&self) -> std::slice::Iter<'_, T> {
        self.stack.iter()
    }
}

impl ObjectHeap {
    fn new() -> Self {
        ObjectHeap {
            closure_heap: GcHeap::new(),
            class_heap: GcHeap::new(),
            instance_heap: GcHeap::new(),
            bound_method_heap: GcHeap::new(),
        }
    }

    fn insert_new_closure(
        &mut self,
        name: InternedString,
        arity: usize,
        chunk: Rc<Chunk>,
        upvalues: Rc<[UpvalueRef]>,
    ) -> Value {
        let closure_obj = LoxClosure {
            name,
            arity,
            chunk,
            upvalues,
        };
        let closure_ptr = self.closure_heap.insert(closure_obj);
        Value::Closure(closure_ptr)
    }

    fn insert_new_class(
        &mut self,
        name: InternedString,
        methods: HashMap<InternedString, GcPtr<LoxClosure>>,
    ) -> Value {
        let class_obj = LoxClass { name, methods };
        let class_ptr = self.class_heap.insert(class_obj);
        Value::Class(class_ptr)
    }

    fn insert_new_instance(
        &mut self,
        class: GcPtr<LoxClass>,
        fields: HashMap<InternedString, Value>,
    ) -> Value {
        let instance_obj = LoxInstance { class, fields };
        let instance_ptr = self.instance_heap.insert(instance_obj);
        Value::Instance(instance_ptr)
    }

    fn insert_new_bound_method(
        &mut self,
        receiver: GcPtr<LoxInstance>,
        closure: GcPtr<LoxClosure>,
    ) -> Value {
        let bound_method_obj = LoxBoundMethod { receiver, closure };
        let bound_method_ptr = self.bound_method_heap.insert(bound_method_obj);
        Value::BoundMethod(bound_method_ptr)
    }

    fn sweep_all(&mut self) {
        self.closure_heap.sweep();
        self.class_heap.sweep();
        self.instance_heap.sweep();
        self.bound_method_heap.sweep();
    }
}

impl VM {
    pub fn new() -> Self {
        let mut vm = VM {
            call_stack: vec![],
            stack: SafeStack::new(),
            open_upvalues: vec![],
            //
            string_table: StringInterner::new(),
            globals: HashMap::new(),
            object_heap: ObjectHeap::new(),
        };

        // Define natives
        for (name, arity, function) in native::get_natives().iter().copied() {
            let name = vm.intern_string(name);
            let native_fn = NativeFunction::new(&name, arity, function);
            vm.globals.insert(name, Value::NativeFunction(native_fn));
        }

        vm
    }

    pub fn interpret(&mut self, main_chunk: Chunk) -> RuntimeResult<()> {
        // Reset invocation state
        self.call_stack.clear();
        self.stack.clear();

        // Make a main() function and push it onto the stack
        let main_name = self.intern_string("<main>");
        let main_fn =
            self.object_heap
                .insert_new_closure(main_name, 0, Rc::new(main_chunk), Rc::from([]));
        self.stack.push(main_fn);

        // Call main to start the program
        self.call(0)?;

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
        let mut gc_counter = 0;
        loop {
            if gc_counter == GC_PERIOD {
                self.collect_garbage();
                gc_counter = 0;
            }
            gc_counter += 1;

            #[cfg(feature = "trace-execution")]
            {
                let frame = self.frame_mut();
                let ip = frame.ip;
                let base_ptr = frame.base_ptr;

                println!("STACK     {:?}", self.stack);
                println!(
                    "IP = {}, BP = {} ({:?})",
                    ip,
                    base_ptr,
                    self.stack.get(base_ptr)
                );
                self.frame_mut().chunk.disassemble_at(ip);
                println!();
            }

            match self.try_read_next_op()? {
                // Constants
                RichOpcode::Constant(idx) => {
                    let value = match self.fetch_constant(idx) {
                        ChunkConstant::Number(n) => Value::Number(n.into()),
                        ChunkConstant::String(s) => Value::String(s),
                        c => return Err(RuntimeError::UntranslatableConstant(c)),
                    };

                    self.stack.push(value);
                }
                RichOpcode::True => self.stack.push(Value::Boolean(true)),
                RichOpcode::False => self.stack.push(Value::Boolean(false)),
                RichOpcode::Nil => self.stack.push(Value::Nil),
                // Arithmetic
                RichOpcode::Add => self.do_add()?,
                RichOpcode::Subtract => self.arithmetic_binop(|a, b| a - b)?,
                RichOpcode::Multiply => self.arithmetic_binop(|a, b| a * b)?,
                RichOpcode::Divide => {
                    // Check for zero
                    if let Value::Number(n) = self.stack.peek(0)? {
                        if *n == 0 {
                            return Err(RuntimeError::DivideByZero);
                        }
                    }

                    self.arithmetic_binop(|a, b| a / b)?;
                }
                RichOpcode::Negate => match self.stack.peek(0)? {
                    Value::Number(n) => {
                        let n = n.to_owned();
                        self.stack.pop()?;
                        self.stack.push(Value::Number(-n));
                    }
                    _ => return Err(RuntimeError::IncorrectOperandType),
                },
                // Logical
                RichOpcode::Not => {
                    let value = self.stack.pop()?;
                    self.stack.push(Value::Boolean(!value.is_truthy()));
                }
                // Comparison
                RichOpcode::Equal => {
                    let rhs = self.stack.pop()?;
                    let lhs = self.stack.pop()?;
                    self.stack.push(Value::Boolean(lhs == rhs));
                }
                RichOpcode::GreaterThan => self.comparison_binop(|a, b| a > b)?,
                RichOpcode::LessThan => self.comparison_binop(|a, b| a < b)?,
                // Variables
                RichOpcode::DefineGlobal(idx) => {
                    let name = self.fetch_string(idx);
                    let value = self.stack.pop()?;
                    self.globals.insert(name, value);
                }
                RichOpcode::GetGlobal(idx) => {
                    let name = self.fetch_string(idx);
                    let value = match self.globals.get(&name) {
                        Some(value) => value.clone(),
                        None => {
                            let name: String = (*name).to_owned();
                            return Err(RuntimeError::UndefinedGlobal(name));
                        }
                    };
                    self.stack.push(value);
                }
                RichOpcode::SetGlobal(idx) => {
                    let name = self.fetch_string(idx);
                    if !self.globals.contains_key(&name) {
                        let name: String = (*name).to_owned();
                        return Err(RuntimeError::UndefinedGlobal(name));
                    }
                    // don't pop; assignment may be inside other expressions
                    let value = self.stack.peek(0)?.clone();
                    self.globals.insert(name, value);
                }
                RichOpcode::GetLocal(idx) => {
                    let idx = usize::from(idx);
                    let base_ptr = self.frame().base_ptr;
                    let value = self.stack.get(base_ptr + idx)?.clone();
                    self.stack.push(value);
                }
                RichOpcode::SetLocal(idx) => {
                    let idx = usize::from(idx);
                    let base_ptr = self.frame().base_ptr;
                    let value = self.stack.peek(0)?.clone();
                    self.stack.set(base_ptr + idx, value)?;
                }
                // Jumps
                RichOpcode::Jump(offset) => {
                    let offset = usize::from(offset);
                    self.frame_mut().ip += offset;
                }
                RichOpcode::JumpIfFalse(offset) => {
                    let offset = usize::from(offset);
                    if !self.stack.peek(0)?.is_truthy() {
                        self.frame_mut().ip += offset;
                    }
                }
                RichOpcode::Loop(offset) => {
                    let offset = usize::from(offset);
                    self.frame_mut().ip -= offset;
                }
                // Closures and Upvalues
                RichOpcode::MakeClosure(idx) => {
                    // Load a constant; we can only proceed if it's a FnTemplate
                    let (name, arity, chunk, upvalue_count) = match self.fetch_constant(idx) {
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
                        let upvalue_addr = self.try_read_next_upvalue()?;
                        let upvalue_obj = match upvalue_addr {
                            UpvalueAddr::Immediate(local_idx) => {
                                // Immediate means the upvalue is a local of the parent.
                                // But we are (right now) in the frame of this closure's parent,
                                // so this upvalue is definitely still on the stack.
                                let stack_idx = self.frame().base_ptr + local_idx as usize;
                                self.make_open_upvalue(stack_idx)
                            }
                            UpvalueAddr::Recursive(upvalue_idx) => {
                                // Recursive means the upvalue is a local of grandparent or
                                // higher. In other words, it's an upvalue of the parent, and
                                // the index is into the parent upvalues. Just grab it from
                                // our upvalue list and clone it.
                                self.frame().upvalues[upvalue_idx as usize].clone()
                            }
                        };
                        upvalues.push(upvalue_obj);
                    }

                    let closure = self.object_heap.insert_new_closure(
                        name,
                        arity,
                        chunk.clone(),
                        Rc::from(upvalues),
                    );
                    self.stack.push(closure);
                }
                RichOpcode::GetUpvalue(idx) => {
                    let idx = usize::from(idx);
                    let value = match &*self.frame().upvalues[idx].borrow() {
                        UpvalueData::Open(idx) => self.stack.get(*idx)?.clone(),
                        UpvalueData::Closed(v) => v.clone(),
                    };

                    self.stack.push(value);
                }
                RichOpcode::SetUpvalue(idx) => {
                    let idx = usize::from(idx);
                    let value = self.stack.peek(0)?.clone();

                    let mut upvalue = self.frame().upvalues[idx].clone();
                    match &mut *upvalue.borrow_mut() {
                        UpvalueData::Open(idx) => self.stack.set(*idx, value)?,
                        UpvalueData::Closed(slot) => *slot = value,
                    };
                }
                RichOpcode::CloseUpvalue => {
                    self.close_upvalues(self.stack.len() - 1)?; // just the topmost element
                    self.stack.pop()?;
                }
                // Classes
                RichOpcode::MakeClass(idx) => {
                    let name = self.fetch_string(idx);
                    let class_value = self.object_heap.insert_new_class(name, HashMap::new());
                    self.stack.push(class_value);
                }
                RichOpcode::GetProperty(idx) => {
                    let name = self.fetch_string(idx);
                    let instance_ptr = self.stack.peek(0)?.cast_to_instance()?;

                    let value = match instance_ptr.borrow().lookup(&name) {
                        PropertyLookup::Field(value) => value,
                        PropertyLookup::Method(method) => self
                            .object_heap
                            .insert_new_bound_method(instance_ptr.clone(), method),
                        PropertyLookup::NotFound => return Err(RuntimeError::UndefinedProperty),
                    };

                    // Pop the instance, push the bound method
                    self.stack.pop()?;
                    self.stack.push(value);
                }
                RichOpcode::SetProperty(idx) => {
                    let name = self.fetch_string(idx);
                    let value = self.stack.peek(0)?.clone();
                    let mut instance_ptr = self.stack.peek(1)?.cast_to_instance()?;

                    instance_ptr.borrow_mut().fields.insert(name, value.clone());

                    // Remove the instance from the stack, but leave the value
                    self.stack.pop()?;
                    self.stack.pop()?;
                    self.stack.push(value);
                }
                RichOpcode::MakeMethod(idx) => {
                    let method_name = self.fetch_string(idx);
                    let method_ptr = self.stack.peek(0)?.cast_to_closure()?;
                    let mut class_ptr = self.stack.peek(1)?.cast_to_class()?;

                    class_ptr
                        .borrow_mut()
                        .methods
                        .insert(method_name, method_ptr.clone());
                    self.stack.pop()?; // pop just the method
                }
                RichOpcode::Invoke(idx, argc) => {
                    let method_name = self.fetch_string(idx);
                    let arg_count = usize::from(argc);

                    let receiver_ptr = self.stack.peek(arg_count)?.cast_to_instance()?;

                    // Gotta check the fields still; must behave identically to a
                    // OP_GET_PROPERTY + OP_CALL
                    match receiver_ptr.borrow().lookup(&method_name) {
                        PropertyLookup::Field(value) => {
                            self.stack.set_back(arg_count, value)?;
                            self.call(arg_count)?;
                        }
                        PropertyLookup::Method(method) => self.call_closure(method, arg_count)?,
                        PropertyLookup::NotFound => return Err(RuntimeError::UndefinedProperty),
                    };
                }
                RichOpcode::Inherit => {
                    let superclass_ptr = self.stack.peek(1)?.cast_to_class()?;
                    let mut class_ptr = self.stack.peek(0)?.cast_to_class()?;

                    let methods = superclass_ptr.borrow().methods.clone();
                    class_ptr.borrow_mut().methods = methods;
                }
                RichOpcode::GetSuper(idx) => {
                    let method_name = self.fetch_string(idx);
                    let class_ptr = self.stack.peek(0)?.cast_to_class()?;
                    let instance_ptr = self.stack.peek(1)?.cast_to_instance()?;

                    let method_ptr = match class_ptr.borrow().methods.get(&method_name) {
                        Some(method_ptr) => method_ptr.clone(),
                        None => return Err(RuntimeError::UndefinedProperty),
                    };

                    let value = self
                        .object_heap
                        .insert_new_bound_method(instance_ptr.clone(), method_ptr);

                    // Remove the two operands and push the result
                    self.stack.pop()?;
                    self.stack.pop()?;
                    self.stack.push(value);
                }
                RichOpcode::SuperInvoke(idx, argc) => {
                    let method_name = self.fetch_string(idx);
                    let arg_count = usize::from(argc);

                    let superclass_ptr = self.stack.peek(0)?.cast_to_class()?;

                    // No need to check the fields, this must be a method. The stack is
                    // already set up exactly how we want it.
                    match superclass_ptr.borrow().methods.get(&method_name) {
                        Some(method) => self.call_closure(method.clone(), arg_count)?,
                        None => return Err(RuntimeError::UndefinedProperty),
                    };

                    self.stack.pop()?;
                }
                // Other
                RichOpcode::Call(argc) => {
                    let arg_count = usize::from(argc);
                    self.call(arg_count)?;
                }
                RichOpcode::Return => {
                    // Rescue the return value off the stack
                    let result = self.pop_frame()?;

                    // Now check if there are any frames left
                    if self.call_stack.is_empty() {
                        return Ok(()); // exit loop
                    } else {
                        self.stack.push(result);
                    }
                }
                RichOpcode::Print => {
                    let value = self.stack.pop()?;
                    println!("[out] {:?}", value);
                }
                RichOpcode::Pop => {
                    self.stack.pop()?;
                }
            }
        }
    }

    // ---- helpers for the call stack ----

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
        upvalues: Rc<[UpvalueRef]>,
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
        let result = self.stack.pop()?;
        let frame = self.call_stack.pop().expect("Call stack empty!");

        // Close all the upvalues above and including the most recent frame ptr
        self.close_upvalues(frame.base_ptr)?;

        // Now we can clear the stack
        self.stack.truncate(frame.base_ptr);

        Ok(result)
    }

    fn try_read_next_op(&mut self) -> RuntimeResult<RichOpcode> {
        let frame = self.frame_mut();
        let result = frame.chunk.try_read_op(frame.ip);
        match result {
            Ok((op, next_ip)) => {
                frame.ip = next_ip;
                Ok(op)
            }
            Err(e) => Err(RuntimeError::from(e)),
        }
    }

    fn try_read_next_upvalue(&mut self) -> RuntimeResult<UpvalueAddr> {
        let frame = self.frame_mut();
        let result = frame.chunk.try_read_upvalue(frame.ip);
        match result {
            Ok(addr) => {
                frame.ip += 2;
                Ok(addr)
            }
            Err(e) => Err(RuntimeError::from(e)),
        }
    }

    fn fetch_constant(&mut self, idx: ConstantIdx) -> ChunkConstant {
        self.frame().chunk.lookup_constant(idx)
    }

    fn fetch_string(&mut self, idx: ConstantIdx) -> InternedString {
        match self.fetch_constant(idx) {
            ChunkConstant::String(s) => s,
            _ => panic!("Global table contains non-string"),
        }
    }

    pub fn call(&mut self, arg_count: usize) -> RuntimeResult<()> {
        let callee = self.stack.peek(arg_count)?.clone();

        // How we call it depends on the object -- do a big match
        match callee {
            Value::Closure(ptr) => self.call_closure(ptr, arg_count),
            Value::NativeFunction(func) => {
                if func.data.arity != arg_count {
                    return Err(RuntimeError::WrongArity);
                }

                // Don't even do anything to the interpreter stack, just plug in the args
                let arg_slice = self.stack.peek_n(arg_count)?;

                match (func.data.function)(arg_slice) {
                    Ok(return_value) => {
                        // Strip off the args and the native fn, putting the return value on instead
                        self.stack.pop_n(arg_count + 1)?;
                        self.stack.push(return_value);
                        Ok(())
                    }
                    Err(error_str) => Err(RuntimeError::NativeError(error_str)),
                }
            }
            Value::Class(class_ptr) => {
                // Create a "blank" instance
                let instance = self
                    .object_heap
                    .insert_new_instance(class_ptr.clone(), HashMap::new());

                // Inject it on the stack, underneath the arguments (where the class was).
                self.stack.set_back(arg_count, instance)?;

                // Then call the initializer, if it exists. The arguments will still be there,
                // and despite the initializer being a bound method in spirit, calling it as
                // a closure will work, since `this` is already in slot #0.
                if let Some(init) = class_ptr.borrow().methods.get("init") {
                    self.call_closure(init.clone(), arg_count)?
                } else if arg_count > 0 {
                    return Err(RuntimeError::ArgumentsToDefaultInitializer);
                };

                // No need to clean the stack, that will happen when we hit a return.
                Ok(())
            }
            Value::BoundMethod(ptr) => {
                let bound_method = ptr.borrow();
                let receiver = Value::Instance(bound_method.receiver.clone());

                // Inject the receiver at slot #0 in the frame (where the bound
                // method was)
                self.stack.set_back(arg_count, receiver)?;
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

    fn make_open_upvalue(&mut self, stack_idx: usize) -> UpvalueRef {
        // Returns an upvalue pointing to the given stack slot.

        // Because UpvalueData between siblings need to be shared, we first
        // search for an existing open UpvalueRef, and return that if possible.
        // (Parent and child also need to share upvalues, but because the child
        // simply clones the UpvalueRef from the parent's array, we get sharing for
        // free).
        fn index_match(upvalue: &UpvalueRef, stack_idx: usize) -> bool {
            stack_idx
                == upvalue
                    .get_open_idx()
                    .expect("open_upvalues contains closed upvalue!")
        }

        match self
            .open_upvalues
            .iter()
            .find(|u| index_match(u, stack_idx))
        {
            // There's a pre-existing upvalue we should grab.
            Some(upvalue) => upvalue.clone(),
            // No matches; make a new one ourselves.
            None => {
                let upvalue = UpvalueRef::new_open(stack_idx);
                self.open_upvalues.push(upvalue.clone());
                upvalue
            }
        }
    }

    fn close_upvalues(&mut self, stack_idx: usize) -> RuntimeResult<()> {
        // Closes all upvalues corresponding to stack slots at or above the given index
        for upvalue in self.open_upvalues.iter() {
            let idx = upvalue
                .get_open_idx()
                .expect("open_upvalues contains closed upvalue!");
            if idx >= stack_idx {
                let value = self.stack.get(idx)?.clone();
                upvalue.close_over_value(value);
            }
        }

        // Remove all closed upvalues
        self.open_upvalues.retain(|u| u.get_open_idx().is_some());

        Ok(())
    }

    // -- other helpers --

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
        // claim more strings.
        self.object_heap.sweep_all();
        self.string_table.clean();
    }

    fn do_add(&mut self) -> RuntimeResult<()> {
        let lhs = self.stack.peek(1)?;
        let rhs = self.stack.peek(0)?;

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

        self.stack.pop()?;
        self.stack.pop()?;
        self.stack.push(value);

        Ok(())
    }

    fn numerical_binop<F>(&mut self, closure: F) -> RuntimeResult<()>
    where
        F: Fn(i64, i64) -> Value,
    {
        let lhs = self.stack.peek(1)?;
        let rhs = self.stack.peek(0)?;

        match (lhs, rhs) {
            (Value::Number(a), Value::Number(b)) => {
                let result = closure(*a, *b);
                self.stack.pop()?;
                self.stack.pop()?;
                self.stack.push(result);
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

    fn intern_string(&mut self, s: &str) -> InternedString {
        self.string_table.get_interned(s)
    }

    pub fn borrow_string_table(&mut self) -> &mut StringInterner {
        &mut self.string_table
    }
}
