use std::convert::{TryFrom, TryInto};
use std::rc::Rc;

use crate::frontend::ast;

use super::chunk::{Chunk, ChunkConstant};
use super::errs::{CompilerError, CompilerResult};
use super::opcode::{
    ConstantIdx, LocalIdx, RichOpcode, UpvalueAddr, UpvalueIdx, MAX_LOCALS, MAX_UPVALUES,
};
use super::string_interning::StringInterner;

const THIS_STR: &str = "this";
const SUPER_STR: &str = "super";

// Describes the information of a local variable (a variable declared in
// the current context).
struct Local {
    name: String,
    scope_depth: u32,
    initialized: bool,
    captured: bool,
}

// A return type describing how to access a resolved variable name
enum VariableLocator {
    Local(LocalIdx),
    Upvalue(UpvalueIdx),
    Global,
}

#[derive(PartialEq, Eq)]
enum FunctionType {
    Root,
    Function,
    Method,
    Initializer,
}

struct Context {
    chunk: Chunk,
    locals: Vec<Local>,
    upvalues: Vec<UpvalueAddr>,
    scope_depth: u32,
    function_type: FunctionType,
}

struct ClassContext {
    has_superclass: bool,
}

pub struct Compiler<'strtable> {
    // We need a string table to stuff strings in
    string_table: &'strtable mut StringInterner,
    context_stack: Vec<Context>,
    class_stack: Vec<ClassContext>,
}

impl Context {
    fn new(function_type: FunctionType) -> Self {
        // TODO can we drop the ID for the non-class types?
        let reserved_id = match function_type {
            // The first item on the stack is the function itself
            FunctionType::Root | FunctionType::Function => "",
            // The first item on the stack is `this`
            FunctionType::Method | FunctionType::Initializer => THIS_STR,
        };
        let reserved_local = Local {
            name: reserved_id.to_owned(),
            scope_depth: 0,
            initialized: true,
            captured: false,
        };

        Context {
            chunk: Chunk::new(),
            locals: vec![reserved_local],
            upvalues: vec![],
            scope_depth: 0,
            function_type,
        }
    }

    fn find_local(&self, name: &str) -> CompilerResult<Option<LocalIdx>> {
        // Walking the array backwards is more efficient, I suppose
        for (idx, local) in self.locals.iter().enumerate().rev() {
            // note: idx is measured from the _bottom_ of the stack
            if local.name == name {
                if !local.initialized {
                    return Err(CompilerError::LocalUsedInOwnInitializer(name.to_owned()));
                }

                return Ok(Some(idx.try_into().unwrap()));
            }
        }

        Ok(None)
    }

    fn add_new_local(&mut self, name: &str) -> CompilerResult<()> {
        if self.locals.len() == MAX_LOCALS {
            return Err(CompilerError::TooManyLocals);
        }

        for local in self.locals.iter().rev() {
            if local.scope_depth == self.scope_depth && local.name == name {
                return Err(CompilerError::LocalAlreadyExists(name.to_owned()));
            }
        }

        self.locals.push(Local {
            name: name.to_owned(),
            scope_depth: self.scope_depth,
            initialized: false,
            captured: false,
        });

        Ok(())
    }

    fn add_upvalue(&mut self, key: UpvalueAddr) -> CompilerResult<UpvalueIdx> {
        for (i, upvalue) in self.upvalues.iter().enumerate() {
            if key == *upvalue {
                return Ok(i.try_into().unwrap());
            }
        }

        // Didn't find it, let's add it if possible
        if self.upvalues.len() == MAX_UPVALUES {
            return Err(CompilerError::TooManyUpvalues);
        }

        self.upvalues.push(key);
        let idx = self.upvalues.len() - 1;

        Ok(idx.try_into().unwrap())
    }

    fn mark_last_local_initialized(&mut self) {
        self.locals.last_mut().unwrap().initialized = true;
    }
}

impl<'strtable> Compiler<'strtable> {
    pub fn new(string_table: &'strtable mut StringInterner) -> Self {
        Compiler {
            string_table,
            context_stack: vec![],
            class_stack: vec![],
        }
    }

    pub fn compile(&mut self, stmts: &[ast::Stmt]) -> CompilerResult<Chunk> {
        // Put in a fresh context
        self.context_stack.push(Context::new(FunctionType::Root));

        for stmt in stmts.iter() {
            self.compile_statement(stmt)?;
        }

        // Return, to exit the VM
        self.emit_op(RichOpcode::Return, 0);

        // Return the chunk defining main()
        let root_state = self.context_stack.pop().expect("Context stack empty!");

        self.print_chunk("<main>", &root_state.chunk); // for debugging

        Ok(root_state.chunk)
    }

    pub fn compile_statement(&mut self, stmt: &ast::Stmt) -> CompilerResult<()> {
        let line_no = stmt.span.lo.line_no;
        match &stmt.kind {
            ast::StmtKind::Expression(expr) => {
                self.compile_expression(expr)?;
                self.emit_op(RichOpcode::Pop, line_no);
            }
            ast::StmtKind::Print(expr) => {
                self.compile_expression(expr)?;
                self.emit_op(RichOpcode::Print, line_no);
            }
            ast::StmtKind::VariableDecl(name, expr) => {
                // Create instructions to put the value of expr on top of the stack
                self.compile_expression(expr)?;

                self.declare_variable(name)?;
                self.define_variable(name, line_no)?;
            }
            ast::StmtKind::Block(stmts) => {
                self.begin_scope();
                for stmt in stmts.iter() {
                    self.compile_statement(stmt)?;
                }
                self.end_scope();
            }
            ast::StmtKind::IfElse(condition, if_body, else_body) => {
                self.compile_if_statement(condition, if_body.as_ref(), else_body.as_deref())?;
            }
            ast::StmtKind::While(condition, body) => {
                self.compile_while_statement(condition, body.as_ref())?;
            }
            ast::StmtKind::FunctionDecl(fn_decl) => {
                // Functions are allowed to refer to themselves; create a local with the
                // appropriate name before actually compiling the function.
                self.declare_variable(&fn_decl.name)?;
                if self.get_ctx().scope_depth != 0 {
                    self.get_ctx_mut().mark_last_local_initialized();
                }

                self.compile_function_decl(fn_decl, line_no, FunctionType::Function)?;

                // Lastly, define the function variable
                self.define_variable(&fn_decl.name, line_no)?;
            }
            ast::StmtKind::Return(expr) => {
                self.compile_return(expr.as_ref(), line_no)?;
            }
            ast::StmtKind::ClassDecl(name, superclass, methods) => {
                self.declare_variable(name)?;

                // Store the name as a constant
                let idx = self.add_string_constant(name)?;
                self.emit_op(RichOpcode::MakeClass(idx), line_no);
                self.define_variable(name, line_no)?;

                // Push the class context onto the stack
                self.class_stack.push(ClassContext {
                    has_superclass: superclass.is_some(),
                });

                // If there's a superclass, load it in the next local slot.
                // Whether or not there is, the class goes on the stack next.
                if let Some(superclass) = superclass {
                    if superclass == name {
                        return Err(CompilerError::SelfInherit(name.to_owned()));
                    }

                    self.begin_scope();
                    self.declare_variable(SUPER_STR)?;
                    self.define_variable(SUPER_STR, line_no)?;

                    self.get_variable(&superclass, line_no)?;
                    self.get_variable(name, line_no)?;
                    self.emit_op(RichOpcode::Inherit, line_no);
                } else {
                    self.get_variable(name, line_no)?;
                }

                // Deal with the methods
                for method in methods.iter() {
                    let fn_type = if method.name == "init" {
                        FunctionType::Initializer
                    } else {
                        FunctionType::Method
                    };
                    self.compile_function_decl(&method, method.body.span.lo.line_no, fn_type)?;

                    let idx = self.add_string_constant(&method.name)?;
                    self.emit_op(RichOpcode::MakeMethod(idx), method.body.span.hi.line_no);
                }

                // Pop the class off the stack
                self.emit_op(RichOpcode::Pop, line_no);

                if superclass.is_some() {
                    self.end_scope();
                }

                self.class_stack.pop();
            }
        };

        Ok(())
    }

    fn compile_if_statement(
        &mut self,
        condition: &ast::Expr,
        if_body: &ast::Stmt,
        else_body: Option<&ast::Stmt>,
    ) -> CompilerResult<()> {
        let line_no = condition.span.lo.line_no;

        self.compile_expression(condition)?;

        // If we pass the jump, pop the condition and do the if-body
        let jump_to_else = self.emit_jump(RichOpcode::JumpIfFalse(0), line_no);
        self.emit_op(RichOpcode::Pop, line_no);
        self.compile_statement(if_body)?;

        // Otherwise, pop the condition now, and do the else body.
        // Note that we always need an else-jump so that we only ever
        // pop once.
        let jump_over_else = self.emit_jump(RichOpcode::Jump(0), line_no);
        self.patch_jump(jump_to_else)?;
        self.emit_op(RichOpcode::Pop, line_no);
        if let Some(else_body) = else_body {
            self.compile_statement(else_body)?;
        }
        self.patch_jump(jump_over_else)?;

        Ok(())
    }

    fn compile_while_statement(
        &mut self,
        condition: &ast::Expr,
        body: &ast::Stmt,
    ) -> CompilerResult<()> {
        let line_no = condition.span.lo.line_no;
        let loop_start = self.current_chunk().len();

        self.compile_expression(condition)?;
        let exit_jump = self.emit_jump(RichOpcode::JumpIfFalse(0), line_no);
        self.emit_op(RichOpcode::Pop, line_no);

        self.compile_statement(body)?;
        self.emit_loop(loop_start, line_no)?;

        self.patch_jump(exit_jump)?;
        self.emit_op(RichOpcode::Pop, line_no);

        Ok(())
    }

    fn compile_function_decl(
        &mut self,
        fn_decl: &ast::FunctionDecl,
        line_no: usize,
        fn_type: FunctionType,
    ) -> CompilerResult<()> {
        // Create the new compiler context
        let new_context = Context::new(fn_type);

        self.context_stack.push(new_context);
        self.begin_scope();

        // Declare+define the arguments
        for param in fn_decl.params.iter() {
            // Unlike normal local variables, we don't compile any expressions here
            self.declare_variable(param)?;
            self.define_variable(param, line_no)?;
        }

        // Compile the function body, and add an implicit return
        self.compile_statement(fn_decl.body.as_ref())?;
        let last_line = fn_decl.body.span.hi.line_no;
        self.compile_return(None, last_line)?;
        self.emit_op(RichOpcode::Return, last_line);

        // no need to end scope, we're just killing this compiler context completely
        let ctx = self.context_stack.pop().expect("Context stack empty!");

        self.print_chunk(&fn_decl.name, &ctx.chunk); // for debugging

        // Build the function template and stash it in the chunk
        let fn_template = ChunkConstant::FnTemplate {
            name: self.string_table.get_interned(&fn_decl.name),
            arity: fn_decl.params.len(),
            chunk: Rc::new(ctx.chunk),
            upvalue_count: ctx.upvalues.len(),
        };

        // Unlike regular constants, we load this with MAKE_CLOSURE
        // TODO should i have a separate list for these in the chunk?
        let idx = self.add_constant(fn_template)?;
        self.emit_op(RichOpcode::MakeClosure(idx), line_no);

        // Now encode all the upvalues that this closure should have
        for upvalue in ctx.upvalues.iter().cloned() {
            self.current_chunk().write_upvalue(upvalue, line_no);
        }

        Ok(())
    }

    fn compile_expression(&mut self, expr: &ast::Expr) -> CompilerResult<()> {
        // stack effect: puts value on top of the stack
        let line_no = expr.span.lo.line_no;
        match &expr.kind {
            ast::ExprKind::Literal(literal) => self.compile_literal(literal, line_no)?,
            ast::ExprKind::BinOp(op, lhs, rhs) => self.compile_infix(*op, lhs, rhs)?,
            ast::ExprKind::UnaryOp(op, expr) => self.compile_prefix(*op, expr)?,
            ast::ExprKind::Variable(var) => self.get_variable(&var, line_no)?,
            ast::ExprKind::Assignment(var, expr) => self.set_variable(&var, expr, line_no)?,
            ast::ExprKind::Logical(ast::LogicalOperator::And, lhs, rhs) => {
                self.compile_and(lhs, rhs)?
            }
            ast::ExprKind::Logical(ast::LogicalOperator::Or, lhs, rhs) => {
                self.compile_or(lhs, rhs)?
            }
            ast::ExprKind::Call(callee, args) => self.compile_call(callee, args)?,
            ast::ExprKind::Get(expr, name) => {
                let idx = self.add_string_constant(name)?;
                self.compile_expression(expr)?;
                self.emit_op(RichOpcode::GetProperty(idx), line_no);
            }
            ast::ExprKind::Set(expr, name, value_expr) => {
                let idx = self.add_string_constant(name)?;
                self.compile_expression(expr)?;
                self.compile_expression(value_expr)?;
                self.emit_op(RichOpcode::SetProperty(idx), line_no);
            }
            ast::ExprKind::This => {
                if self.class_stack.is_empty() {
                    return Err(CompilerError::ThisOutsideClass);
                }
                self.get_variable(THIS_STR, line_no)?;
            }
            ast::ExprKind::Super(method_name) => {
                // Check if we are in a class with a superclass
                self.check_super_validity()?;

                self.get_variable(THIS_STR, line_no)?;
                self.get_variable(SUPER_STR, line_no)?;

                let idx = self.add_string_constant(method_name)?;
                self.emit_op(RichOpcode::GetSuper(idx), line_no);
            }
        };

        Ok(())
    }

    fn compile_literal(&mut self, literal: &ast::Literal, line_no: usize) -> CompilerResult<()> {
        match literal {
            ast::Literal::Number(n) => {
                let value = ChunkConstant::Number(*n);
                let idx = self.add_constant(value)?;
                self.emit_op(RichOpcode::Constant(idx), line_no);
            }
            ast::Literal::Boolean(b) => {
                let opcode = if *b {
                    RichOpcode::True
                } else {
                    RichOpcode::False
                };
                self.emit_op(opcode, line_no);
            }
            ast::Literal::Str(s) => {
                let value = ChunkConstant::String(self.string_table.get_interned(s));
                let idx = self.add_constant(value)?;
                self.emit_op(RichOpcode::Constant(idx), line_no);
            }
            ast::Literal::Nil => {
                self.emit_op(RichOpcode::Nil, line_no);
            }
        };

        Ok(())
    }

    fn compile_infix(
        &mut self,
        op: ast::BinaryOperator,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
    ) -> CompilerResult<()> {
        let line_no = lhs.span.hi.line_no; // I guess??

        self.compile_expression(lhs)?;
        self.compile_expression(rhs)?;

        match op {
            ast::BinaryOperator::Add => self.emit_op(RichOpcode::Add, line_no),
            ast::BinaryOperator::Subtract => self.emit_op(RichOpcode::Subtract, line_no),
            ast::BinaryOperator::Multiply => self.emit_op(RichOpcode::Multiply, line_no),
            ast::BinaryOperator::Divide => self.emit_op(RichOpcode::Divide, line_no),
            ast::BinaryOperator::EqualTo => self.emit_op(RichOpcode::Equal, line_no),
            ast::BinaryOperator::NotEqualTo => {
                self.emit_op(RichOpcode::Equal, line_no);
                self.emit_op(RichOpcode::Not, line_no)
            }
            ast::BinaryOperator::GreaterThan => self.emit_op(RichOpcode::GreaterThan, line_no),
            ast::BinaryOperator::GreaterEq => {
                self.emit_op(RichOpcode::LessThan, line_no);
                self.emit_op(RichOpcode::Not, line_no)
            }
            ast::BinaryOperator::LessThan => self.emit_op(RichOpcode::LessThan, line_no),
            ast::BinaryOperator::LessEq => {
                self.emit_op(RichOpcode::GreaterThan, line_no);
                self.emit_op(RichOpcode::Not, line_no)
            }
        };

        Ok(())
    }

    fn compile_prefix(&mut self, op: ast::UnaryOperator, expr: &ast::Expr) -> CompilerResult<()> {
        self.compile_expression(expr)?;
        let opcode = match op {
            ast::UnaryOperator::Negate => RichOpcode::Negate,
            ast::UnaryOperator::LogicalNot => RichOpcode::Not,
        };

        self.emit_op(opcode, expr.span.lo.line_no);
        Ok(())
    }

    fn compile_and(&mut self, lhs: &ast::Expr, rhs: &ast::Expr) -> CompilerResult<()> {
        self.compile_expression(lhs)?;

        // lhs is now on the stack; if it's falsey, keep it. otherwise, try the rhs.
        let jump = self.emit_jump(RichOpcode::JumpIfFalse(0), lhs.span.lo.line_no);
        self.emit_op(RichOpcode::Pop, rhs.span.lo.line_no);
        self.compile_expression(rhs)?;
        self.patch_jump(jump)?;

        Ok(())
    }

    fn compile_or(&mut self, lhs: &ast::Expr, rhs: &ast::Expr) -> CompilerResult<()> {
        self.compile_expression(lhs)?;

        // lhs is now on the stack. if it's true, we want to keep it.
        let skip_jump = self.emit_jump(RichOpcode::JumpIfFalse(0), lhs.span.lo.line_no);
        let jump_for_true = self.emit_jump(RichOpcode::Jump(0), lhs.span.lo.line_no);
        self.patch_jump(skip_jump)?;

        self.emit_op(RichOpcode::Pop, rhs.span.lo.line_no);
        self.compile_expression(rhs)?;
        self.patch_jump(jump_for_true)?;

        Ok(())
    }

    fn compile_call(&mut self, callee: &ast::Expr, args: &[ast::Expr]) -> CompilerResult<()> {
        let num_args = u8::try_from(args.len()).expect("Too many arguments in AST");

        // Special cases: If we're calling a method on an instance, we can emit an OP_INVOKE,
        // and if we're calling a super method, we can emit OP_SUPER_INVOKE.
        match &callee.kind {
            ast::ExprKind::Get(instance_expr, method_name) => {
                // We put the instance on the stack, and then all the arguments
                self.compile_expression(instance_expr)?;
                for arg in args.iter() {
                    self.compile_expression(arg)?;
                }

                // Then we emit the OP_INVOKE
                let line_no = callee.span.hi.line_no;
                let idx = self.add_string_constant(method_name)?;

                self.emit_op(RichOpcode::Invoke(idx, num_args), line_no);
            }
            ast::ExprKind::Super(method_name) => {
                self.check_super_validity()?;

                // We put the receiver on the stack, then all the arguments, then the superclass
                let line_no = callee.span.lo.line_no;
                self.get_variable(THIS_STR, line_no)?;
                for arg in args.iter() {
                    self.compile_expression(arg)?;
                }
                self.get_variable(SUPER_STR, line_no)?;

                // Then we emit the OP_SUPER_INVOKE
                let line_no = callee.span.hi.line_no;
                let idx = self.add_string_constant(method_name)?;

                self.emit_op(RichOpcode::SuperInvoke(idx, num_args), line_no);
            }
            _ => {
                // Normal path: put the callee and its args on the stack, in order
                self.compile_expression(callee)?;
                for arg in args.iter() {
                    self.compile_expression(arg)?;
                }
                self.emit_op(RichOpcode::Call(num_args), callee.span.hi.line_no);
            }
        }

        Ok(())
    }

    fn declare_variable(&mut self, name: &str) -> CompilerResult<()> {
        // Check if we're declaring a global or local variable
        if self.get_ctx().scope_depth == 0 {
            // nothing to do
        } else {
            // Add the local to the compiler's list.
            self.get_ctx_mut().add_new_local(name)?;
        }

        Ok(())
    }

    fn define_variable(&mut self, name: &str, line_no: usize) -> CompilerResult<()> {
        // Check if we're defining a global or local variable
        if self.get_ctx().scope_depth == 0 {
            // We store the name of the global as a string constant, so the VM can
            // keep track of it.
            let global_idx = self.add_string_constant(name)?;
            self.emit_op(RichOpcode::DefineGlobal(global_idx), line_no);
        } else {
            self.get_ctx_mut().mark_last_local_initialized();
        }

        Ok(())
    }

    fn get_variable(&mut self, var_name: &str, line_no: usize) -> CompilerResult<()> {
        match self.resolve_variable(var_name)? {
            VariableLocator::Global => {
                // Stash the name in the chunk constants, and emit a GetGlobal instruction.
                let global_idx = self.add_string_constant(var_name)?;
                self.emit_op(RichOpcode::GetGlobal(global_idx), line_no);
            }
            VariableLocator::Local(idx) => {
                self.emit_op(RichOpcode::GetLocal(idx), line_no);
            }
            VariableLocator::Upvalue(idx) => {
                self.emit_op(RichOpcode::GetUpvalue(idx), line_no);
            }
        }

        Ok(())
    }

    fn set_variable(
        &mut self,
        var_name: &str,
        expr: &ast::Expr,
        line_no: usize,
    ) -> CompilerResult<()> {
        // In any case, we need to emit instructions to put the result of the
        // expression on the stack.
        self.compile_expression(expr)?;

        match self.resolve_variable(var_name)? {
            VariableLocator::Global => {
                // Stash the name in the chunk constants, and emit a SetGlobal instruction.
                let global_idx = self.add_string_constant(var_name)?;
                self.emit_op(RichOpcode::SetGlobal(global_idx), line_no);
            }
            VariableLocator::Local(idx) => {
                self.emit_op(RichOpcode::SetLocal(idx), line_no);
            }
            VariableLocator::Upvalue(idx) => {
                self.emit_op(RichOpcode::SetUpvalue(idx), line_no);
            }
        }

        Ok(())
    }

    // ---- helpers ----

    fn get_ctx(&self) -> &Context {
        match self.context_stack.last() {
            Some(state) => state,
            None => panic!("Context stack empty!"),
        }
    }

    fn get_ctx_mut(&mut self) -> &mut Context {
        match self.context_stack.last_mut() {
            Some(state) => state,
            None => panic!("Context stack empty!"),
        }
    }

    fn check_super_validity(&self) -> CompilerResult<()> {
        match self.class_stack.last() {
            Some(class) => {
                if class.has_superclass {
                    Ok(())
                } else {
                    Err(CompilerError::SuperWithoutSuperclass)
                }
            }
            None => Err(CompilerError::SuperOutsideClass),
        }
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.get_ctx_mut().chunk
    }

    fn add_constant(&mut self, constant: ChunkConstant) -> CompilerResult<ConstantIdx> {
        self.current_chunk().add_constant(constant)
    }

    fn add_string_constant(&mut self, string: &str) -> CompilerResult<ConstantIdx> {
        let constant = ChunkConstant::String(self.string_table.get_interned(string));
        self.add_constant(constant)
    }

    fn begin_scope(&mut self) {
        self.get_ctx_mut().scope_depth += 1;
    }

    fn end_scope(&mut self) {
        let state = self.get_ctx_mut();
        state.scope_depth -= 1;

        // Go through the tail of the locals list, and pop off things until
        // we're done with that scope.
        while let Some(local) = state.locals.last() {
            if local.scope_depth <= state.scope_depth {
                break;
            }

            if local.captured {
                state.chunk.write_op(RichOpcode::CloseUpvalue, 0);
            } else {
                state.chunk.write_op(RichOpcode::Pop, 0); // TODO line no
            }

            state.locals.pop();
        }
    }

    fn resolve_variable(&mut self, name: &str) -> CompilerResult<VariableLocator> {
        // First, check if it's local to any of our enclosing scopes
        let mut found_at = None;
        for (stack_idx, context) in self.context_stack.iter().enumerate().rev() {
            if let Some(local_idx) = context.find_local(name)? {
                found_at = Some((stack_idx, local_idx));
                break;
            }
        }

        // If we haven't found it, it's hopefully global. Otherwise,
        // we know where it was defined.
        // TODO mark local captured
        let (root_idx, local_idx) = match found_at {
            Some(t) => t,
            None => return Ok(VariableLocator::Global),
        };

        // If it's in the topmost context, this is just a local. Easy.
        if root_idx == self.context_stack.len() - 1 {
            return Ok(VariableLocator::Local(local_idx));
        }

        // Otherwise, we mark it as captured, and start inserting upvalues into each
        // of the contexts in between.
        self.context_stack[root_idx].locals[local_idx as usize].captured = true;

        // The first upvalue we mark is an Immediate, but after that, it's all Recursive
        let mut upvalue_idx =
            self.context_stack[root_idx + 1].add_upvalue(UpvalueAddr::Immediate(local_idx))?;
        for context in self.context_stack[root_idx + 2..].iter_mut() {
            upvalue_idx = context.add_upvalue(UpvalueAddr::Recursive(upvalue_idx))?;
        }

        // If we made it here, it means this variable is local to some enclosing scope,
        // so it's located in our upvalue array.
        Ok(VariableLocator::Upvalue(upvalue_idx))
    }

    fn compile_return(&mut self, expr: Option<&ast::Expr>, line_no: usize) -> CompilerResult<()> {
        // Emit the instructions to put the return value on the stack
        match self.get_ctx().function_type {
            FunctionType::Root => return Err(CompilerError::ReturnAtTopLevel),
            FunctionType::Function | FunctionType::Method => match expr {
                Some(expr) => self.compile_expression(expr)?,
                None => self.emit_op(RichOpcode::Nil, line_no),
            },
            FunctionType::Initializer => {
                if expr.is_some() {
                    return Err(CompilerError::ReturnInInitializer);
                } else {
                    self.emit_op(RichOpcode::GetLocal(0), line_no);
                }
            }
        };

        self.emit_op(RichOpcode::Return, line_no);
        Ok(())
    }

    // chunk-writing methods

    fn emit_op(&mut self, op: RichOpcode, line_no: usize) {
        self.current_chunk().write_op(op, line_no);
    }

    fn emit_jump(&mut self, op: RichOpcode, line_no: usize) -> usize {
        let jmp_idx = self.current_chunk().len();
        self.emit_op(op, line_no);
        jmp_idx
    }

    fn patch_jump(&mut self, jmp_idx: usize) -> CompilerResult<()> {
        // distance from (instruction after JMP) to here
        let distance = self.current_chunk().len() - (jmp_idx + 3);

        // TODO: better error handling here...
        let distance = u16::try_from(distance).map_err(|_| CompilerError::JumpTooLong)?;
        self.current_chunk()
            .patch_u16(jmp_idx + 1, distance)
            .unwrap();
        Ok(())
    }

    fn emit_loop(&mut self, loop_start_idx: usize, line_no: usize) -> CompilerResult<()> {
        // distance from (instruction after LOOP) to loop_start
        let distance = (self.current_chunk().len() + 3) - loop_start_idx;
        let distance = u16::try_from(distance).map_err(|_| CompilerError::JumpTooLong)?;
        self.emit_op(RichOpcode::Loop(distance), line_no);
        Ok(())
    }

    #[allow(unused_variables)]
    fn print_chunk(&self, name: &str, chunk: &Chunk) {
        #[cfg(feature = "print-chunks")]
        chunk.disassemble(name);
    }
}
