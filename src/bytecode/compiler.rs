use std::convert::{TryFrom, TryInto};
use std::rc::Rc;

use crate::common::ast;
use crate::common::operator::{InfixOperator, LogicalOperator, PrefixOperator};

use super::chunk::{Chunk, ChunkConstant, ConstantIdx};
use super::chunk::{UPVALUE_KIND_IMMEDIATE, UPVALUE_KIND_RECURSIVE};
use super::errs::{CompilerError, CompilerResult};
use super::opcode::OpCode;
use super::string_interning::StringInterner;

// since the GET_LOCAL instruction takes a byte
const MAX_LOCALS: usize = 256;
type LocalIdx = u8;

const MAX_UPVALUES: usize = 256;
type UpvalueIdx = u8;

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

// Describes the information of an upvalue (a variable declared in an
// enclosing scope.)
#[derive(Clone, PartialEq, Eq)]
enum Upvalue {
    // A local defined in the immediately enclosing scope
    Immediate(LocalIdx),
    // A local defined in some higher scope, i.e., an upvalue in
    // the immediately enclosing scope
    Recursive(UpvalueIdx),
}

// A return type describing how to access a resolved variable name
enum VariableLocator {
    Local(LocalIdx),
    Upvalue(UpvalueIdx),
    Global,
}

#[derive(PartialEq, Eq)]
enum FunctionType {
    Function,
    Method,
    Initializer,
}

struct Context {
    chunk: Chunk,
    locals: Vec<Local>,
    upvalues: Vec<Upvalue>,
    scope_depth: u32,
    is_initializer: bool,
}

pub struct Compiler<'strtable> {
    // We need a string table to stuff strings in
    string_table: &'strtable mut StringInterner,
    context_stack: Vec<Context>,
}

impl Context {
    fn new(reserved_id: &str) -> Self {
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
            is_initializer: false,
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

    fn add_upvalue(&mut self, key: Upvalue) -> CompilerResult<UpvalueIdx> {
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
        }
    }

    pub fn compile(&mut self, stmts: &[ast::Stmt]) -> CompilerResult<Chunk> {
        // Put in a fresh context
        self.context_stack.push(Context::new(""));

        for stmt in stmts.iter() {
            self.compile_statement(stmt)?;
        }

        // Return, to exit the VM
        self.current_chunk().write_op(OpCode::Return, 0);

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
                self.current_chunk().write_op(OpCode::Pop, line_no);
            }
            ast::StmtKind::Print(expr) => {
                self.compile_expression(expr)?;
                self.current_chunk().write_op(OpCode::Print, line_no);
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
                match expr {
                    Some(expr) => {
                        self.compile_expression(expr)?;
                    }
                    None => {
                        self.emit_implicit_return_arg(line_no);
                    }
                }
                self.current_chunk().write_op(OpCode::Return, line_no);
            }
            ast::StmtKind::ClassDecl(name, superclass, methods) => {
                self.declare_variable(name)?;

                // Store the name as a constant
                let idx = self.add_string_constant(name);
                self.current_chunk()
                    .write_op_with_u8(OpCode::MakeClass, idx, line_no);
                self.define_variable(name, line_no)?;

                // If there's a superclass, load it in the next local slot.
                // Whether or not there is, the class goes on the stack next.
                if let Some(superclass) = superclass {
                    if superclass.name == *name {
                        todo!();
                    }

                    self.begin_scope();
                    self.declare_variable(SUPER_STR)?;
                    self.define_variable(SUPER_STR, line_no)?;

                    self.get_variable(&superclass.name, line_no)?;
                    self.get_variable(name, line_no)?;
                    self.current_chunk().write_op(OpCode::Inherit, line_no);
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

                    let idx = self.add_string_constant(&method.name);
                    self.current_chunk().write_op_with_u8(
                        OpCode::MakeMethod,
                        idx,
                        method.body.span.hi.line_no,
                    );
                }

                // Pop the class off the stack
                self.current_chunk().write_op(OpCode::Pop, line_no);

                if superclass.is_some() {
                    self.end_scope();
                }
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
        let jump_to_else = self.current_chunk().emit_jump(OpCode::JumpIfFalse, line_no);
        self.current_chunk().write_op(OpCode::Pop, line_no);
        self.compile_statement(if_body)?;

        // Otherwise, pop the condition now, and do the else body.
        // Note that we always need an else-jump so that we only ever
        // pop once.
        let jump_over_else = self.current_chunk().emit_jump(OpCode::Jump, line_no);
        self.current_chunk().patch_jump(jump_to_else);
        self.current_chunk().write_op(OpCode::Pop, line_no);
        if let Some(else_body) = else_body {
            self.compile_statement(else_body)?;
        }
        self.current_chunk().patch_jump(jump_over_else);

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
        let exit_jump = self.current_chunk().emit_jump(OpCode::JumpIfFalse, line_no);
        self.current_chunk().write_op(OpCode::Pop, line_no);

        self.compile_statement(body)?;
        self.current_chunk().emit_loop(loop_start, line_no);

        self.current_chunk().patch_jump(exit_jump);
        self.current_chunk().write_op(OpCode::Pop, line_no);

        Ok(())
    }

    fn compile_function_decl(
        &mut self,
        fn_decl: &ast::FunctionDecl,
        line_no: usize,
        fn_type: FunctionType,
    ) -> CompilerResult<()> {
        // Create the new compiler context
        let mut new_context = Context::new(match fn_type {
            FunctionType::Function => "",
            FunctionType::Method | FunctionType::Initializer => THIS_STR,
        });
        if fn_type == FunctionType::Initializer {
            new_context.is_initializer = true;
        }

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
        self.emit_implicit_return_arg(last_line);
        self.current_chunk().write_op(OpCode::Return, last_line);

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
        let idx = self.add_constant(fn_template);
        self.current_chunk()
            .write_op_with_u8(OpCode::MakeClosure, idx, line_no);

        // Now encode all the upvalues that this closure should have
        for upvalue in ctx.upvalues.iter().cloned() {
            let bytes = match upvalue {
                Upvalue::Immediate(idx) => [UPVALUE_KIND_IMMEDIATE, idx],
                Upvalue::Recursive(idx) => [UPVALUE_KIND_RECURSIVE, idx],
            };
            self.current_chunk().write_u8(bytes[0], line_no);
            self.current_chunk().write_u8(bytes[1], line_no);
        }

        Ok(())
    }

    fn compile_expression(&mut self, expr: &ast::Expr) -> CompilerResult<()> {
        // stack effect: puts value on top of the stack
        let line_no = expr.span.lo.line_no;
        match &expr.kind {
            ast::ExprKind::Literal(literal) => self.compile_literal(literal, line_no),
            ast::ExprKind::Infix(op, lhs, rhs) => self.compile_infix(*op, lhs, rhs)?,
            ast::ExprKind::Prefix(op, expr) => self.compile_prefix(*op, expr)?,
            ast::ExprKind::Variable(var) => self.get_variable(&var.name, line_no)?,
            ast::ExprKind::Assignment(var, expr) => self.set_variable(&var.name, expr, line_no)?,
            ast::ExprKind::Logical(LogicalOperator::And, lhs, rhs) => self.compile_and(lhs, rhs)?,
            ast::ExprKind::Logical(LogicalOperator::Or, lhs, rhs) => self.compile_or(lhs, rhs)?,
            ast::ExprKind::Call(callee, args) => self.compile_call(callee, args)?,
            ast::ExprKind::Get(expr, name) => {
                let idx = self.add_string_constant(name);
                self.compile_expression(expr)?;
                self.current_chunk()
                    .write_op_with_u8(OpCode::GetProperty, idx, line_no);
            }
            ast::ExprKind::Set(expr, name, value_expr) => {
                let idx = self.add_string_constant(name);
                self.compile_expression(expr)?;
                self.compile_expression(value_expr)?;
                self.current_chunk()
                    .write_op_with_u8(OpCode::SetProperty, idx, line_no);
            }
            ast::ExprKind::This(_) => {
                self.get_variable(THIS_STR, line_no)?;
            }
            ast::ExprKind::Super(_, method_name) => {
                self.get_variable(THIS_STR, line_no)?;
                self.get_variable(SUPER_STR, line_no)?;

                let idx = self.add_string_constant(method_name);
                self.current_chunk()
                    .write_op_with_u8(OpCode::GetSuper, idx, line_no);
            }
        };

        Ok(())
    }

    fn compile_literal(&mut self, literal: &ast::Literal, line_no: usize) {
        match literal {
            ast::Literal::Number(n) => {
                let value = ChunkConstant::Number(*n);
                let idx = self.add_constant(value);
                self.current_chunk()
                    .write_op_with_u8(OpCode::Constant, idx, line_no);
            }
            ast::Literal::Boolean(b) => {
                let opcode = if *b { OpCode::True } else { OpCode::False };
                self.current_chunk().write_op(opcode, line_no);
            }
            ast::Literal::Str(s) => {
                let value = ChunkConstant::String(self.string_table.get_interned(s));
                let idx = self.add_constant(value);
                self.current_chunk()
                    .write_op_with_u8(OpCode::Constant, idx, line_no);
            }
            ast::Literal::Nil => {
                self.current_chunk().write_op(OpCode::Nil, line_no);
            }
        }
    }

    fn compile_infix(
        &mut self,
        op: InfixOperator,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
    ) -> CompilerResult<()> {
        let line_no = lhs.span.hi.line_no; // I guess??

        self.compile_expression(lhs)?;
        self.compile_expression(rhs)?;

        let chunk = self.current_chunk();
        match op {
            InfixOperator::Add => chunk.write_op(OpCode::Add, line_no),
            InfixOperator::Subtract => chunk.write_op(OpCode::Subtract, line_no),
            InfixOperator::Multiply => chunk.write_op(OpCode::Multiply, line_no),
            InfixOperator::Divide => chunk.write_op(OpCode::Divide, line_no),
            InfixOperator::EqualTo => chunk.write_op(OpCode::Equal, line_no),
            InfixOperator::NotEqualTo => {
                chunk.write_op(OpCode::Equal, line_no);
                chunk.write_op(OpCode::Not, line_no)
            }
            InfixOperator::GreaterThan => chunk.write_op(OpCode::GreaterThan, line_no),
            InfixOperator::GreaterEq => {
                chunk.write_op(OpCode::LessThan, line_no);
                chunk.write_op(OpCode::Not, line_no)
            }
            InfixOperator::LessThan => chunk.write_op(OpCode::LessThan, line_no),
            InfixOperator::LessEq => {
                chunk.write_op(OpCode::GreaterThan, line_no);
                chunk.write_op(OpCode::Not, line_no)
            }
        };

        Ok(())
    }

    fn compile_prefix(&mut self, op: PrefixOperator, expr: &ast::Expr) -> CompilerResult<()> {
        self.compile_expression(expr)?;
        let opcode = match op {
            PrefixOperator::Negate => OpCode::Negate,
            PrefixOperator::LogicalNot => OpCode::Not,
        };

        self.current_chunk().write_op(opcode, expr.span.lo.line_no);
        Ok(())
    }

    fn compile_and(&mut self, lhs: &ast::Expr, rhs: &ast::Expr) -> CompilerResult<()> {
        self.compile_expression(lhs)?;

        // lhs is now on the stack; if it's falsey, keep it. otherwise, try the rhs.
        let jump = self
            .current_chunk()
            .emit_jump(OpCode::JumpIfFalse, lhs.span.lo.line_no);
        self.current_chunk()
            .write_op(OpCode::Pop, rhs.span.lo.line_no);
        self.compile_expression(rhs)?;
        self.current_chunk().patch_jump(jump);

        Ok(())
    }

    fn compile_or(&mut self, lhs: &ast::Expr, rhs: &ast::Expr) -> CompilerResult<()> {
        self.compile_expression(lhs)?;

        // lhs is now on the stack. if it's true, we want to keep it.
        let skip_jump = self
            .current_chunk()
            .emit_jump(OpCode::JumpIfFalse, lhs.span.lo.line_no);
        let jump_for_true = self
            .current_chunk()
            .emit_jump(OpCode::Jump, lhs.span.lo.line_no);
        self.current_chunk().patch_jump(skip_jump);

        self.current_chunk()
            .write_op(OpCode::Pop, rhs.span.lo.line_no);
        self.compile_expression(rhs)?;
        self.current_chunk().patch_jump(jump_for_true);

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
                let idx = self.add_string_constant(method_name);

                self.current_chunk().write_op(OpCode::Invoke, line_no);
                self.current_chunk().write_u8(idx, line_no);
                self.current_chunk().write_u8(num_args, line_no);
            }
            ast::ExprKind::Super(_, method_name) => {
                // We put the reciever on the stack, then all the arguments, then the superclass
                let line_no = callee.span.lo.line_no;
                self.get_variable(THIS_STR, line_no)?;
                for arg in args.iter() {
                    self.compile_expression(arg)?;
                }
                self.get_variable(SUPER_STR, line_no)?;

                // Then we emit the OP_SUPER_INVOKE
                let line_no = callee.span.hi.line_no;
                let idx = self.add_string_constant(method_name);

                self.current_chunk().write_op(OpCode::SuperInvoke, line_no);
                self.current_chunk().write_u8(idx, line_no);
                self.current_chunk().write_u8(num_args, line_no);
            }
            _ => {
                // Normal path: put the callee and its args on the stack, in order
                self.compile_expression(callee)?;
                for arg in args.iter() {
                    self.compile_expression(arg)?;
                }
                self.current_chunk().write_op_with_u8(
                    OpCode::Call,
                    num_args,
                    callee.span.hi.line_no,
                );
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
            let global_idx = self.add_string_constant(name);
            self.current_chunk()
                .write_op_with_u8(OpCode::DefineGlobal, global_idx, line_no);
        } else {
            self.get_ctx_mut().mark_last_local_initialized();
        }

        Ok(())
    }

    fn get_variable(&mut self, var_name: &str, line_no: usize) -> CompilerResult<()> {
        match self.resolve_variable(var_name)? {
            VariableLocator::Global => {
                // Stash the name in the chunk constants, and emit a GetGlobal instruction.
                let global_idx = self.add_string_constant(var_name);
                self.current_chunk()
                    .write_op_with_u8(OpCode::GetGlobal, global_idx, line_no);
            }
            VariableLocator::Local(idx) => {
                self.current_chunk()
                    .write_op_with_u8(OpCode::GetLocal, idx, line_no);
            }
            VariableLocator::Upvalue(idx) => {
                self.current_chunk()
                    .write_op_with_u8(OpCode::GetUpvalue, idx, line_no);
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
                let global_idx = self.add_string_constant(var_name);
                self.current_chunk()
                    .write_op_with_u8(OpCode::SetGlobal, global_idx, line_no);
            }
            VariableLocator::Local(idx) => {
                self.current_chunk()
                    .write_op_with_u8(OpCode::SetLocal, idx, line_no);
            }
            VariableLocator::Upvalue(idx) => {
                self.current_chunk()
                    .write_op_with_u8(OpCode::SetUpvalue, idx, line_no);
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

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.get_ctx_mut().chunk
    }

    fn add_constant(&mut self, constant: ChunkConstant) -> ConstantIdx {
        self.current_chunk().add_constant(constant)
    }

    fn add_string_constant(&mut self, string: &str) -> ConstantIdx {
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
                state.chunk.write_op(OpCode::CloseUpvalue, 0);
            } else {
                state.chunk.write_op(OpCode::Pop, 0); // TODO line no
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
            self.context_stack[root_idx + 1].add_upvalue(Upvalue::Immediate(local_idx))?;
        for context in self.context_stack[root_idx + 2..].iter_mut() {
            upvalue_idx = context.add_upvalue(Upvalue::Recursive(upvalue_idx))?;
        }

        // If we made it here, it means this variable is local to some enclosing scope,
        // so it's located in our upvalue array.
        Ok(VariableLocator::Upvalue(upvalue_idx))
    }

    fn emit_implicit_return_arg(&mut self, line_no: usize) {
        // this is pulled out into its own method so it can be called in two spots
        if self.get_ctx().is_initializer {
            self.current_chunk()
                .write_op_with_u8(OpCode::GetLocal, 0, line_no);
        } else {
            self.current_chunk().write_op(OpCode::Nil, line_no);
        }
    }

    #[allow(unused_variables)]
    fn print_chunk(&self, name: &str, chunk: &Chunk) {
        #[cfg(feature = "print-chunks")]
        chunk.disassemble(name);
    }
}
