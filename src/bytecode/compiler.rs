use std::convert::{TryFrom, TryInto};
use std::rc::Rc;

use crate::common::ast;
use crate::common::operator::{InfixOperator, LogicalOperator, PrefixOperator};

use super::chunk::{Chunk, ChunkConstant};
use super::errs::{CompilerError, CompilerResult};
use super::opcode::OpCode;
use super::string_interning::StringInterner;

// since the GET_LOCAL instruction takes a byte
const MAX_LOCALS: usize = 256;
type LocalIdx = u8;

const MAX_UPVALUES: usize = 256;
type UpvalueIdx = u8;

struct Local {
    name: String,
    scope_depth: u32,
    initialized: bool,
    captured: bool,
}

// TODO i feel like this shouldn't be exposed, but it's part of the chunk
// data, so I guess it's unavoidable :\
#[derive(Clone, PartialEq, Eq)]
pub enum Upvalue {
    // A local defined in the immediately enclosing scope
    Immediate(LocalIdx),
    // An upvalue belonging to the immediately enclosing scope
    Recursive(UpvalueIdx),
}

enum VariableLocator {
    Local(LocalIdx),
    Upvalue(UpvalueIdx),
    Global,
}

struct Context {
    chunk: Chunk,
    locals: Vec<Local>,
    upvalues: Vec<Upvalue>,
    scope_depth: u32,
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
            initialized: false, // doesn't matter
            captured: false,
        };
        Context {
            chunk: Chunk::new(),
            locals: vec![reserved_local],
            upvalues: vec![],
            scope_depth: 0,
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

// TODO: kill all the panics
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
                self.compile_function_decl(fn_decl, line_no)?;
            }
            ast::StmtKind::Return(expr) => {
                match expr {
                    Some(expr) => {
                        self.compile_expression(expr)?;
                    }
                    None => self.current_chunk().write_op(OpCode::Nil, line_no),
                }
                self.current_chunk().write_op(OpCode::Return, line_no);
            }
            _ => panic!("Don't know how to compile that statement yet!"),
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
    ) -> CompilerResult<()> {
        // Functions are allowed to refer to themselves; create a local with the
        // appropriate name before actually compiling the function.
        self.declare_variable(&fn_decl.name)?;
        if self.get_ctx().scope_depth != 0 {
            self.get_ctx_mut().mark_last_local_initialized();
        }

        // Create the new compiler context
        self.context_stack.push(Context::new(""));
        self.begin_scope();

        // Declare+define the arguments
        for param in fn_decl.params.iter() {
            // Unlike normal local variables, we don't compile any expressions here
            self.declare_variable(param)?;
            self.define_variable(param, line_no)?;
        }

        // Compile the function body, and add an implicit return
        self.compile_statement(fn_decl.body.as_ref())?;
        self.current_chunk()
            .write_op(OpCode::Nil, fn_decl.body.span.hi.line_no);
        self.current_chunk()
            .write_op(OpCode::Return, fn_decl.body.span.hi.line_no);

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
        let idx = self.current_chunk().add_constant(fn_template);
        self.current_chunk()
            .write_op_with_u8(OpCode::MakeClosure, idx, line_no);

        // Now encode all the upvalues that this closure should have
        for upvalue in ctx.upvalues.iter().cloned() {
            let bytes = match upvalue {
                Upvalue::Immediate(idx) => [1, idx],
                Upvalue::Recursive(idx) => [0, idx],
            };
            self.current_chunk().write_u8(bytes[0], line_no);
            self.current_chunk().write_u8(bytes[1], line_no);
        }

        // Finally, define the function variable
        self.define_variable(&fn_decl.name, line_no)?;

        Ok(())
    }

    fn compile_expression(&mut self, expr: &ast::Expr) -> CompilerResult<()> {
        // stack effect: puts value on top of the stack
        let line_no = expr.span.lo.line_no;
        match &expr.kind {
            ast::ExprKind::Literal(literal) => self.compile_literal(literal, line_no),
            ast::ExprKind::Infix(op, lhs, rhs) => self.compile_infix(*op, lhs, rhs)?,
            ast::ExprKind::Prefix(op, expr) => self.compile_prefix(*op, expr)?,
            ast::ExprKind::Variable(var) => self.get_variable(var, line_no)?,
            ast::ExprKind::Assignment(var, expr) => self.set_variable(var, expr, line_no)?,
            ast::ExprKind::Logical(LogicalOperator::And, lhs, rhs) => self.compile_and(lhs, rhs)?,
            ast::ExprKind::Logical(LogicalOperator::Or, lhs, rhs) => self.compile_or(lhs, rhs)?,
            ast::ExprKind::Call(callee, args) => {
                // Put the callee and its args on the stack, in order
                self.compile_expression(callee.as_ref())?;
                for arg in args.iter() {
                    self.compile_expression(arg)?;
                }
                self.current_chunk().write_op_with_u8(
                    OpCode::Call,
                    u8::try_from(args.len()).expect("Too many arguments in AST"),
                    line_no,
                );
            }
            _ => panic!("Don't know how to compile that expression yet!"),
        };

        Ok(())
    }

    fn compile_literal(&mut self, literal: &ast::Literal, line_no: usize) {
        match literal {
            ast::Literal::Number(n) => {
                let value = ChunkConstant::Number(*n);
                let idx = self.current_chunk().add_constant(value);
                self.current_chunk()
                    .write_op_with_u8(OpCode::Constant, idx, line_no);
            }
            ast::Literal::Boolean(b) => {
                let opcode = if *b { OpCode::True } else { OpCode::False };
                self.current_chunk().write_op(opcode, line_no);
            }
            ast::Literal::Str(s) => {
                let value = ChunkConstant::String(self.string_table.get_interned(s));
                let idx = self.current_chunk().add_constant(value);
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
            let value = ChunkConstant::String(self.string_table.get_interned(name));
            let global_idx = self.current_chunk().add_constant(value);
            self.current_chunk()
                .write_op_with_u8(OpCode::DefineGlobal, global_idx, line_no);
        } else {
            self.get_ctx_mut().mark_last_local_initialized();
        }

        Ok(())
    }

    fn get_variable(&mut self, var: &ast::VariableRef, line_no: usize) -> CompilerResult<()> {
        match self.resolve_variable(&var.name)? {
            VariableLocator::Global => {
                // Stash the name in the chunk constants, and emit a GetGlobal instruction.
                let value = ChunkConstant::String(self.string_table.get_interned(&var.name));
                let global_idx = self.current_chunk().add_constant(value);
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
        var: &ast::VariableRef,
        expr: &ast::Expr,
        line_no: usize,
    ) -> CompilerResult<()> {
        // In any case, we need to emit instructions to put the result of the
        // expression on the stack.
        self.compile_expression(expr)?;

        match self.resolve_variable(&var.name)? {
            VariableLocator::Global => {
                // Stash the name in the chunk constants, and emit a SetGlobal instruction.
                let value = ChunkConstant::String(self.string_table.get_interned(&var.name));
                let global_idx = self.current_chunk().add_constant(value);
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

    #[allow(unused_variables)]
    fn print_chunk(&self, name: &str, chunk: &Chunk) {
        #[cfg(feature = "print-chunks")]
        chunk.disassemble(name);
    }
}
