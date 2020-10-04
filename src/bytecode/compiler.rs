use crate::common::ast;
use crate::common::operator::{InfixOperator, LogicalOperator, PrefixOperator};

use super::chunk::{Chunk, ConstantIdx};
use super::errs::{CompilerError, CompilerResult};
use super::opcode::OpCode;
use super::value::Value;
use super::vm::VM;

const DEBUG_PRINT_CODE: bool = true;

// since the GET_LOCAL instruction takes a byte
const MAX_LOCALS: usize = 256;
type LocalIdx = u8;

struct Local {
    name: String,
    scope_depth: u32,
    initialized: bool,
}

pub struct Compiler<'vm> {
    // We need to be able to access the VM so we can stuff objects into
    // into the heap. For example, the string "cat" in `var a = "cat"`.
    vm_ref: &'vm mut VM,
    locals: Vec<Local>,
    current_scope: u32,
}

// TODO: kill all the panics
impl<'vm> Compiler<'vm> {
    pub fn new(vm_ref: &'vm mut VM) -> Self {
        Compiler {
            vm_ref,
            locals: vec![],
            current_scope: 0,
        }
    }

    pub fn compile(&mut self, stmts: &Vec<ast::Stmt>, chunk: &mut Chunk) -> CompilerResult<()> {
        for stmt in stmts.iter() {
            self.compile_statement(stmt, chunk)?;
        }

        // Return, to exit the VM
        chunk.write_op(OpCode::Return, 0);

        if DEBUG_PRINT_CODE {
            chunk.disassemble("code");
        }
        Ok(())
    }

    pub fn compile_statement(&mut self, stmt: &ast::Stmt, chunk: &mut Chunk) -> CompilerResult<()> {
        let line_no = stmt.span.lo.line_no;
        match &stmt.kind {
            ast::StmtKind::Expression(expr) => {
                self.compile_expression(expr, chunk)?;
                chunk.write_op(OpCode::Pop, line_no);
            }
            ast::StmtKind::Print(expr) => {
                self.compile_expression(expr, chunk)?;
                chunk.write_op(OpCode::Print, line_no);
            }
            ast::StmtKind::VariableDecl(name, expr) => {
                self.define_variable(name, expr, chunk, line_no)?;
            }
            ast::StmtKind::Block(stmts) => {
                self.begin_scope();
                for stmt in stmts.iter() {
                    self.compile_statement(stmt, chunk)?;
                }
                self.end_scope(chunk);
            }
            ast::StmtKind::IfElse(condition, if_body, else_body) => {
                self.compile_if_statement(
                    condition,
                    if_body.as_ref(),
                    else_body.as_deref(),
                    chunk,
                )?;
            }
            ast::StmtKind::While(condition, body) => {
                self.compile_while_statement(condition, body.as_ref(), chunk)?;
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
        chunk: &mut Chunk,
    ) -> CompilerResult<()> {
        let line_no = condition.span.lo.line_no;

        self.compile_expression(condition, chunk)?;

        // If we pass the jump, pop the condition and do the if-body
        let jump_to_else = chunk.emit_jump(OpCode::JumpIfFalse, line_no);
        chunk.write_op(OpCode::Pop, line_no);
        self.compile_statement(if_body, chunk)?;
        chunk.patch_jump(jump_to_else);

        // Otherwise, pop the condition now, and do the else body.
        // Note that we always need an else-jump so that we only ever
        // pop once.
        let jump_over_else = chunk.emit_jump(OpCode::Jump, line_no);
        chunk.write_op(OpCode::Pop, line_no);
        if let Some(else_body) = else_body {
            self.compile_statement(else_body, chunk)?;
        }
        chunk.patch_jump(jump_over_else);

        Ok(())
    }

    fn compile_while_statement(
        &mut self,
        condition: &ast::Expr,
        body: &ast::Stmt,
        chunk: &mut Chunk,
    ) -> CompilerResult<()> {
        let line_no = condition.span.lo.line_no;
        let loop_start = chunk.len();

        self.compile_expression(condition, chunk)?;
        let exit_jump = chunk.emit_jump(OpCode::JumpIfFalse, line_no);
        chunk.write_op(OpCode::Pop, line_no);

        self.compile_statement(body, chunk)?;
        chunk.emit_loop(loop_start, line_no);

        chunk.patch_jump(exit_jump);
        chunk.write_op(OpCode::Pop, line_no);

        Ok(())
    }

    fn compile_expression(&mut self, expr: &ast::Expr, chunk: &mut Chunk) -> CompilerResult<()> {
        // stack effect: puts value on top of the stack
        let line_no = expr.span.lo.line_no;
        match &expr.kind {
            ast::ExprKind::Literal(literal) => self.compile_literal(literal, line_no, chunk),
            ast::ExprKind::Infix(op, lhs, rhs) => self.compile_infix(*op, lhs, rhs, chunk)?,
            ast::ExprKind::Prefix(op, expr) => self.compile_prefix(*op, expr, chunk)?,
            ast::ExprKind::Variable(var) => self.get_variable(var, chunk, line_no)?,
            ast::ExprKind::Assignment(var, expr) => self.set_variable(var, expr, chunk, line_no)?,
            ast::ExprKind::Logical(LogicalOperator::And, lhs, rhs) => {
                self.compile_and(lhs, rhs, chunk)?
            }
            ast::ExprKind::Logical(LogicalOperator::Or, lhs, rhs) => {
                self.compile_or(lhs, rhs, chunk)?
            }
            _ => panic!("Don't know how to compile that expression yet!"),
        };

        Ok(())
    }

    fn compile_literal(&mut self, literal: &ast::Literal, line_no: usize, chunk: &mut Chunk) {
        match literal {
            ast::Literal::Number(n) => {
                let value = Value::Number((*n).into());
                let idx = chunk.add_constant(value);
                chunk.write_op_with_u8(OpCode::Constant, idx, line_no);
            }
            ast::Literal::Boolean(b) => {
                let opcode = if *b { OpCode::True } else { OpCode::False };
                chunk.write_op(opcode, line_no);
            }
            ast::Literal::Str(s) => {
                let idx = self.add_constant_string(s, chunk);

                chunk.write_op_with_u8(OpCode::Constant, idx, line_no);
            }
            ast::Literal::Nil => {
                chunk.write_op(OpCode::Nil, line_no);
            }
        }
    }

    fn compile_infix(
        &mut self,
        op: InfixOperator,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
        chunk: &mut Chunk,
    ) -> CompilerResult<()> {
        let line_no = lhs.span.hi.line_no; // I guess??

        self.compile_expression(lhs, chunk)?;
        self.compile_expression(rhs, chunk)?;

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

    fn compile_prefix(
        &mut self,
        op: PrefixOperator,
        expr: &ast::Expr,
        chunk: &mut Chunk,
    ) -> CompilerResult<()> {
        self.compile_expression(expr, chunk)?;
        let opcode = match op {
            PrefixOperator::Negate => OpCode::Negate,
            PrefixOperator::LogicalNot => OpCode::Not,
        };

        chunk.write_op(opcode, expr.span.lo.line_no);
        Ok(())
    }

    fn compile_and(
        &mut self,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
        chunk: &mut Chunk,
    ) -> CompilerResult<()> {
        self.compile_expression(lhs, chunk)?;

        // lhs is now on the stack; if it's falsey, keep it. otherwise, try the rhs.
        let jump = chunk.emit_jump(OpCode::JumpIfFalse, lhs.span.lo.line_no);
        chunk.write_op(OpCode::Pop, rhs.span.lo.line_no);
        self.compile_expression(rhs, chunk)?;
        chunk.patch_jump(jump);

        Ok(())
    }

    fn compile_or(
        &mut self,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
        chunk: &mut Chunk,
    ) -> CompilerResult<()> {
        self.compile_expression(lhs, chunk)?;

        // lhs is now on the stack. if it's true, we want to keep it.
        let skip_jump = chunk.emit_jump(OpCode::JumpIfFalse, lhs.span.lo.line_no);
        let jump_for_true = chunk.emit_jump(OpCode::Jump, lhs.span.lo.line_no);
        chunk.patch_jump(skip_jump);

        chunk.write_op(OpCode::Pop, rhs.span.lo.line_no);
        self.compile_expression(rhs, chunk)?;
        chunk.patch_jump(jump_for_true);

        Ok(())
    }

    fn define_variable(
        &mut self,
        name: &str,
        expr: &ast::Expr,
        chunk: &mut Chunk,
        line_no: usize,
    ) -> CompilerResult<()> {
        // Check if we're defining a global or local variable
        if self.current_scope == 0 {
            // We store the name of the global as a string constant, so the VM can
            // keep track of it.
            let global_idx = self.add_constant_string(name, chunk);

            self.compile_expression(expr, chunk)?;
            chunk.write_op_with_u8(OpCode::DefineGlobal, global_idx, line_no);
        } else {
            // Add the local to the compiler's list, and create instructions
            // to put the expression on the stack.
            self.add_local(name)?;
            self.compile_expression(expr, chunk)?;
            self.locals.last_mut().unwrap().initialized = true;
        }

        Ok(())
    }

    fn get_variable(
        &mut self,
        var: &ast::VariableRef,
        chunk: &mut Chunk,
        line_no: usize,
    ) -> CompilerResult<()> {
        match self.find_local(&var.name)? {
            None => {
                // Global variable; stash the name in the chunk constants, and
                // emit a GetGlobal instruction.
                let global_idx = self.add_constant_string(&var.name, chunk);
                chunk.write_op_with_u8(OpCode::GetGlobal, global_idx, line_no);
            }
            Some(idx) => {
                // Local variable
                chunk.write_op_with_u8(OpCode::GetLocal, idx, line_no);
            }
        }

        Ok(())
    }

    fn set_variable(
        &mut self,
        var: &ast::VariableRef,
        expr: &ast::Expr,
        chunk: &mut Chunk,
        line_no: usize,
    ) -> CompilerResult<()> {
        // In any case, we need to emit instructions to put the result of the
        // expression on the stack.
        self.compile_expression(expr, chunk)?;

        match self.find_local(&var.name)? {
            None => {
                // Global variable; stash the name in the chunk constants, and
                // emit a SetGlobal instruction.
                let global_idx = self.add_constant_string(&var.name, chunk);
                chunk.write_op_with_u8(OpCode::SetGlobal, global_idx, line_no);
            }
            Some(idx) => {
                // Local variable
                chunk.write_op_with_u8(OpCode::SetLocal, idx, line_no);
            }
        }

        Ok(())
    }

    // ---- helpers ----

    fn add_constant_string(&mut self, name: &str, chunk: &mut Chunk) -> ConstantIdx {
        let name = Value::String(self.vm_ref.intern_string(name));
        chunk.add_constant(name)
    }

    fn begin_scope(&mut self) {
        self.current_scope += 1;
    }

    fn end_scope(&mut self, chunk: &mut Chunk) {
        self.current_scope -= 1;

        // Go through the tail of the locals list, and pop off things until
        // we're done with that scope.
        while let Some(local) = self.locals.last() {
            if local.scope_depth > self.current_scope {
                chunk.write_op(OpCode::Pop, 0); // TODO line no
                self.locals.pop();
            } else {
                break;
            }
        }
    }

    fn add_local(&mut self, name: &str) -> CompilerResult<()> {
        if self.locals.len() == MAX_LOCALS {
            return Err(CompilerError::TooManyLocals);
        }

        for local in self.locals.iter().rev() {
            if local.scope_depth == self.current_scope && local.name == name {
                return Err(CompilerError::LocalAlreadyExists(name.to_owned()));
            }
        }

        let new_local = Local {
            name: name.to_owned(),
            scope_depth: self.current_scope,
            initialized: false,
        };
        self.locals.push(new_local);

        Ok(())
    }

    fn find_local(&self, name: &str) -> CompilerResult<Option<LocalIdx>> {
        // Walking the array backwards is more efficient, I suppose
        for (idx, local) in self.locals.iter().enumerate().rev() {
            // note: idx is measured from the _bottom_ of the stack
            if local.name == name {
                if !local.initialized {
                    return Err(CompilerError::LocalUsedInOwnInitializer(name.to_owned()));
                }
                return Ok(Some(idx as LocalIdx));
            }
        }

        // Not found, hopefully it's global.
        return Ok(None);
    }
}
