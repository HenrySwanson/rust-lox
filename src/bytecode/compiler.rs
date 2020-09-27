use crate::common::ast;
use crate::common::operator::{InfixOperator, PrefixOperator};

use super::chunk::{Chunk, ConstantIdx};
use super::opcode::OpCode;
use super::value::Value;
use super::vm::VM;

const DEBUG_PRINT_CODE: bool = true;

pub struct Compiler<'vm> {
    // We need to be able to access the VM so we can stuff objects into
    // into the heap. For example, the string "cat" in `var a = "cat"`.
    vm_ref: &'vm mut VM,
}

// TODO: kill all the panics
impl<'vm> Compiler<'vm> {
    pub fn new(vm_ref: &'vm mut VM) -> Self {
        Compiler { vm_ref }
    }

    pub fn compile(&mut self, stmts: &Vec<ast::Stmt>, chunk: &mut Chunk) {
        for stmt in stmts.iter() {
            self.compile_statement(stmt, chunk);
        }

        // Return, to exit the VM
        chunk.write_instruction(OpCode::Return, 0);
    }

    pub fn compile_statement(&mut self, stmt: &ast::Stmt, chunk: &mut Chunk) {
        let line_no = stmt.span.lo.line_no;
        let expr = match &stmt.kind {
            ast::StmtKind::Expression(expr) => {
                self.compile_expression(expr, chunk);
                chunk.write_instruction(OpCode::Pop, line_no);
            }
            ast::StmtKind::Print(expr) => {
                self.compile_expression(expr, chunk);
                chunk.write_instruction(OpCode::Print, line_no);
            }
            ast::StmtKind::VariableDecl(name, expr) => {
                // We store the name of the global as a string constant
                let global_idx = self.add_constant_string(name, chunk);

                self.compile_expression(expr, chunk);
                chunk.write_instruction(OpCode::DefineGlobal, line_no);
                chunk.write_byte(global_idx, line_no);
                           }
            _ => panic!("Don't know how to compile that statement yet!"),
        };

        if DEBUG_PRINT_CODE {
            chunk.disassemble("code");
        }
    }

    fn compile_expression(&mut self, expr: &ast::Expr, chunk: &mut Chunk) {
        // stack effect: puts value on top of the stack
        let line_no = expr.span.lo.line_no;
        match &expr.kind {
            ast::ExprKind::Literal(literal) => self.compile_literal(literal, line_no, chunk),
            ast::ExprKind::Infix(op, lhs, rhs) => self.compile_infix(*op, lhs, rhs, chunk),
            ast::ExprKind::Prefix(op, expr) => self.compile_prefix(*op, expr, chunk),
            ast::ExprKind::Variable(var) => {
                let global_idx = self.add_constant_string(&var.name, chunk);
                chunk.write_instruction(OpCode::GetGlobal, line_no);
                chunk.write_byte(global_idx, line_no);
            }
            ast::ExprKind::Assignment(var, expr) => {
                // Compile the RHS, then add instructions to pull it onto the stack
                self.compile_expression(expr, chunk);

                let global_idx = self.add_constant_string(&var.name, chunk);
                chunk.write_instruction(OpCode::SetGlobal, line_no);
                chunk.write_byte(global_idx, line_no);
            }
            _ => panic!("Don't know how to compile that expression yet!"),
        }
    }

    fn compile_literal(&mut self, literal: &ast::Literal, line_no: usize, chunk: &mut Chunk) {
        match literal {
            ast::Literal::Number(n) => {
                let value = Value::Number((*n).into());
                let idx = chunk.add_constant(value);
                chunk.write_instruction(OpCode::Constant, line_no);
                chunk.write_byte(idx, line_no);
            }
            ast::Literal::Boolean(b) => {
                let opcode = if *b { OpCode::True } else { OpCode::False };
                chunk.write_instruction(opcode, line_no);
            }
            ast::Literal::Str(s) => {
                let idx = self.add_constant_string(s, chunk);

                chunk.write_instruction(OpCode::Constant, line_no);
                chunk.write_byte(idx, line_no);
            }
            ast::Literal::Nil => {
                chunk.write_instruction(OpCode::Nil, line_no);
            }
        }
    }

    fn compile_infix(
        &mut self,
        op: InfixOperator,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
        chunk: &mut Chunk,
    ) {
        let line_no = lhs.span.hi.line_no; // I guess??

        self.compile_expression(lhs, chunk);
        self.compile_expression(rhs, chunk);

        match op {
            InfixOperator::Add => chunk.write_instruction(OpCode::Add, line_no),
            InfixOperator::Subtract => chunk.write_instruction(OpCode::Subtract, line_no),
            InfixOperator::Multiply => chunk.write_instruction(OpCode::Multiply, line_no),
            InfixOperator::Divide => chunk.write_instruction(OpCode::Divide, line_no),
            InfixOperator::EqualTo => chunk.write_instruction(OpCode::Equal, line_no),
            InfixOperator::NotEqualTo => {
                chunk.write_instruction(OpCode::Equal, line_no);
                chunk.write_instruction(OpCode::Not, line_no)
            }
            InfixOperator::GreaterThan => chunk.write_instruction(OpCode::GreaterThan, line_no),
            InfixOperator::GreaterEq => {
                chunk.write_instruction(OpCode::LessThan, line_no);
                chunk.write_instruction(OpCode::Not, line_no)
            }
            InfixOperator::LessThan => chunk.write_instruction(OpCode::LessThan, line_no),
            InfixOperator::LessEq => {
                chunk.write_instruction(OpCode::GreaterThan, line_no);
                chunk.write_instruction(OpCode::Not, line_no)
            }
        }
    }

    fn compile_prefix(&mut self, op: PrefixOperator, expr: &ast::Expr, chunk: &mut Chunk) {
        self.compile_expression(expr, chunk);
        let opcode = match op {
            PrefixOperator::Negate => OpCode::Negate,
            PrefixOperator::LogicalNot => OpCode::Not,
        };

        chunk.write_instruction(opcode, expr.span.lo.line_no);
    }

    // ---- helpers ----

    fn add_constant_string(&mut self, name: &str, chunk: &mut Chunk) -> ConstantIdx {
                        let name = Value::String(self.vm_ref.intern_string(name));
                chunk.add_constant(name)
    }
}
