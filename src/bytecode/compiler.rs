use crate::common::ast;
use crate::common::operator::{InfixOperator, PrefixOperator};

use super::chunk::Chunk;
use super::opcode::OpCode;

const DEBUG_PRINT_CODE: bool = true;

pub struct Compiler {}

// TODO: kill all the panics
impl Compiler {
    pub fn compile(stmt: &ast::Stmt, chunk: &mut Chunk) {
        let expr = match &stmt.kind {
            ast::StmtKind::Expression(expr) => expr,
            _ => panic!("Don't know how to compile that statement yet!"),
        };

        Self::compile_expression(expr, chunk);
        chunk.write_instruction(OpCode::Return, 0);

        if DEBUG_PRINT_CODE {
            chunk.disassemble("code");
        }
    }

    fn compile_expression(expr: &ast::Expr, chunk: &mut Chunk) {
        let line_no = expr.span.lo.line_no;
        match &expr.kind {
            ast::ExprKind::Literal(literal) => Self::compile_literal(literal, line_no, chunk),
            ast::ExprKind::Infix(op, lhs, rhs) => Self::compile_infix(*op, lhs, rhs, chunk),
            ast::ExprKind::Prefix(op, expr) => Self::compile_prefix(*op, expr, chunk),
            _ => panic!("Don't know how to compile that expression yet!"),
        }
    }

    fn compile_literal(literal: &ast::Literal, line_no: usize, chunk: &mut Chunk) {
        match literal {
            ast::Literal::Number(n) => {
                let c = chunk.add_constant(i64::from(*n));
                chunk.write_instruction(OpCode::Constant, line_no);
                chunk.write_byte(c, line_no);
            }
            _ => panic!("Don't know how to compile that literal yet!"),
        }
    }

    fn compile_infix(op: InfixOperator, lhs: &ast::Expr, rhs: &ast::Expr, chunk: &mut Chunk) {
        Self::compile_expression(lhs, chunk);
        Self::compile_expression(rhs, chunk);
        let opcode = match op {
            InfixOperator::Add => OpCode::Add,
            InfixOperator::Subtract => OpCode::Subtract,
            InfixOperator::Multiply => OpCode::Multiply,
            InfixOperator::Divide => OpCode::Divide,
            _ => panic!("Don't know how to compile that infix op yet!"),
        };

        chunk.write_instruction(opcode, lhs.span.hi.line_no);
    }

    fn compile_prefix(op: PrefixOperator, expr: &ast::Expr, chunk: &mut Chunk) {
        Self::compile_expression(expr, chunk);
        let opcode = match op {
            PrefixOperator::Negate => OpCode::Negate,
            _ => panic!("Don't know how to compile that prefix op yet!"),
        };

        chunk.write_instruction(opcode, expr.span.lo.line_no);
    }
}
