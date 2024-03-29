use std::convert::{TryFrom, TryInto};
use std::rc::Rc;

use crate::frontend::{ast, Span};

use super::super::chunk::{Chunk, ChunkConstant};
use super::super::opcode::{
    ConstantIdx, LocalIdx, RichOpcode, UpvalueAddr, UpvalueIdx, MAX_LOCALS, MAX_UPVALUES,
};
use super::super::string_interning::StringInterner;
use super::errs::{CompilerError, CompilerResult};

const THIS_STR: &str = "this";
const SUPER_STR: &str = "super";

// Describes the information of a local variable (a variable declared in
// the current context).
#[derive(Debug)]
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
        self.emit_op(RichOpcode::Return, Span::dummy());

        // Return the chunk defining main()
        let root_state = self.context_stack.pop().expect("Context stack empty!");

        self.print_chunk("<main>", &root_state.chunk); // for debugging

        Ok(root_state.chunk)
    }

    pub fn compile_statement(&mut self, stmt: &ast::Stmt) -> CompilerResult<()> {
        match &stmt.kind {
            ast::StmtKind::Expression(expr) => {
                self.compile_expression(expr)?;
                self.emit_op(RichOpcode::Pop, stmt.span);
            }
            ast::StmtKind::Print(expr) => {
                self.compile_expression(expr)?;
                self.emit_op(RichOpcode::Print, stmt.span);
            }
            ast::StmtKind::VariableDecl(ident, expr) => {
                // Create instructions to put the value of expr on top of the stack
                self.compile_expression(expr)?;

                self.declare_variable(&ident.name)?;
                self.define_variable(ident)?;
            }
            ast::StmtKind::Block(stmts) => {
                self.begin_scope();
                for stmt in stmts.iter() {
                    self.compile_statement(stmt)?;
                }
                self.end_scope();
            }
            ast::StmtKind::IfElse(condition, if_body, else_body) => {
                self.compile_if_statement(
                    condition,
                    if_body.as_ref(),
                    else_body.as_deref(),
                    stmt.span,
                )?;
            }
            ast::StmtKind::While(condition, body) => {
                self.compile_while_statement(condition, body.as_ref(), stmt.span)?;
            }
            ast::StmtKind::For(initializer, condition, increment, body) => {
                self.compile_for_statement(
                    initializer.as_deref(),
                    condition.as_deref(),
                    increment.as_deref(),
                    body.as_ref(),
                    stmt.span,
                )?;
            }
            ast::StmtKind::FunctionDecl(fn_decl) => {
                // Functions are allowed to refer to themselves; create a local with the
                // appropriate name before actually compiling the function.
                self.declare_variable(&fn_decl.ident.name)?;
                if self.get_ctx().scope_depth != 0 {
                    self.get_ctx_mut().mark_last_local_initialized();
                }

                self.compile_function_decl(fn_decl, FunctionType::Function)?;

                // Lastly, define the function variable
                self.define_variable(&fn_decl.ident)?;
            }
            ast::StmtKind::Return(expr) => {
                self.compile_return(expr.as_ref(), stmt.span)?;
            }
            ast::StmtKind::ClassDecl(ident, superclass, methods) => {
                self.declare_variable(&ident.name)?;

                // Store the name as a constant
                let idx = self.add_string_constant(&ident.name)?;
                self.emit_op(RichOpcode::MakeClass(idx), ident.span);
                self.define_variable(ident)?;

                // Push the class context onto the stack
                self.class_stack.push(ClassContext {
                    has_superclass: superclass.is_some(),
                });

                // If there's a superclass, load it in the next local slot.
                // Whether or not there is, the class goes on the stack next.
                if let Some(superclass) = superclass {
                    if superclass.name == ident.name {
                        return Err(CompilerError::SelfInherit(ident.name.clone()));
                    }

                    let synthetic_super = self.synthetic_id(SUPER_STR, stmt.span);

                    self.begin_scope();
                    self.declare_variable(&synthetic_super.name)?;
                    self.define_variable(&synthetic_super)?;

                    self.get_variable(superclass)?;
                    self.get_variable(ident)?;
                    self.emit_op(RichOpcode::Inherit, superclass.span);
                } else {
                    self.get_variable(ident)?;
                }

                // Deal with the methods
                for method in methods.iter() {
                    let fn_type = if method.ident.name == "init" {
                        FunctionType::Initializer
                    } else {
                        FunctionType::Method
                    };
                    // TODO deal with function decl span better
                    self.compile_function_decl(method, fn_type)?;

                    let idx = self.add_string_constant(&method.ident.name)?;
                    self.emit_op(RichOpcode::MakeMethod(idx), method.span);
                }

                // Pop the class off the stack
                // Q: should this be a span for the whole class declaration?
                self.emit_op(RichOpcode::Pop, ident.span);

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
        whole_span: Span,
    ) -> CompilerResult<()> {
        self.compile_expression(condition)?;

        // If we pass the jump, pop the condition and do the if-body
        let jump_to_else = self.emit_jump(RichOpcode::JumpIfFalse(0), whole_span);
        self.emit_op(RichOpcode::Pop, whole_span);
        self.compile_statement(if_body)?;

        // Otherwise, pop the condition now, and do the else body.
        // Note that we always need an else-jump so that we only ever
        // pop once.
        let jump_over_else = self.emit_jump(RichOpcode::Jump(0), whole_span);
        self.patch_jump(jump_to_else)?;
        self.emit_op(RichOpcode::Pop, whole_span);
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
        whole_span: Span,
    ) -> CompilerResult<()> {
        let loop_start = self.current_chunk().len();

        self.compile_expression(condition)?;
        let exit_jump = self.emit_jump(RichOpcode::JumpIfFalse(0), whole_span);
        self.emit_op(RichOpcode::Pop, whole_span);

        self.compile_statement(body)?;
        self.emit_loop(loop_start, whole_span)?;

        self.patch_jump(exit_jump)?;
        self.emit_op(RichOpcode::Pop, whole_span);

        Ok(())
    }

    fn compile_for_statement(
        &mut self,
        initializer: Option<&ast::Stmt>,
        condition: Option<&ast::Expr>,
        increment: Option<&ast::Expr>,
        body: &ast::Stmt,
        whole_span: Span,
    ) -> CompilerResult<()> {
        self.begin_scope();

        // First compile the initializer, if present
        if let Some(initializer) = initializer {
            self.compile_statement(initializer)?;
        }

        // Then compile the while loop
        let loop_start = self.current_chunk().len();
        let exit_jump = match condition {
            Some(condition) => {
                self.compile_expression(condition)?;
                let exit_jump = self.emit_jump(RichOpcode::JumpIfFalse(0), whole_span);
                self.emit_op(RichOpcode::Pop, whole_span);
                Some(exit_jump)
            }
            None => None,
        };

        // Then we compile the body
        self.begin_scope();
        self.compile_statement(body)?;
        self.end_scope();

        // Compile the increment
        if let Some(increment) = increment {
            self.compile_expression(increment)?;
            self.emit_op(RichOpcode::Pop, whole_span);
        }

        // Lastly, compile the jump back to the start of the loop and patch the exit jump
        self.emit_loop(loop_start, whole_span)?;
        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump)?;
        }
        self.emit_op(RichOpcode::Pop, whole_span);

        // This closes the scope that started the initializer. We can only close
        // this scope once all the loop is resolved, because this emits instructions!
        self.end_scope();

        Ok(())
    }

    fn compile_function_decl(
        &mut self,
        fn_decl: &ast::FunctionDecl,
        fn_type: FunctionType,
    ) -> CompilerResult<()> {
        // Create the new compiler context
        let new_context = Context::new(fn_type);

        self.context_stack.push(new_context);
        self.begin_scope();

        // Declare+define the arguments
        for param in fn_decl.params.iter() {
            // Unlike normal local variables, we don't compile any expressions here
            self.declare_variable(&param.name)?;
            self.define_variable(param)?;
        }

        // Compile the function body, and add an implicit return
        for stmt in fn_decl.body.iter() {
            self.compile_statement(stmt)?;
        }
        // TODO: can we emit a span for "end of function?"
        self.compile_return(None, fn_decl.span)?;
        self.emit_op(RichOpcode::Return, fn_decl.span);

        // no need to end scope, we're just killing this compiler context completely
        let ctx = self.context_stack.pop().expect("Context stack empty!");

        self.print_chunk(&fn_decl.ident.name, &ctx.chunk); // for debugging

        // Build the function template and stash it in the chunk
        let fn_template = ChunkConstant::FnTemplate {
            name: self.string_table.get_interned(&fn_decl.ident.name),
            arity: fn_decl.params.len(),
            chunk: Rc::new(ctx.chunk),
            upvalue_count: ctx.upvalues.len(),
        };

        // Unlike regular constants, we load this with MAKE_CLOSURE
        // TODO should i have a separate list for these in the chunk?
        let idx = self.add_constant(fn_template)?;
        self.emit_op(RichOpcode::MakeClosure(idx), fn_decl.span);

        // Now encode all the upvalues that this closure should have
        for upvalue in ctx.upvalues.iter().cloned() {
            self.current_chunk()
                .write_upvalue(upvalue, fn_decl.span.lo.line_no);
        }

        Ok(())
    }

    fn compile_expression(&mut self, expr: &ast::Expr) -> CompilerResult<()> {
        // stack effect: puts value on top of the stack
        match &expr.kind {
            ast::ExprKind::Literal(literal) => self.compile_literal(literal, expr.span)?,
            ast::ExprKind::BinOp(op, lhs, rhs) => self.compile_infix(*op, lhs, rhs, expr.span)?,
            ast::ExprKind::UnaryOp(op, subexpr) => self.compile_prefix(*op, subexpr, expr.span)?,
            ast::ExprKind::Variable(var) => self.get_variable(var)?,
            ast::ExprKind::Assignment(var, subexpr) => self.set_variable(var, subexpr)?,
            ast::ExprKind::Logical(ast::LogicalOperator::And, lhs, rhs) => {
                self.compile_and(lhs, rhs, expr.span)?
            }
            ast::ExprKind::Logical(ast::LogicalOperator::Or, lhs, rhs) => {
                self.compile_or(lhs, rhs, expr.span)?
            }
            ast::ExprKind::Call(callee, args) => self.compile_call(callee, args)?,
            ast::ExprKind::Get(subexpr, property) => {
                let idx = self.add_string_constant(&property.name)?;
                self.compile_expression(subexpr)?;
                self.emit_op(RichOpcode::GetProperty(idx), property.span);
            }
            ast::ExprKind::Set(subexpr, property, value_expr) => {
                let idx = self.add_string_constant(&property.name)?;
                self.compile_expression(subexpr)?;
                self.compile_expression(value_expr)?;
                self.emit_op(RichOpcode::SetProperty(idx), property.span);
            }
            ast::ExprKind::This => {
                if self.class_stack.is_empty() {
                    return Err(CompilerError::ThisOutsideClass);
                }
                let synthetic_this = self.synthetic_id(THIS_STR, expr.span);
                self.get_variable(&synthetic_this)?;
            }
            ast::ExprKind::Super(method_name) => {
                // Check if we are in a class with a superclass
                self.check_super_validity()?;

                let synthetic_this = self.synthetic_id(THIS_STR, expr.span);
                let synthetic_super = self.synthetic_id(SUPER_STR, expr.span);

                self.get_variable(&synthetic_this)?;
                self.get_variable(&synthetic_super)?;

                let idx = self.add_string_constant(&method_name.name)?;
                self.emit_op(RichOpcode::GetSuper(idx), expr.span);
            }
        };

        Ok(())
    }

    fn compile_literal(&mut self, literal: &ast::Literal, span: Span) -> CompilerResult<()> {
        match literal {
            ast::Literal::Number(n) => {
                let value = ChunkConstant::Number(*n);
                let idx = self.add_constant(value)?;
                self.emit_op(RichOpcode::Constant(idx), span);
            }
            ast::Literal::Boolean(b) => {
                let opcode = if *b {
                    RichOpcode::True
                } else {
                    RichOpcode::False
                };
                self.emit_op(opcode, span);
            }
            ast::Literal::Str(s) => {
                let value = ChunkConstant::String(self.string_table.get_interned(s));
                let idx = self.add_constant(value)?;
                self.emit_op(RichOpcode::Constant(idx), span);
            }
            ast::Literal::Nil => {
                self.emit_op(RichOpcode::Nil, span);
            }
        };

        Ok(())
    }

    fn compile_infix(
        &mut self,
        op: ast::BinaryOperator,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
        span: Span,
    ) -> CompilerResult<()> {
        self.compile_expression(lhs)?;
        self.compile_expression(rhs)?;

        match op {
            ast::BinaryOperator::Add => self.emit_op(RichOpcode::Add, span),
            ast::BinaryOperator::Subtract => self.emit_op(RichOpcode::Subtract, span),
            ast::BinaryOperator::Multiply => self.emit_op(RichOpcode::Multiply, span),
            ast::BinaryOperator::Divide => self.emit_op(RichOpcode::Divide, span),
            ast::BinaryOperator::EqualTo => self.emit_op(RichOpcode::Equal, span),
            ast::BinaryOperator::NotEqualTo => {
                self.emit_op(RichOpcode::Equal, span);
                self.emit_op(RichOpcode::Not, span)
            }
            ast::BinaryOperator::GreaterThan => self.emit_op(RichOpcode::GreaterThan, span),
            ast::BinaryOperator::GreaterEq => {
                self.emit_op(RichOpcode::LessThan, span);
                self.emit_op(RichOpcode::Not, span)
            }
            ast::BinaryOperator::LessThan => self.emit_op(RichOpcode::LessThan, span),
            ast::BinaryOperator::LessEq => {
                self.emit_op(RichOpcode::GreaterThan, span);
                self.emit_op(RichOpcode::Not, span)
            }
        };

        Ok(())
    }

    fn compile_prefix(
        &mut self,
        op: ast::UnaryOperator,
        expr: &ast::Expr,
        span: Span,
    ) -> CompilerResult<()> {
        self.compile_expression(expr)?;
        let opcode = match op {
            ast::UnaryOperator::Negate => RichOpcode::Negate,
            ast::UnaryOperator::LogicalNot => RichOpcode::Not,
        };

        self.emit_op(opcode, span);
        Ok(())
    }

    fn compile_and(&mut self, lhs: &ast::Expr, rhs: &ast::Expr, span: Span) -> CompilerResult<()> {
        self.compile_expression(lhs)?;

        // lhs is now on the stack; if it's falsey, keep it. otherwise, try the rhs.
        let jump = self.emit_jump(RichOpcode::JumpIfFalse(0), span);
        self.emit_op(RichOpcode::Pop, span);
        self.compile_expression(rhs)?;
        self.patch_jump(jump)?;

        Ok(())
    }

    fn compile_or(&mut self, lhs: &ast::Expr, rhs: &ast::Expr, span: Span) -> CompilerResult<()> {
        self.compile_expression(lhs)?;

        // lhs is now on the stack. if it's true, we want to keep it.
        let skip_jump = self.emit_jump(RichOpcode::JumpIfFalse(0), span);
        let jump_for_true = self.emit_jump(RichOpcode::Jump(0), span);
        self.patch_jump(skip_jump)?;

        self.emit_op(RichOpcode::Pop, span);
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
                let idx = self.add_string_constant(&method_name.name)?;

                self.emit_op(RichOpcode::Invoke(idx, num_args), callee.span);
            }
            ast::ExprKind::Super(method_name) => {
                self.check_super_validity()?;

                // We put the receiver on the stack, then all the arguments, then the superclass

                let synthetic_this = self.synthetic_id(THIS_STR, callee.span);
                let synthetic_super = self.synthetic_id(SUPER_STR, callee.span);

                self.get_variable(&synthetic_this)?;
                for arg in args.iter() {
                    self.compile_expression(arg)?;
                }
                self.get_variable(&synthetic_super)?;

                // Then we emit the OP_SUPER_INVOKE
                let idx = self.add_string_constant(&method_name.name)?;

                self.emit_op(RichOpcode::SuperInvoke(idx, num_args), callee.span);
            }
            _ => {
                // Normal path: put the callee and its args on the stack, in order
                self.compile_expression(callee)?;
                for arg in args.iter() {
                    self.compile_expression(arg)?;
                }
                self.emit_op(RichOpcode::Call(num_args), callee.span);
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

    fn define_variable(&mut self, id: &ast::Identifier) -> CompilerResult<()> {
        // Check if we're defining a global or local variable
        if self.get_ctx().scope_depth == 0 {
            // We store the name of the global as a string constant, so the VM can
            // keep track of it.
            let global_idx = self.add_string_constant(&id.name)?;
            self.emit_op(RichOpcode::DefineGlobal(global_idx), id.span);
        } else {
            self.get_ctx_mut().mark_last_local_initialized();
        }

        Ok(())
    }

    fn get_variable(&mut self, id: &ast::Identifier) -> CompilerResult<()> {
        match self.resolve_variable(&id.name)? {
            VariableLocator::Global => {
                // Stash the name in the chunk constants, and emit a GetGlobal instruction.
                let global_idx = self.add_string_constant(&id.name)?;
                self.emit_op(RichOpcode::GetGlobal(global_idx), id.span);
            }
            VariableLocator::Local(idx) => {
                self.emit_op(RichOpcode::GetLocal(idx), id.span);
            }
            VariableLocator::Upvalue(idx) => {
                self.emit_op(RichOpcode::GetUpvalue(idx), id.span);
            }
        }

        Ok(())
    }

    fn set_variable(&mut self, id: &ast::Identifier, expr: &ast::Expr) -> CompilerResult<()> {
        // In any case, we need to emit instructions to put the result of the
        // expression on the stack.
        self.compile_expression(expr)?;

        match self.resolve_variable(&id.name)? {
            VariableLocator::Global => {
                // Stash the name in the chunk constants, and emit a SetGlobal instruction.
                let global_idx = self.add_string_constant(&id.name)?;
                self.emit_op(RichOpcode::SetGlobal(global_idx), id.span);
            }
            VariableLocator::Local(idx) => {
                self.emit_op(RichOpcode::SetLocal(idx), id.span);
            }
            VariableLocator::Upvalue(idx) => {
                self.emit_op(RichOpcode::SetUpvalue(idx), id.span);
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

    fn synthetic_id(&self, name: &str, span: Span) -> ast::Identifier {
        ast::Identifier::new(name.to_owned(), span)
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.get_ctx_mut().chunk
    }

    fn add_constant(&mut self, constant: ChunkConstant) -> CompilerResult<ConstantIdx> {
        self.current_chunk()
            .add_constant(constant)
            .ok_or(CompilerError::TooManyConstants)
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

    fn compile_return(&mut self, expr: Option<&ast::Expr>, whole_span: Span) -> CompilerResult<()> {
        // Emit the instructions to put the return value on the stack
        match self.get_ctx().function_type {
            FunctionType::Root => return Err(CompilerError::ReturnAtTopLevel),
            FunctionType::Function | FunctionType::Method => match expr {
                Some(expr) => self.compile_expression(expr)?,
                None => self.emit_op(RichOpcode::Nil, whole_span),
            },
            FunctionType::Initializer => {
                if expr.is_some() {
                    return Err(CompilerError::ReturnInInitializer);
                } else {
                    self.emit_op(RichOpcode::GetLocal(0), whole_span);
                }
            }
        };

        self.emit_op(RichOpcode::Return, whole_span);
        Ok(())
    }

    // chunk-writing methods

    fn emit_op(&mut self, op: RichOpcode, span: Span) {
        // We take the whole span as input, since everyone has a span, but we
        // only store line number for runtime error purposes.
        self.current_chunk().write_op(op, span.lo.line_no);
    }

    fn emit_jump(&mut self, op: RichOpcode, span: Span) -> usize {
        let jmp_idx = self.current_chunk().len();
        self.emit_op(op, span);
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

    fn emit_loop(&mut self, loop_start_idx: usize, span: Span) -> CompilerResult<()> {
        // distance from (instruction after LOOP) to loop_start
        let distance = (self.current_chunk().len() + 3) - loop_start_idx;
        let distance = u16::try_from(distance).map_err(|_| CompilerError::JumpTooLong)?;
        self.emit_op(RichOpcode::Loop(distance), span);
        Ok(())
    }

    #[allow(unused_variables)]
    fn print_chunk(&self, name: &str, chunk: &Chunk) {
        #[cfg(feature = "print-chunks")]
        chunk.disassemble(name);
    }
}
