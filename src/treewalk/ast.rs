use crate::frontend::ast as frontend_ast;
use crate::frontend::span::Span;

// AST Nodes

// Things that are identical to those in the regular AST
pub type Literal = frontend_ast::Literal;
pub type UnaryOperator = frontend_ast::UnaryOperator;
pub type BinaryOperator = frontend_ast::BinaryOperator;
pub type LogicalOperator = frontend_ast::LogicalOperator;

#[derive(Debug, Clone)]
pub struct Tree {
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Expression(Expr),
    Print(Expr),
    VariableDecl(String, Expr),
    Block(Vec<Stmt>),
    IfElse(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    // TODO use Rc for this, so it's cloneable
    FunctionDecl(FunctionDecl),
    Return(Expr),
    ClassDecl(String, Option<VariableRef>, Vec<FunctionDecl>),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Literal(Literal),
    BinOp(BinaryOperator, Box<Expr>, Box<Expr>),
    UnaryOp(UnaryOperator, Box<Expr>),
    Logical(LogicalOperator, Box<Expr>, Box<Expr>),
    Variable(VariableRef),
    Assignment(VariableRef, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Get(Box<Expr>, String),
    Set(Box<Expr>, String, Box<Expr>),
    This(VariableRef),
    Super(VariableRef, String),
}

#[derive(Debug, Clone)]
pub enum VHops {
    // Not found in any enclosing environment, it's either global or not defined.
    Global,
    // How many environments we have to look back; 0 is our current environment,
    // 1 is our parent, etc.
    Local(usize),
}

#[derive(Debug, Clone)]
pub struct VariableRef {
    pub name: String,
    pub hops: VHops, // used by the resolver
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<String>,
    pub body: Box<Stmt>,
}
