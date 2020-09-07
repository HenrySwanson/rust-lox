use super::operator::{InfixOperator, LogicalOperator, PrefixOperator};

use crate::common::span::Span;

// AST Nodes

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StmtKind {
    Expression(Expr),
    Print(Expr),
    VariableDecl(String, Expr),
    Block(Vec<Stmt>),
    IfElse(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    FunctionDecl(FunctionDecl),
    Return(Option<Expr>),
    ClassDecl(String, Option<VariableRef>, Vec<FunctionDecl>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprKind {
    Literal(Literal),
    Infix(InfixOperator, Box<Expr>, Box<Expr>),
    Prefix(PrefixOperator, Box<Expr>),
    Logical(LogicalOperator, Box<Expr>, Box<Expr>),
    Variable(VariableRef),
    Assignment(VariableRef, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Get(Box<Expr>, String),
    Set(Box<Expr>, String, Box<Expr>),
    This(VariableRef),
    Super(VariableRef, String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Literal {
    Number(u32),
    Boolean(bool),
    Str(String),
    Nil,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VariableRef {
    pub name: String,
    pub hops: Option<usize>, // used by the resolver
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<String>,
    pub body: Box<Stmt>,
}

impl Stmt {
    pub fn new(kind: StmtKind, span: Span) -> Self {
        Stmt { kind, span }
    }
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Expr { kind, span }
    }
}

impl Expr {
    /// Returns a pretty-formatted string to show the AST. Uses a Lisp-like format,
    /// with a lot of parentheses.
    #[allow(dead_code)]
    pub fn lispy_string(&self) -> String {
        match &self.kind {
            ExprKind::Literal(lit) => match lit {
                Literal::Number(n) => n.to_string(),
                Literal::Boolean(b) => b.to_string(),
                Literal::Str(s) => format!("\"{}\"", s),
                Literal::Nil => "nil".to_owned(),
            },
            ExprKind::Infix(op, lhs, rhs) => format!(
                "({} {} {})",
                op.symbol(),
                lhs.lispy_string(),
                rhs.lispy_string()
            ),
            ExprKind::Prefix(op, expr) => format!("({} {})", op.symbol(), expr.lispy_string()),
            ExprKind::Logical(op, lhs, rhs) => format!(
                "({} {} {})",
                op.symbol(),
                lhs.lispy_string(),
                rhs.lispy_string()
            ),
            ExprKind::Variable(var) => var.name.clone(),
            ExprKind::Assignment(var, expr) => {
                format!("(set {} {})", var.name, expr.lispy_string())
            }
            ExprKind::Call(callee, args) => {
                let exprs: Vec<_> = args.iter().map(|a| a.lispy_string()).collect();
                format!("(call {} {})", callee.lispy_string(), exprs.join(" "))
            }
            ExprKind::Get(expr, property) => format!("(get {} {})", expr.lispy_string(), property),
            ExprKind::Set(expr, property, value) => format!(
                "(set {} {} {})",
                expr.lispy_string(),
                property,
                value.lispy_string()
            ),
            ExprKind::This(_) => String::from("this"),
            ExprKind::Super(_, method_name) => format!("(super {})", method_name),
        }
    }
}

impl VariableRef {
    pub fn new(name: String) -> Self {
        VariableRef { name, hops: None }
    }
}

impl FunctionDecl {
    pub fn new(name: String, params: Vec<String>, body: Stmt) -> Self {
        FunctionDecl {
            name,
            params,
            body: Box::new(body),
        }
    }
}
