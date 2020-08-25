use super::operator::{InfixOperator, LogicalOperator, PrefixOperator};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VariableRef {
    pub name: String,
    pub hops: Option<usize>, // used by the resolver
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionData {
    pub name: String,
    pub params: Vec<String>,
    pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    NumberLiteral(u32),
    BooleanLiteral(bool),
    StringLiteral(String),
    NilLiteral,
    Infix(InfixOperator, Box<Expr>, Box<Expr>),
    Prefix(PrefixOperator, Box<Expr>),
    Logical(LogicalOperator, Box<Expr>, Box<Expr>),
    Variable(VariableRef),
    Assignment(VariableRef, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Get(Box<Expr>, String),
    Set(Box<Expr>, String, Box<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    VariableDecl(String, Expr),
    Block(Vec<Stmt>),
    IfElse(Expr, Box<Stmt>, Box<Option<Stmt>>),
    While(Expr, Box<Stmt>),
    FunctionDecl(FunctionData),
    Return(Expr),
    ClassDecl(String, Vec<FunctionData>),
}

impl VariableRef {
    pub fn new(name: String) -> Self {
        VariableRef { name, hops: None }
    }
}

impl FunctionData {
    pub fn new(name: String, params: Vec<String>, body: Stmt) -> Self {
        FunctionData {
            name,
            params,
            body: Box::new(body),
        }
    }
}

impl Expr {
    /// Returns a pretty-formatted string to show the AST. Uses a Lisp-like format,
    /// with a lot of parentheses.
    pub fn lispy_string(&self) -> String {
        match self {
            Expr::NumberLiteral(n) => n.to_string(),
            Expr::BooleanLiteral(b) => b.to_string(),
            Expr::StringLiteral(s) => format!("\"{}\"", s),
            Expr::NilLiteral => "nil".to_owned(),
            Expr::Infix(op, lhs, rhs) => format!(
                "({} {} {})",
                op.symbol(),
                lhs.lispy_string(),
                rhs.lispy_string()
            ),
            Expr::Prefix(op, expr) => format!("({} {})", op.symbol(), expr.lispy_string()),
            Expr::Logical(op, lhs, rhs) => format!(
                "({} {} {})",
                op.symbol(),
                lhs.lispy_string(),
                rhs.lispy_string()
            ),
            Expr::Variable(var) => var.name.clone(),
            Expr::Assignment(var, expr) => format!("(set {} {})", var.name, expr.lispy_string()),
            Expr::Call(callee, args) => {
                let exprs: Vec<_> = args.iter().map(|a| a.lispy_string()).collect();
                format!("(call {} {})", callee.lispy_string(), exprs.join(" "))
            }
            Expr::Get(expr, property) => format!("(get {} {})", expr.lispy_string(), property),
            Expr::Set(expr, property, value) => format!(
                "(set {} {} {})",
                expr.lispy_string(),
                property,
                value.lispy_string()
            ),
        }
    }
}
