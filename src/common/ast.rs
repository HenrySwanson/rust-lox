use super::operator::{InfixOperator, LogicalOperator, PrefixOperator};

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    NumberLiteral(u32),
    BooleanLiteral(bool),
    StringLiteral(String),
    NilLiteral,
    Infix(InfixOperator, Box<Expr>, Box<Expr>),
    Prefix(PrefixOperator, Box<Expr>),
    Logical(LogicalOperator, Box<Expr>, Box<Expr>),
    Variable(String),
    Assignment(String, Box<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    VariableDecl(String, Expr),
    Block(Vec<Stmt>),
    IfElse(Expr, Box<Stmt>, Box<Option<Stmt>>),
    While(Expr, Box<Stmt>),
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
            Expr::Variable(name) => name.clone(),
            Expr::Assignment(name, expr) => format!("(set {} {})", name, expr.lispy_string()),
        }
    }
}
