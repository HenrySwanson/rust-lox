use super::span::Span;

// AST Nodes

// TODO: add declaration: variable/class/fn decl, or regular statement

#[derive(Debug)]
pub struct Tree {
    pub statements: Vec<Stmt>,
}

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
    ClassDecl(String, Option<String>, Vec<FunctionDecl>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprKind {
    Literal(Literal),
    BinOp(BinaryOperator, Box<Expr>, Box<Expr>),
    UnaryOp(UnaryOperator, Box<Expr>),
    Logical(LogicalOperator, Box<Expr>, Box<Expr>),
    Variable(String),
    Assignment(String, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Get(Box<Expr>, String),
    Set(Box<Expr>, String, Box<Expr>),
    This,
    Super(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Literal {
    Number(u32),
    Boolean(bool),
    Str(String),
    Nil,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<String>,
    pub body: Box<Stmt>,
}
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UnaryOperator {
    Negate,
    LogicalNot,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    EqualTo,
    NotEqualTo,
    GreaterThan,
    GreaterEq,
    LessThan,
    LessEq,
}

// these are different because they short-circuit
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LogicalOperator {
    And,
    Or,
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
            ExprKind::BinOp(op, lhs, rhs) => format!(
                "({} {} {})",
                op.symbol(),
                lhs.lispy_string(),
                rhs.lispy_string()
            ),
            ExprKind::UnaryOp(op, expr) => format!("({} {})", op.symbol(), expr.lispy_string()),
            ExprKind::Logical(op, lhs, rhs) => format!(
                "({} {} {})",
                op.symbol(),
                lhs.lispy_string(),
                rhs.lispy_string()
            ),
            ExprKind::Variable(var) => var.clone(),
            ExprKind::Assignment(var, expr) => {
                format!("(set {} {})", var, expr.lispy_string())
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
            ExprKind::This => String::from("this"),
            ExprKind::Super(method_name) => format!("(super {})", method_name),
        }
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

impl UnaryOperator {
    fn symbol(&self) -> &str {
        match self {
            UnaryOperator::Negate => "-",
            UnaryOperator::LogicalNot => "!",
        }
    }
}

impl BinaryOperator {
    fn symbol(&self) -> &str {
        match self {
            BinaryOperator::Add => "+",
            BinaryOperator::Subtract => "-",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
            BinaryOperator::EqualTo => "==",
            BinaryOperator::NotEqualTo => "!=",
            BinaryOperator::GreaterThan => ">",
            BinaryOperator::GreaterEq => ">=",
            BinaryOperator::LessThan => "<",
            BinaryOperator::LessEq => "<=",
        }
    }
}

impl LogicalOperator {
    fn symbol(&self) -> &str {
        match self {
            LogicalOperator::And => "and",
            LogicalOperator::Or => "or",
        }
    }
}
