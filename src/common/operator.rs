// TODO fold into AST

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PrefixOperator {
    Negate,
    LogicalNot,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum InfixOperator {
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

impl PrefixOperator {
    pub fn symbol(&self) -> &str {
        match self {
            PrefixOperator::Negate => "-",
            PrefixOperator::LogicalNot => "!",
        }
    }
}

impl InfixOperator {
    pub fn symbol(&self) -> &str {
        match self {
            InfixOperator::Add => "+",
            InfixOperator::Subtract => "-",
            InfixOperator::Multiply => "*",
            InfixOperator::Divide => "/",
            InfixOperator::EqualTo => "==",
            InfixOperator::NotEqualTo => "!=",
            InfixOperator::GreaterThan => ">",
            InfixOperator::GreaterEq => ">=",
            InfixOperator::LessThan => "<",
            InfixOperator::LessEq => "<=",
        }
    }
}

impl LogicalOperator {
    pub fn symbol(&self) -> &str {
        match self {
            LogicalOperator::And => "and",
            LogicalOperator::Or => "or",
        }
    }
}
