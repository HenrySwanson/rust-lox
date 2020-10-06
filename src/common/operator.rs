use super::token::Token;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Precedence {
    // weakest-binding
    Lowest,
    Assignment,
    LogicalOr,
    LogicalAnd,
    Equality,
    Comparison,
    Addition,
    Multiplication,
    Unary,
    Property,
    Call,
    // tightest-binding
}

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
    pub fn try_from_token(token: &Token) -> Option<PrefixOperator> {
        let op = match token {
            Token::Minus => PrefixOperator::Negate,
            Token::Bang => PrefixOperator::LogicalNot,
            _ => return None,
        };

        Some(op)
    }

    pub fn precedence(&self) -> Precedence {
        match self {
            PrefixOperator::Negate | PrefixOperator::LogicalNot => Precedence::Unary,
        }
    }

    pub fn symbol(&self) -> &str {
        match self {
            PrefixOperator::Negate => "-",
            PrefixOperator::LogicalNot => "!",
        }
    }
}

impl InfixOperator {
    pub fn try_from_token(token: &Token) -> Option<InfixOperator> {
        let op = match token {
            Token::Plus => InfixOperator::Add,
            Token::Minus => InfixOperator::Subtract,
            Token::Asterisk => InfixOperator::Multiply,
            Token::Slash => InfixOperator::Divide,
            Token::DoubleEq => InfixOperator::EqualTo,
            Token::BangEq => InfixOperator::NotEqualTo,
            Token::RightAngle => InfixOperator::GreaterThan,
            Token::RightAngleEq => InfixOperator::GreaterEq,
            Token::LeftAngle => InfixOperator::LessThan,
            Token::LeftAngleEq => InfixOperator::LessEq,
            _ => return None,
        };

        Some(op)
    }

    pub fn precedence(&self) -> Precedence {
        match self {
            InfixOperator::Add | InfixOperator::Subtract => Precedence::Addition,
            InfixOperator::Multiply | InfixOperator::Divide => Precedence::Multiplication,
            InfixOperator::EqualTo | InfixOperator::NotEqualTo => Precedence::Equality,
            InfixOperator::GreaterThan
            | InfixOperator::GreaterEq
            | InfixOperator::LessThan
            | InfixOperator::LessEq => Precedence::Comparison,
        }
    }

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
    pub fn try_from_token(token: &Token) -> Option<LogicalOperator> {
        let op = match token {
            Token::And => LogicalOperator::And,
            Token::Or => LogicalOperator::Or,
            _ => return None,
        };

        Some(op)
    }

    pub fn precedence(&self) -> Precedence {
        match self {
            LogicalOperator::And => Precedence::LogicalAnd,
            LogicalOperator::Or => Precedence::LogicalOr,
        }
    }

    pub fn symbol(&self) -> &str {
        match self {
            LogicalOperator::And => "and",
            LogicalOperator::Or => "or",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ::more_asserts::*;

    #[test]
    fn precedence() {
        assert_lt!(Precedence::Lowest, Precedence::Unary);
        assert_gt!(Precedence::Multiplication, Precedence::Addition);
        assert_gt!(Precedence::Comparison, Precedence::Equality);
    }

    #[test]
    fn operators() {
        assert_eq!(
            InfixOperator::try_from_token(&Token::Plus),
            Some(InfixOperator::Add)
        );
        assert_eq!(
            InfixOperator::try_from_token(&Token::Minus),
            Some(InfixOperator::Subtract)
        );
        assert_eq!(
            PrefixOperator::try_from_token(&Token::Minus),
            Some(PrefixOperator::Negate)
        );

        assert_eq!(InfixOperator::try_from_token(&Token::Bang), None);
        assert_eq!(PrefixOperator::try_from_token(&Token::Asterisk), None);
    }
}
