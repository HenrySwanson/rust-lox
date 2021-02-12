use super::ast::{BinaryOperator, LogicalOperator};
use super::token::Token;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum InfixOperator {
    Arithequal(BinaryOperator),
    Logical(LogicalOperator),
    Assignment,
    Call,
    Property,
}

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

#[derive(Debug, Clone, Copy)]
pub enum Associativity {
    Left,
    Right,
}

impl InfixOperator {
    pub fn try_from_token(token: &Token) -> Option<InfixOperator> {
        // these fns are extracted so we don't have this behemoth everywhere:
        // `Some(InfixOperator::Arithequal(BinaryOperator::XXX))`
        if let Some(op) = Self::try_arithequal(token) {
            return Some(InfixOperator::Arithequal(op));
        }

        if let Some(op) = Self::try_logical(token) {
            return Some(InfixOperator::Logical(op));
        }

        match token {
            Token::Equals => Some(InfixOperator::Assignment),
            Token::LeftParen => Some(InfixOperator::Call),
            Token::Dot => Some(InfixOperator::Property),
            _ => None,
        }
    }

    fn try_arithequal(token: &Token) -> Option<BinaryOperator> {
        let infix_op = match token {
            Token::Plus => BinaryOperator::Add,
            Token::Minus => BinaryOperator::Subtract,
            Token::Asterisk => BinaryOperator::Multiply,
            Token::Slash => BinaryOperator::Divide,
            Token::DoubleEq => BinaryOperator::EqualTo,
            Token::BangEq => BinaryOperator::NotEqualTo,
            Token::RightAngle => BinaryOperator::GreaterThan,
            Token::RightAngleEq => BinaryOperator::GreaterEq,
            Token::LeftAngle => BinaryOperator::LessThan,
            Token::LeftAngleEq => BinaryOperator::LessEq,
            _ => return None,
        };
        Some(infix_op)
    }

    fn try_logical(token: &Token) -> Option<LogicalOperator> {
        let logical_op = match token {
            Token::And => LogicalOperator::And,
            Token::Or => LogicalOperator::Or,
            _ => return None,
        };
        Some(logical_op)
    }

    pub fn exceeds(&self, min_precedence: Precedence) -> bool {
        use std::cmp::Ordering;
        match self.precedence().cmp(&min_precedence) {
            // This is easy if the precedences are different, otherwise we
            // have to check associativity.
            Ordering::Greater => true,
            Ordering::Less => false,
            Ordering::Equal => match self.associativity() {
                Associativity::Left => false, // (A ... B) o C
                Associativity::Right => true, //  A ... (B o C)
            },
        }
    }

    pub fn precedence(&self) -> Precedence {
        match self {
            InfixOperator::Arithequal(op) => match op {
                // Arithmetic
                BinaryOperator::Add | BinaryOperator::Subtract => Precedence::Addition,
                BinaryOperator::Multiply | BinaryOperator::Divide => Precedence::Multiplication,
                // Comparison
                BinaryOperator::EqualTo | BinaryOperator::NotEqualTo => Precedence::Equality,
                BinaryOperator::GreaterThan
                | BinaryOperator::GreaterEq
                | BinaryOperator::LessThan
                | BinaryOperator::LessEq => Precedence::Comparison,
            },
            InfixOperator::Logical(op) => match op {
                LogicalOperator::And => Precedence::LogicalAnd,
                LogicalOperator::Or => Precedence::LogicalOr,
            },
            InfixOperator::Assignment => Precedence::Assignment,
            InfixOperator::Call => Precedence::Call,
            InfixOperator::Property => Precedence::Property,
        }
    }

    pub fn associativity(&self) -> Associativity {
        // A precedence level must have the same associativity type for all
        // its operators, so this implementation enforces that.
        self.precedence().associativity()
    }
}

impl Precedence {
    fn associativity(&self) -> Associativity {
        match self {
            Precedence::Lowest => unreachable!(),
            Precedence::Assignment => Associativity::Right,
            Precedence::LogicalOr => Associativity::Left,
            Precedence::LogicalAnd => Associativity::Left,
            Precedence::Equality => Associativity::Left,
            Precedence::Comparison => Associativity::Left,
            Precedence::Addition => Associativity::Left,
            Precedence::Multiplication => Associativity::Left,
            Precedence::Unary => unreachable!(),
            Precedence::Property => Associativity::Left,
            Precedence::Call => Associativity::Left,
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
            Some(InfixOperator::Arithequal(BinaryOperator::Add))
        );
        assert_eq!(
            InfixOperator::try_from_token(&Token::Minus),
            Some(InfixOperator::Arithequal(BinaryOperator::Subtract))
        );
        assert_eq!(
            InfixOperator::try_from_token(&Token::And),
            Some(InfixOperator::Logical(LogicalOperator::And))
        );

        assert_eq!(InfixOperator::try_from_token(&Token::Bang), None);
    }
}
