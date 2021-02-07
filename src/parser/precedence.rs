use crate::common::operator::{InfixOperator, LogicalOperator};
use crate::common::token::Token;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PrattOperator {
    Arithequal(InfixOperator),
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

impl PrattOperator {
    pub fn try_from_token(token: &Token) -> Option<PrattOperator> {
        // these fns are extracted so we don't have this behemoth everywhere:
        // `Some(PrattOperator::Arithequal(InfixOperator::XXX))`
        if let Some(op) = Self::try_arithequal(token) {
            return Some(PrattOperator::Arithequal(op));
        }

        if let Some(op) = Self::try_logical(token) {
            return Some(PrattOperator::Logical(op));
        }

        match token {
            Token::Equals => Some(PrattOperator::Assignment),
            Token::LeftParen => Some(PrattOperator::Call),
            Token::Dot => Some(PrattOperator::Property),
            _ => None,
        }
    }

    fn try_arithequal(token: &Token) -> Option<InfixOperator> {
        let infix_op = match token {
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
            PrattOperator::Arithequal(op) => match op {
                // Arithmetic
                InfixOperator::Add | InfixOperator::Subtract => Precedence::Addition,
                InfixOperator::Multiply | InfixOperator::Divide => Precedence::Multiplication,
                // Comparison
                InfixOperator::EqualTo | InfixOperator::NotEqualTo => Precedence::Equality,
                InfixOperator::GreaterThan
                | InfixOperator::GreaterEq
                | InfixOperator::LessThan
                | InfixOperator::LessEq => Precedence::Comparison,
            },
            PrattOperator::Logical(op) => match op {
                LogicalOperator::And => Precedence::LogicalAnd,
                LogicalOperator::Or => Precedence::LogicalOr,
            },
            PrattOperator::Assignment => Precedence::Assignment,
            PrattOperator::Call => Precedence::Call,
            PrattOperator::Property => Precedence::Property,
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
            PrattOperator::try_from_token(&Token::Plus),
            Some(PrattOperator::Arithequal(InfixOperator::Add))
        );
        assert_eq!(
            PrattOperator::try_from_token(&Token::Minus),
            Some(PrattOperator::Arithequal(InfixOperator::Subtract))
        );
        assert_eq!(
            PrattOperator::try_from_token(&Token::And),
            Some(PrattOperator::Logical(LogicalOperator::And))
        );

        assert_eq!(PrattOperator::try_from_token(&Token::Bang), None);
    }
}
