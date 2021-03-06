use std::fmt;

/// Struct for tracking position in a file. Note that:
/// a) line and column numbers are 1-index
/// b) the derived Ord traits work the way you expect
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct CodePosition {
    pub line_no: usize,
    pub column_no: usize,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Span {
    pub lo: CodePosition,
    pub hi: CodePosition,
}

impl CodePosition {
    pub fn new(line_no: usize, column_no: usize) -> Self {
        CodePosition { line_no, column_no }
    }
}

impl fmt::Display for CodePosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line_no, self.column_no)
    }
}

impl Span {
    pub fn new(mut lo: CodePosition, mut hi: CodePosition) -> Self {
        if lo > hi {
            std::mem::swap(&mut lo, &mut hi)
        }
        Span { lo, hi }
    }

    pub fn dummy() -> Self {
        let zero_pos = CodePosition::new(0, 0);
        Span::new(zero_pos, zero_pos)
    }

    pub fn to(&self, other: Self) -> Self {
        Span::new(
            std::cmp::min(self.lo, other.lo),
            std::cmp::max(self.hi, other.hi),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ::more_asserts::*;

    #[test]
    fn code_position_ordering() {
        let a = CodePosition::new(2, 7);
        let b = CodePosition::new(4, 1);
        assert_le!(a, a);
        assert_ge!(a, a);
        assert_lt!(a, b);
    }

    #[test]
    fn unite_spans() {
        // Consider the following file. Capital letters start the span,
        // small letters end them.
        //
        //   1 2 3 4 5 6 7 8 9
        // 1 - - - - - - D - -
        // 2 A - - - - - - - -
        // 3 - - - - - a B - -
        // 4 - - - - - - - - -
        // 5 - b C - - - - - -
        // 6 - - - d - - c - -

        let a = Span::new(CodePosition::new(2, 1), CodePosition::new(3, 6));
        let b = Span::new(CodePosition::new(3, 7), CodePosition::new(5, 2));
        let c = Span::new(CodePosition::new(5, 3), CodePosition::new(6, 7));
        let d = Span::new(CodePosition::new(1, 7), CodePosition::new(6, 4));

        assert_eq!(a.to(b), Span::new(a.lo, b.hi));
        assert_eq!(a.to(c), Span::new(a.lo, c.hi));
        assert_eq!(a.to(d), d);
        assert_eq!(b.to(c), c.to(b));
    }
}
