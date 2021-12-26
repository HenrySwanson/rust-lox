use std::fmt;

/// Struct for tracking position in a file. Note that:
/// a) line and column numbers are 1-index
/// b) the derived Ord traits work the way you expect
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct CodePosition {
    pub byte_pos: usize,
    pub line_no: usize,
    pub column_no: usize,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Span {
    pub lo: CodePosition,
    pub hi: CodePosition,
}

impl CodePosition {
    pub fn new(byte_pos: usize, line_no: usize, column_no: usize) -> Self {
        CodePosition {
            byte_pos,
            line_no,
            column_no,
        }
    }

    pub fn from_byte_pos(source: &str, byte_pos: usize) -> Option<Self> {
        let mut line_no = 1;
        let mut col_no = 1;
        for ch in source.get(..byte_pos)?.chars() {
            if ch == '\n' {
                line_no += 1;
                col_no = 1;
            } else {
                col_no += 1;
            }
        }

        Some(CodePosition {
            byte_pos,
            line_no,
            column_no: col_no,
        })
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
        let zero_pos = CodePosition::new(0, 0, 0);
        Span::new(zero_pos, zero_pos)
    }

    pub fn to(&self, other: Self) -> Self {
        Span::new(
            std::cmp::min(self.lo, other.lo),
            std::cmp::max(self.hi, other.hi),
        )
    }

    pub fn extract_string<'a>(&self, source: &'a str) -> Option<&'a str> {
        source.get(self.lo.byte_pos..self.hi.byte_pos)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ::more_asserts::*;

    const SAMPLE_TEXT: &str = "Some example text.\nA second line.\n  Something indented.";

    #[test]
    fn line_and_column() {
        let a = CodePosition::from_byte_pos(SAMPLE_TEXT, 0).unwrap();
        assert_eq!(a, CodePosition::new(0, 1, 1));

        let a = CodePosition::from_byte_pos(SAMPLE_TEXT, 5).unwrap();
        assert_eq!(a, CodePosition::new(5, 1, 6));

        let a = CodePosition::from_byte_pos(SAMPLE_TEXT, 19).unwrap();
        assert_eq!(a, CodePosition::new(19, 2, 1));

        let a = CodePosition::from_byte_pos(SAMPLE_TEXT, 36).unwrap();
        assert_eq!(a, CodePosition::new(36, 3, 3));
    }

    #[test]
    fn extract_string() {
        fn get_span(lo: usize, hi: usize) -> Span {
            Span::new(
                CodePosition::from_byte_pos(SAMPLE_TEXT, lo).unwrap(),
                CodePosition::from_byte_pos(SAMPLE_TEXT, hi).unwrap(),
            )
        }
        assert_eq!(get_span(0, 4).extract_string(SAMPLE_TEXT).unwrap(), "Some");
        assert_eq!(get_span(1, 4).extract_string(SAMPLE_TEXT).unwrap(), "ome");
        assert_eq!(
            get_span(13, 27).extract_string(SAMPLE_TEXT).unwrap(),
            "text.\nA second"
        );
    }

    #[test]
    fn code_position_ordering() {
        let a = CodePosition::new(12, 2, 7);
        let b = CodePosition::new(14, 4, 1);
        assert_le!(a, a);
        assert_ge!(a, a);
        assert_lt!(a, b);
    }
}
