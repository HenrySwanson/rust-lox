use super::span::CodePosition;
use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug, Clone)]
pub struct Cursor<'src> {
    char_iterator: Peekable<CharIndices<'src>>,
    position: CodePosition,
}

impl<'src> Cursor<'src> {
    /// Creates a character stream for the given source string
    pub fn new(source: &'src str) -> Self {
        Cursor {
            char_iterator: source.char_indices().peekable(),
            position: CodePosition::new(1, 1),
        }
    }

    /// Returns the position of the cursor
    pub fn get_position(&self) -> CodePosition {
        self.position
    }

    /// Peeks at the next character without consuming it. Returns the
    /// byte-position and the value of the character.
    pub fn peek(&mut self) -> Option<(usize, char)> {
        self.char_iterator.peek().copied()
    }

    pub fn peek_next(&mut self) -> Option<(usize, char)> {
        // TODO this could be non-mut. Strange for peek to be mut and peek_next not to be though...
        let mut cl = self.clone();
        cl.take();
        cl.peek()
    }

    /// Consumes the next character, returning the byte-position and the value
    /// of the character.
    pub fn take(&mut self) -> Option<(usize, char)> {
        let (byte_idx, ch) = match self.char_iterator.next() {
            None => return None,
            Some(t) => t,
        };

        // TODO how does this work for multi-byte chars?
        // Advance the peek position
        if ch == '\n' {
            self.position.line_no += 1;
            self.position.column_no = 1;
        } else {
            self.position.column_no += 1;
        }

        Some((byte_idx, ch))
    }

    /// Consumes the next character only if it matches the given character,
    /// in which case it returns true. Otherwise returns false.
    pub fn take_if(&mut self, expected: char) -> bool {
        match self.peek() {
            None => false,
            Some((_, ch)) if ch != expected => false,
            _ => {
                self.take();
                true
            }
        }
    }

    /// Advances the iterator until the condition is false, or the string ends.
    /// The next character (if any) will not satisfy the condition.
    pub fn take_while<F>(&mut self, condition: F)
    where
        F: Fn(char) -> bool,
    {
        loop {
            match self.peek() {
                Some((_, ch)) if condition(ch) => {
                    self.take();
                }
                _ => break,
            }
        }
    }

    /// Advances the iterator until the condition is true, or the string ends.
    /// The next character (if any) will satisfy the condition.
    pub fn take_until<F>(&mut self, condition: F)
    where
        F: Fn(char) -> bool,
    {
        self.take_while(|ch| !condition(ch));
    }
}
