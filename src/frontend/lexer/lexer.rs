use super::super::span::Span;
use super::cursor::Cursor;
use super::token::{SpannedToken, Token};

pub struct Lexer<'a> {
    source: &'a str,
    cursor: Cursor<'a>,
}

fn is_identifier_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}

fn is_digit_char(ch: char) -> bool {
    ch.is_ascii_digit()
}

impl<'src> Lexer<'src> {
    /// Creates a lexer for the given source string
    pub fn new(source: &'src str) -> Self {
        Lexer {
            source,
            cursor: Cursor::new(source),
        }
    }

    /// Advances through the string, and returns the next token, repeating
    /// EOF if the string is exhausted.
    pub fn next_token(&mut self) -> SpannedToken {
        // This eventually terminates because we hit EOF, at which point lex_token
        // returns Some(Token::EndOfFile).
        loop {
            // Consume whitespace. In theory we could do this in lex_token.
            self.cursor.take_while(|ch| ch.is_ascii_whitespace());

            // Grab the position before lexing the next token.
            let start_pos = self.cursor.get_position();
            let token = self.lex_token();
            let end_pos = self.cursor.get_position();

            if let Some(token) = token {
                return SpannedToken {
                    token,
                    // TODO should span end at end-1? how would that work for col=1?
                    span: Span::new(start_pos, end_pos),
                };
            }
        }
    }

    /// Returns the next meaningful token, or None if the next
    /// token was trivia (just comments for now).
    fn lex_token(&mut self) -> Option<Token> {
        // Read the next character, if any
        let (byte_idx, ch) = match self.cursor.take() {
            Some(tuple) => tuple,
            None => return Some(Token::EndOfFile),
        };

        // Figure out what the next token is
        let token = match ch {
            // Single character stuff
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Asterisk,
            '.' => Token::Dot,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            // Now for the trickier stuff
            '/' => {
                if self.cursor.take_if('/') {
                    // We got a comment, return None
                    self.consume_line();
                    return None;
                } else {
                    Token::Slash
                }
            }
            '=' => self.look_for_eq_sign(Token::Equals, Token::DoubleEq),
            '>' => self.look_for_eq_sign(Token::RightAngle, Token::RightAngleEq),
            '<' => self.look_for_eq_sign(Token::LeftAngle, Token::LeftAngleEq),
            '!' => self.look_for_eq_sign(Token::Bang, Token::BangEq),
            // Everything else
            // Strings start with "
            '"' => self.lex_string(byte_idx),
            // Numbers start with a digit
            _ if is_digit_char(ch) => self.lex_number(byte_idx),
            // This branch would match a digit, but we match digits earlier so it's okay
            _ if is_identifier_char(ch) => self.lex_identifier_or_kw(byte_idx),
            _ => Token::Error(format!("Unexpected character '{}'", ch)),
        };

        Some(token)
    }

    // ---- helpers ----

    /// Reads up to and including the end of the current line.
    fn consume_line(&mut self) {
        self.cursor.take_while(|ch| ch != '\n');
        self.cursor.take(); // consume the newline itself
    }

    /// Checks the next character; if it's an '=', consumes it and returns t2,
    /// otherwise returns t1.
    fn look_for_eq_sign(&mut self, t1: Token, t2: Token) -> Token {
        if self.cursor.take_if('=') {
            t2
        } else {
            t1
        }
    }

    /// Scans up to the next ", and returns a token representing the string consumed.
    /// `start_idx` is the index of the character we previously consumed
    /// (i.e., the first quote).
    fn lex_string(&mut self, start_idx: usize) -> Token {
        // Bump start_idx up by one so that we get the first character
        // of the string, not the quote.
        let start_idx = start_idx + 1;

        self.cursor.take_until(|ch| ch == '"');

        // Did we hit the end of the string?
        match self.cursor.peek() {
            Some((end_idx, '"')) => {
                self.cursor.take(); // consume the closing quote too
                let string = self.source[start_idx..end_idx].to_owned();
                Token::String(string)
            }
            None => Token::Error("Unterminated string".to_owned()),
            _ => unreachable!(),
        }
    }

    /// Scans to the end of the number, and returns a token containing its value.
    fn lex_number(&mut self, start_idx: usize) -> Token {
        self.cursor.take_while(is_digit_char);

        // Check for a fractional part
        if let Some((_, '.')) = self.cursor.peek() {
            if self
                .cursor
                .peek_next()
                .map_or(false, |t| is_digit_char(t.1))
            {
                self.cursor.take();
                self.cursor.take_while(is_digit_char);
            }
        }

        let end_idx = match self.cursor.peek() {
            Some((i, _)) => i,
            None => self.source.len(),
        };

        let slice = &self.source[start_idx..end_idx];
        match slice.parse() {
            Ok(value) => Token::Number(value),
            Err(_) => Token::Error(format!("Unparsable integer `{}`", slice)),
        }
    }

    /// Scans up to the end of the word, and returns the token for it.
    fn lex_identifier_or_kw(&mut self, start_idx: usize) -> Token {
        self.cursor.take_while(is_identifier_char);

        let end_idx = match self.cursor.peek() {
            Some((i, _)) => i,
            None => self.source.len(),
        };

        match &self.source[start_idx..end_idx] {
            "and" => Token::And,
            "class" => Token::Class,
            "else" => Token::Else,
            "false" => Token::False,
            "fun" => Token::Fun,
            "for" => Token::For,
            "if" => Token::If,
            "nil" => Token::Nil,
            "or" => Token::Or,
            "print" => Token::Print,
            "return" => Token::Return,
            "super" => Token::Super,
            "this" => Token::This,
            "true" => Token::True,
            "var" => Token::Var,
            "while" => Token::While,
            other => Token::Identifier(other.to_owned()),
        }
    }

    /// Returns an iterator over all non-EOF tokens.
    pub fn iter(self) -> LexerIterator<'src> {
        LexerIterator { lexer: self }
    }
}

pub struct LexerIterator<'src> {
    lexer: Lexer<'src>,
}

impl<'src> Iterator for LexerIterator<'src> {
    type Item = SpannedToken;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.lexer.next_token();

        if token.token == Token::EndOfFile {
            return None;
        }

        Some(token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(source: &str) -> Vec<SpannedToken> {
        Lexer::new(source).iter().collect()
    }

    #[test]
    fn test_simple_characters() {
        let tokens = lex("() {} + - * / .,;");
        assert_eq!(tokens[0].token, Token::LeftParen);
        assert_eq!(tokens[1].token, Token::RightParen);
        assert_eq!(tokens[2].token, Token::LeftBrace);
        assert_eq!(tokens[3].token, Token::RightBrace);
        assert_eq!(tokens[4].token, Token::Plus);
        assert_eq!(tokens[5].token, Token::Minus);
        assert_eq!(tokens[6].token, Token::Asterisk);
        assert_eq!(tokens[7].token, Token::Slash);
        assert_eq!(tokens[8].token, Token::Dot);
        assert_eq!(tokens[9].token, Token::Comma);
        assert_eq!(tokens[10].token, Token::Semicolon);
        assert_eq!(tokens.len(), 11);
    }

    #[test]
    fn test_joined_characters() {
        let tokens = lex("> >= < <= = == != !");
        assert_eq!(tokens[0].token, Token::RightAngle);
        assert_eq!(tokens[1].token, Token::RightAngleEq);
        assert_eq!(tokens[2].token, Token::LeftAngle);
        assert_eq!(tokens[3].token, Token::LeftAngleEq);
        assert_eq!(tokens[4].token, Token::Equals);
        assert_eq!(tokens[5].token, Token::DoubleEq);
        assert_eq!(tokens[6].token, Token::BangEq);
        assert_eq!(tokens[7].token, Token::Bang);
        assert_eq!(tokens.len(), 8);
    }

    #[test]
    fn test_literals() {
        let tokens = lex("\"word\" \"hello world\" \"multi\nline\" 3 -4 104.1 identifier");
        assert_eq!(tokens[0].token, Token::String("word".to_owned()));
        assert_eq!(tokens[1].token, Token::String("hello world".to_owned()));
        assert_eq!(tokens[2].token, Token::String("multi\nline".to_owned()));
        assert_eq!(tokens[3].token, Token::Number(3.0));
        assert_eq!(tokens[4].token, Token::Minus);
        assert_eq!(tokens[5].token, Token::Number(4.0));
        assert_eq!(tokens[6].token, Token::Number(104.1));
        assert_eq!(tokens[7].token, Token::Identifier("identifier".to_owned()));
        assert_eq!(tokens.len(), 8);
    }

    #[test]
    fn test_keywords() {
        let tokens = lex("and class else nonkw return printandmorechars retur");
        assert_eq!(tokens[0].token, Token::And);
        assert_eq!(tokens[1].token, Token::Class);
        assert_eq!(tokens[2].token, Token::Else);
        assert_eq!(tokens[3].token, Token::Identifier("nonkw".to_owned()));
        assert_eq!(tokens[4].token, Token::Return);
        assert_eq!(
            tokens[5].token,
            Token::Identifier("printandmorechars".to_owned())
        );
        assert_eq!(tokens[6].token, Token::Identifier("retur".to_owned()));
        assert_eq!(tokens.len(), 7);
    }

    #[test]
    fn test_unterminated_str() {
        let tokens = lex("\"a string\" \"incomplete");
        assert_eq!(tokens[0].token, Token::String("a string".to_owned()));
        assert_eq!(
            tokens[1].token,
            Token::Error("Unterminated string".to_owned())
        );
        assert_eq!(tokens.len(), 2);
    }
}
