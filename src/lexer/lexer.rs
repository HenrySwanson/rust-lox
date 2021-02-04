use super::cursor::Cursor;
use crate::common::span::Span;
use crate::common::token::{SpannedToken, Token};

pub struct Lexer<'a> {
    source: &'a str,
    cursor: Cursor<'a>,
    seen_eof: bool,
}

pub type LexResult<T> = Result<T, String>;

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
            seen_eof: false,
        }
    }

    /// Advances through the string, and returns the next token, or
    /// None when the string is exhausted.
    fn scan_token(&mut self) -> Option<LexResult<SpannedToken>> {
        // Consume whitespace
        self.cursor.take_while(|ch| ch.is_ascii_whitespace());

        // Note the start position
        let start_pos = self.cursor.get_position();

        // To avoid Some-clutter when doing the matching, let's handle the EOF
        // case here.
        let (byte_idx, ch) = match self.cursor.take() {
            Some(t) => t,
            None => {
                if self.seen_eof {
                    return None;
                }
                self.seen_eof = true;
                let span = Span::new(start_pos, start_pos);
                return Some(Ok(SpannedToken::new(Token::EndOfFile, span)));
            }
        };

        // Figure out what the next token is
        let token_result = match ch {
            // Single character stuff
            '(' => Ok(Token::LeftParen),
            ')' => Ok(Token::RightParen),
            '{' => Ok(Token::LeftBrace),
            '}' => Ok(Token::RightBrace),
            '+' => Ok(Token::Plus),
            '-' => Ok(Token::Minus),
            '*' => Ok(Token::Asterisk),
            '.' => Ok(Token::Dot),
            ',' => Ok(Token::Comma),
            ';' => Ok(Token::Semicolon),
            // Now for the trickier stuff
            '/' => {
                if self.cursor.take_if('/') {
                    // We got a comment, recurse to get the next token
                    self.consume_line();
                    return self.scan_token();
                } else {
                    Ok(Token::Slash)
                }
            }
            '=' => Ok(self.look_for_eq_sign(Token::Equals, Token::DoubleEq)),
            '>' => Ok(self.look_for_eq_sign(Token::RightAngle, Token::RightAngleEq)),
            '<' => Ok(self.look_for_eq_sign(Token::LeftAngle, Token::LeftAngleEq)),
            '!' => Ok(self.look_for_eq_sign(Token::Bang, Token::BangEq)),
            // Everything else
            // Strings start with "
            '"' => self.scan_string(byte_idx),
            // Numbers start with a digit
            _ if is_digit_char(ch) => self.scan_number(byte_idx),
            // This branch would match a digit, but we match digits earlier so it's okay
            _ if is_identifier_char(ch) => self.scan_identifier_or_kw(byte_idx),
            _ => Err(format!("Unrecognized token `{}`", ch)),
        };

        // If we got a real token, return it, otherwise bubble it up
        match token_result {
            Ok(token) => {
                // TODO n-1?
                let end_pos = self.cursor.get_position();
                let token = SpannedToken::new(token, Span::new(start_pos, end_pos));
                Some(Ok(token))
            }
            Err(e) => Some(Err(format!("{}: {}", start_pos, e))),
        }
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
    fn scan_string(&mut self, start_idx: usize) -> LexResult<Token> {
        // Bump start_idx up by one so that we get the first character
        // of the string, not the quote
        let start_idx = start_idx + 1;

        self.cursor.take_until(|ch| ch == '"');

        let end_idx = match self.cursor.peek() {
            None => return Err("Unterminated string".to_owned()),
            Some((i, _)) => i,
        };

        // Now consume the closing quote
        self.cursor.take();

        let value = self.source[start_idx..end_idx].to_owned();
        Ok(Token::String(value))
    }

    /// Scans to the end of the number, and returns a token containing its value.
    fn scan_number(&mut self, start_idx: usize) -> LexResult<Token> {
        self.cursor.take_while(is_digit_char);

        let end_idx = match self.cursor.peek() {
            None => self.source.len(),
            Some((i, _)) => i,
        };

        let slice = &self.source[start_idx..end_idx];
        match slice.parse() {
            Ok(value) => Ok(Token::Number(value)),
            Err(_) => Err(format!("Unparsable integer `{}`", slice)),
        }
    }

    /// Scans up to the end of the word, and returns the token for it.
    fn scan_identifier_or_kw(&mut self, start_idx: usize) -> LexResult<Token> {
        self.cursor.take_while(is_identifier_char);

        let end_idx = match self.cursor.peek() {
            None => self.source.len(),
            Some((i, _)) => i,
        };

        let slice = &self.source[start_idx..end_idx];
        let token = match slice {
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
            _ => Token::Identifier(slice.to_owned()),
        };

        Ok(token)
    }

    // I feel like it's more idiomatic to have lexer.iter()?
    pub fn iter(self) -> LexerIterator<'src> {
        LexerIterator { lexer: self }
    }
}

pub struct LexerIterator<'src> {
    lexer: Lexer<'src>,
}

impl<'src> Iterator for LexerIterator<'src> {
    type Item = LexResult<SpannedToken>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.scan_token()
    }
}

#[cfg(test)]
mod tests {

    use super::{Lexer, Span, SpannedToken, Token};
    use crate::common::span::CodePosition;

    fn lex(source: &str) -> (Vec<SpannedToken>, Vec<String>) {
        let mut tokens = vec![];
        let mut errors = vec![];

        for r in Lexer::new(source).iter() {
            match r {
                Ok(token) => tokens.push(token),
                Err(err) => errors.push(err),
            }
        }

        (tokens, errors)
    }

    #[test]
    fn test_simple_characters() {
        let (tokens, _) = lex("() {} + - * / .,;");
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
        assert_eq!(tokens[11].token, Token::EndOfFile);
    }

    #[test]
    fn test_joined_characters() {
        let (tokens, _) = lex("> >= < <= = == != !");
        assert_eq!(tokens[0].token, Token::RightAngle);
        assert_eq!(tokens[1].token, Token::RightAngleEq);
        assert_eq!(tokens[2].token, Token::LeftAngle);
        assert_eq!(tokens[3].token, Token::LeftAngleEq);
        assert_eq!(tokens[4].token, Token::Equals);
        assert_eq!(tokens[5].token, Token::DoubleEq);
        assert_eq!(tokens[6].token, Token::BangEq);
        assert_eq!(tokens[7].token, Token::Bang);
        assert_eq!(tokens[8].token, Token::EndOfFile);
    }

    #[test]
    fn test_literals() {
        let (tokens, _) = lex("\"word\" \"hello world\" \"multi\nline\" 3 -4 104 identifier");
        assert_eq!(tokens[0].token, Token::String("word".to_owned()));
        assert_eq!(tokens[1].token, Token::String("hello world".to_owned()));
        assert_eq!(tokens[2].token, Token::String("multi\nline".to_owned()));
        assert_eq!(tokens[3].token, Token::Number(3));
        assert_eq!(tokens[4].token, Token::Minus);
        assert_eq!(tokens[5].token, Token::Number(4));
        assert_eq!(tokens[6].token, Token::Number(104));
        assert_eq!(tokens[7].token, Token::Identifier("identifier".to_owned()));
        assert_eq!(tokens[8].token, Token::EndOfFile);
    }

    #[test]
    fn test_keywords() {
        let (tokens, _) = lex("and class else nonkw return printandmorechars retur");
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
        assert_eq!(tokens[7].token, Token::EndOfFile);
    }

    #[test]
    fn test_unterminated_str() {
        let (tokens, errors) = lex("\"a string\" \"incomplete");
        assert_eq!(tokens[0].token, Token::String("a string".to_owned()));
        assert_eq!(tokens[1].token, Token::EndOfFile);

        assert_eq!(errors, vec!["1:12: Unterminated string"]);
    }

    #[test]
    fn test_malformed_number() {
        let (tokens, errors) = lex("3 999999999999999999999999999999999999999 1");
        assert_eq!(tokens[0].token, Token::Number(3));
        assert_eq!(tokens[1].token, Token::Number(1));
        assert_eq!(tokens[2].token, Token::EndOfFile);

        assert_eq!(
            errors,
            vec!["1:3: Unparsable integer `999999999999999999999999999999999999999`"]
        );
    }
}
