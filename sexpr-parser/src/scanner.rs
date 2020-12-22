use crate::Int;
use crate::{Error, Result};
pub use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Eof(usize),
    Int(usize, Int),
    Symbol(usize, &'a str),
}

impl<'a> Token<'a> {
    pub fn eof(ofs: usize) -> Self {
        Token::Eof(ofs)
    }

    pub fn int(ofs: usize, i: Int) -> Self {
        Token::Int(ofs, i)
    }

    pub fn symbol(ofs: usize, s: &'a str) -> Self {
        Token::Symbol(ofs, s)
    }
}

pub struct Scanner<'a> {
    input: &'a [u8],
    current_pos: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        Scanner {
            input: input.as_bytes(),
            current_pos: 0,
        }
    }

    pub fn next_token(&mut self) -> Result<Token<'a>> {
        let token = match self.peek() {
            None => Token::eof(self.current_pos),
            Some(_) => self.scan_word()?,
        };
        self.skip_whitespace();
        Ok(token)
    }

    pub fn scan_word(&mut self) -> Result<Token<'a>> {
        let start = self.current_pos;

        while !self.peek_delimiter() {
            self.advance();
        }

        let text = std::str::from_utf8(&self.input[start..self.current_pos]).map_err(|_| Error::Utf8Error)?;

        text
            .parse()
            .map(|i| Token::int(start, i))
            .or_else(|_| Ok(Token::symbol(start, text)))
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_ascii_whitespace() {
                self.advance();
            } else {
                return;
            }
        }
    }

    fn peek_delimiter(&mut self) -> bool {
        match self.peek() {
            None | Some(b'(') | Some(b')') => true,
            Some(ch) => ch.is_ascii_whitespace(),
        }
    }

    fn advance(&mut self) -> u8 {
        let ch = self.input[self.current_pos];
        self.current_pos += 1;
        ch
    }

    fn peek(&self) -> Option<u8> {
        self.input.get(self.current_pos).copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_empty_input_returns_eof() {
        let mut scanner = Scanner::new("");
        assert_eq!(scanner.next_token(), Ok(Token::eof(0)));
    }

    #[test]
    fn scan_multiple_digits() {
        let mut scanner = Scanner::new("123");
        assert_eq!(scanner.next_token(), Ok(Token::int(0, 123)));
    }

    #[test]
    fn scan_symbol() {
        let mut scanner = Scanner::new("foo-bar");
        assert_eq!(scanner.next_token(), Ok(Token::symbol(0, "foo-bar")));
    }
}
