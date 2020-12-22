use crate::{Context, Int};
use crate::{Error, Result};
pub use std::iter::Peekable;
use std::marker::PhantomData;

#[derive(Debug, PartialEq)]
pub enum Token {
    Eof(usize),
    Int(usize, Int),
}

impl Token {
    pub fn eof(ofs: usize) -> Self {
        Token::Eof(ofs)
    }
    pub fn int(ofs: usize, i: Int) -> Self {
        Token::Int(ofs, i)
    }
}

pub struct Scanner<'a, T: Iterator<Item = u8>> {
    _p: std::marker::PhantomData<&'a ()>,
    input: Peekable<T>,
    current_pos: usize,
}

impl<T: Iterator<Item = u8>> Scanner<'_, T> {
    pub fn new(input: T) -> Self {
        Scanner {
            _p: PhantomData,
            input: input.peekable(),
            current_pos: 0,
        }
    }

    pub fn next_token(&mut self) -> Result<Token> {
        let token = match self.input.peek() {
            None => Token::eof(self.current_pos),
            Some(ch) if ch.is_ascii_digit() => self.scan_number()?,
            _ => unimplemented!(),
        };
        self.skip_whitespace();
        Ok(token)
    }

    pub fn scan_number(&mut self) -> Result<Token> {
        let start = self.current_pos;

        let mut text = vec![];
        while !self.peek_delimiter() {
            text.push(self.advance());
        }

        String::from_utf8(text)
            .ok()
            .and_then(|text| text.parse().ok())
            .map(|i| Token::int(start, i))
            .ok_or(Context::offset(self.current_pos, Error::InvalidNumber))
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.input.peek() {
            if ch.is_ascii_whitespace() {
                self.advance();
            } else {
                return;
            }
        }
    }

    fn peek_delimiter(&mut self) -> bool {
        match self.input.peek() {
            None | Some(b'(') | Some(b')') => true,
            Some(ch) => ch.is_ascii_whitespace(),
        }
    }

    fn advance(&mut self) -> u8 {
        self.current_pos += 1;
        self.input.next().unwrap()
    }
}

impl<'a> Scanner<'a, std::str::Bytes<'a>> {
    pub fn from_str(s: &'a str) -> Self {
        Scanner::new(s.bytes())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_empty_input_returns_eof() {
        let mut scanner = Scanner::from_str("");
        assert_eq!(scanner.next_token(), Ok(Token::eof(0)));
    }

    #[test]
    fn scan_single_digit() {
        let mut scanner = Scanner::from_str("1");
        assert_eq!(scanner.next_token(), Ok(Token::int(0, 1)));
    }

    #[test]
    fn scan_multiple_digits() {
        let mut scanner = Scanner::from_str("123");
        assert_eq!(scanner.next_token(), Ok(Token::int(0, 123)));
    }
}
