use crate::scanner::{Scanner, Token};
use crate::{parser, Context, Sexpr};
use crate::{Error, Result};

pub fn parse_str(s: &str) -> Result<Context<Sexpr>> {
    let scanner = Scanner::from_str(s);
    parser::parse(scanner).map(|x| Context::string(s, x))
}

pub fn parse<T: Iterator<Item = u8>>(mut scanner: Scanner<T>) -> Result<Context<Sexpr>> {
    let token = scanner.next_token()?;

    match token {
        Token::Eof(pos) => Err(Context::offset(pos, Error::UnexpectedEof)),
        Token::Int(pos, i) => Ok(Context::offset(pos, Sexpr::int(i))),
        Token::Symbol(pos, s) => Ok(Context::offset(pos, Sexpr::symbol(s))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_empty_scanner_eof_error() {
        assert_eq!(parse_str(""), Err(Context::offset(0, Error::UnexpectedEof)));
    }

    #[test]
    fn parse_number() {
        let input = "123";
        assert_eq!(
            parse_str(input),
            Ok(Context::string(
                input.to_string(),
                Context::offset(0, Sexpr::int(123))
            ))
        );
    }
}
