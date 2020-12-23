use crate::scanner::{Scanner, Token};
use crate::{parser, Context, Sexpr};
use crate::{Error, Result};

pub fn parse_str(s: &str) -> Result<Context<Sexpr>> {
    let mut scanner = Scanner::new(s);
    parser::parse(&mut scanner).map(|x| Context::string(s, x))
}

pub fn parse_str_sequence(s: &str) -> Result<Vec<Context<Sexpr>>> {
    let mut scanner = Scanner::new(s);
    let mut sequence = vec![];
    while scanner.peek().is_some() {
        sequence.push(parser::parse(&mut scanner).map(|x| Context::string(s, x))?)
    }
    Ok(sequence)
}

pub fn parse(scanner: &mut Scanner) -> Result<Context<Sexpr>> {
    let token = scanner.next_token()?;
    parse_token(token, scanner)
}

fn parse_token(token: Token, scanner: &mut Scanner) -> Result<Context<Sexpr>> {
    match token {
        Token::Eof(pos) => Err(Context::offset(pos, Error::UnexpectedEof)),
        Token::Int(pos, i) => Ok(Context::offset(pos, Sexpr::int(i))),
        Token::Symbol(pos, s) => Ok(Context::offset(pos, Sexpr::symbol(s))),
        Token::String(pos, s) => Ok(Context::offset(pos, Sexpr::string(s))),
        Token::LParen(pos) => parse_list(pos, scanner),
        Token::RParen(pos) => Err(Context::offset(pos, Error::UnexpectedToken)),
    }
}

fn parse_list(start_pos: usize, scanner: &mut Scanner) -> Result<Context<Sexpr>> {
    let token = scanner.next_token()?;
    match token {
        Token::RParen(pos) => Ok(Context::offset(pos, Sexpr::Nil)),
        _ => Ok(Context::offset(
            start_pos,
            Sexpr::cons(
                parse_token(token, scanner)?,
                parse_list(start_pos, scanner)?,
            ),
        )),
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

    #[test]
    fn parse_empty_list() {
        let input = "()";
        assert_eq!(
            parse_str(input),
            Ok(Context::string(
                input.to_string(),
                Context::offset(1, Sexpr::nil())
            ))
        );
    }
}
