use crate::scanner::{Scanner, Token};
use crate::{parser, Context, Sexpr};
use crate::{Error, Result};
use logos::Logos;

pub fn parse_str(s: &str) -> Result<Context<Sexpr>> {
    let mut scanner = Token::lexer(s);
    parser::parse(&mut scanner).map(|x| Context::string(s, x))
}

pub fn parse_str_sequence(s: &str) -> Result<Vec<Context<Sexpr>>> {
    let mut scanner = Token::lexer(s);
    let mut sequence = vec![];
    loop {
        let token = scanner.next();
        if token.is_none() {
            break;
        }
        let sexpr = parse_token(token, &mut scanner)?;
        sequence.push(sexpr);
    }
    Ok(sequence)
}

pub fn parse(scanner: &mut Scanner) -> Result<Context<Sexpr>> {
    let token = scanner.next();
    parse_token(token, scanner)
}

fn parse_token(token: Option<Token>, scanner: &mut Scanner) -> Result<Context<Sexpr>> {
    let pos = scanner.span().start;
    match token {
        None => Err(Context::offset(pos, Error::UnexpectedEof)),
        Some(Token::Int(i)) => Ok(Context::offset(pos, Sexpr::int(i))),
        Some(Token::Symbol) => Ok(Context::offset(pos, Sexpr::symbol(scanner.slice()))),
        Some(Token::String(s)) => Ok(Context::offset(pos, Sexpr::string(s))),
        Some(Token::LParen) => parse_list(pos, scanner),
        Some(Token::RParen) => Err(Context::offset(pos, Error::UnexpectedToken)),
        Some(Token::Error) => Err(Context::offset(
            pos,
            Error::InvalidToken(scanner.slice().to_string()),
        )),
    }
}

fn parse_list(start_pos: usize, scanner: &mut Scanner) -> Result<Context<Sexpr>> {
    let token = scanner.next();
    match token {
        Some(Token::RParen) => Ok(Context::offset(scanner.span().start, Sexpr::Nil)),
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
