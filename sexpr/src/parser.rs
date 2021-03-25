lalrpop_mod!(pub sexpr_grammar); // synthesized by LALRPOP

use crate::{Sexpr, SourceLocation};
use lalrpop_util::{lexer::Token, ParseError};
use std::borrow::Cow;

pub type Result<T> = std::result::Result<T, SourceLocation<Error>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    InvalidToken,
    UnexpectedEof {
        expected: Vec<String>,
    },
    UnrecognizedToken {
        token: String,
        expected: Vec<String>,
    },
    ExtraToken,
    User {
        error: &'static str,
    },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::InvalidToken => write!(f, "Invalid token."),
            Error::UnexpectedEof { expected } => write!(
                f,
                "Unexpected end of file. Expected one of {}",
                expected.join(", ")
            ),
            Error::UnrecognizedToken { token, expected } => write!(
                f,
                "Unrecognized token `{}`. Expected one of {}",
                token,
                expected.join(", ")
            ),
            Error::ExtraToken => write!(f, "Extra token."),
            Error::User { error } => write!(f, "{}", error),
        }
    }
}

impl From<lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'_>, &'static str>>
    for SourceLocation<Error>
{
    fn from(
        pe: lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'_>, &'static str>,
    ) -> Self {
        match pe {
            lalrpop_util::ParseError::InvalidToken { location } => {
                SourceLocation::new(Error::InvalidToken).with_span(location..location + 1)
            }
            lalrpop_util::ParseError::UnrecognizedEOF { location, expected } => {
                SourceLocation::new(Error::UnexpectedEof { expected }).with_span(location..location)
            }
            lalrpop_util::ParseError::UnrecognizedToken {
                token: (l, t, r),
                expected,
            } => SourceLocation::new(Error::UnrecognizedToken {
                token: t.to_string(),
                expected,
            })
            .with_span(l..r),
            lalrpop_util::ParseError::ExtraToken { token: (l, _, r) } => {
                SourceLocation::new(Error::ExtraToken).with_span(l..r)
            }
            lalrpop_util::ParseError::User { error } => SourceLocation::new(Error::User { error }),
        }
    }
}

pub fn parse_str(s: &str) -> Result<Vec<SourceLocation<Sexpr>>> {
    let context = SourceLocation::new(()).in_string(s);
    Ok(sexpr_grammar::ExplicitSequenceParser::new().parse(&context, s)?)
}

fn unescape(s: &str) -> std::result::Result<Cow<str>, ParseError<usize, Token, &'static str>> {
    let mut result = String::new();
    let mut section_start = 0;
    let mut escapes = s
        .char_indices()
        .filter(|(_, ch)| *ch == '\\')
        .map(|(i, _)| i);
    while let Some(escape_start) = escapes.next() {
        if result.is_empty() {
            result.reserve(s.len());
        }
        result.push_str(&s[section_start..escape_start]);

        match &s.as_bytes().get(escape_start + 1) {
            Some(b'a') => {
                result.push_str("\u{0007}");
                section_start = escape_start + 2;
            }
            Some(b'b') => {
                result.push_str("\u{0008}");
                section_start = escape_start + 2;
            }
            Some(b't') => {
                result.push_str("\u{0009}");
                section_start = escape_start + 2;
            }
            Some(b'n') => {
                result.push_str("\u{000A}");
                section_start = escape_start + 2;
            }
            Some(b'r') => {
                result.push_str("\u{000D}");
                section_start = escape_start + 2;
            }
            Some(b'"') => {
                result.push('"');
                section_start = escape_start + 2;
            }
            Some(b'\\') => {
                result.push_str(r"\");
                section_start = escape_start + 2;
                escapes.next().unwrap();
            }
            Some(b'|') => {
                result.push('|');
                section_start = escape_start + 2;
            }
            Some(b'x') => {
                let substring = &s[escape_start..];
                let end = substring
                    .bytes()
                    .position(|ch| ch == b';')
                    .ok_or(ParseError::User {
                        error: "invalid escape sequence",
                    })?;
                let utf8_ch = u32::from_str_radix(&substring[2..end], 16)
                    .ok()
                    .and_then(std::char::from_u32)
                    .ok_or(ParseError::User {
                        error: "invalid escape sequence",
                    })?;
                result.push(utf8_ch);
                section_start = escape_start + end + 1;
            }
            _ => {
                let mut i = escape_start + 1;
                while let Some(&ch) = s.as_bytes().get(i) {
                    if ch == b'\n' || !ch.is_ascii_whitespace() {
                        break;
                    }
                    i += 1;
                }
                if s.as_bytes().get(i) != Some(&b'\n') {
                    return Err(ParseError::User {
                        error: "invalid escape sequence",
                    });
                }
                while let Some(ch) = s.as_bytes().get(i) {
                    if !ch.is_ascii_whitespace() {
                        break;
                    }
                    i += 1;
                }
                section_start = i;
            }
        }
    }

    if section_start == 0 {
        Ok(Cow::Borrowed(s))
    } else {
        result.push_str(&s[section_start..]);
        Ok(Cow::Owned(result))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::parse_str;
    use crate::*;

    #[test]
    fn can_parse_empty_string() {
        let sexpr = parse_str("");
        assert!(sexpr.unwrap().is_empty());
    }

    #[test]
    fn can_parse_true() {
        let sexpr = parse_str("#t");
        assert_eq!(sexpr.unwrap(), vec![Sexpr::bool(true)]);
    }

    #[test]
    fn can_parse_false() {
        let sexpr = parse_str("#f");
        assert_eq!(sexpr.unwrap(), vec![Sexpr::bool(false)]);
    }

    #[test]
    fn can_parse_integer() {
        let sexpr = parse_str("0");
        assert_eq!(sexpr.unwrap(), vec![Sexpr::int(0)]);
    }

    #[test]
    fn can_parse_symbol() {
        let sexpr = parse_str("foo");
        assert_eq!(sexpr.unwrap(), vec![Sexpr::symbol("foo")]);
    }

    #[test]
    fn can_parse_string() {
        let sexpr = parse_str("\"foo\"");
        assert_eq!(sexpr.unwrap(), vec![Sexpr::string("foo")]);
    }

    #[test]
    fn can_parse_pair() {
        let sexpr = &parse_str("(1 . 2)").unwrap()[0];
        assert_eq!(sexpr.car().unwrap(), &Sexpr::int(1));
        assert_eq!(sexpr.cdr().unwrap(), &Sexpr::int(2));
    }

    #[test]
    fn can_parse_list() {
        let sexpr = &parse_str("(1 (2 3) 4)").unwrap()[0];
        assert_eq!(sexpr.car().unwrap(), &Sexpr::int(1));
        assert_eq!(sexpr.caadr().unwrap(), &Sexpr::int(2));
        assert_eq!(sexpr.cadadr().unwrap(), &Sexpr::int(3));
        assert_eq!(sexpr.cddadr().unwrap(), &Sexpr::nil());
        assert_eq!(sexpr.caddr().unwrap(), &Sexpr::int(4));
        assert_eq!(sexpr.cdddr().unwrap(), &Sexpr::nil());
    }

    #[test]
    fn can_parse_improper_list() {
        let sexpr = &parse_str("(1 2 . 3)").unwrap()[0];
        assert_eq!(sexpr.car().unwrap(), &Sexpr::int(1));
        assert_eq!(sexpr.cadr().unwrap(), &Sexpr::int(2));
        assert_eq!(sexpr.cddr().unwrap(), &Sexpr::int(3));
    }

    #[test]
    fn can_parse_special_character_symbols() {
        // note that a single '.' can't be parsed as an identifier
        for ch in "!$%&*+-/:<=>?@^_~".chars().map(|ch| ch.to_string()) {
            let sexpr = parse_str(&ch).map_err(|e| e.in_string(&ch));
            assert_eq!(sexpr.unwrap(), vec![Sexpr::symbol(&ch)]);
        }
    }

    #[test]
    fn can_parse_special_character_symbols_infix() {
        for ch in "!$%&*+-./:<=>?@^_~".chars().map(|ch| format!("x{}y", ch)) {
            let sexpr = parse_str(&ch).map_err(|e| e.in_string(&ch));
            assert_eq!(sexpr.unwrap(), vec![Sexpr::symbol(&ch)]);
        }
    }

    #[test]
    fn can_parse_empty_verbatim_symbol() {
        let sexpr = parse_str("||");
        assert_eq!(sexpr.unwrap(), vec![Sexpr::symbol("")]);
    }

    #[test]
    fn can_parse_whitespace_only_verbatim_symbol() {
        let sexpr = parse_str("|  |");
        assert_eq!(sexpr.unwrap(), vec![Sexpr::symbol("  ")]);
    }

    #[test]
    fn can_parse_arbitrary_verbatim_symbol() {
        let sexpr = parse_str("|hello, world!|");
        assert_eq!(sexpr.unwrap(), vec![Sexpr::symbol("hello, world!")]);
    }

    #[test]
    fn can_parse_example_identifiers() {
        // R7RS section 2.1
        assert_eq!(parse_str("...").unwrap(), vec![Sexpr::symbol("...")]);
        assert_eq!(parse_str("+").unwrap(), vec![Sexpr::symbol("+")]);
        assert_eq!(parse_str("+soup+").unwrap(), vec![Sexpr::symbol("+soup+")]);
        assert_eq!(parse_str("<=?").unwrap(), vec![Sexpr::symbol("<=?")]);
        assert_eq!(
            parse_str("->string").unwrap(),
            vec![Sexpr::symbol("->string")]
        );
        assert_eq!(
            parse_str("a34kTMNs").unwrap(),
            vec![Sexpr::symbol("a34kTMNs")]
        );
        assert_eq!(parse_str("lambda").unwrap(), vec![Sexpr::symbol("lambda")]);
        assert_eq!(
            parse_str("list->vector").unwrap(),
            vec![Sexpr::symbol("list->vector")]
        );
        assert_eq!(parse_str("q").unwrap(), vec![Sexpr::symbol("q")]);
        assert_eq!(parse_str("V17a").unwrap(), vec![Sexpr::symbol("V17a")]);
        assert_eq!(
            parse_str("|two words|").unwrap(),
            vec![Sexpr::symbol("two words")]
        );
        assert_eq!(
            parse_str(r"|two\x20;words|").unwrap(),
            vec![Sexpr::symbol("two words")]
        );
        assert_eq!(
            parse_str("the-word-recursion-has-many-meanings").unwrap(),
            vec![Sexpr::symbol("the-word-recursion-has-many-meanings")]
        );
        assert_eq!(
            parse_str(r"|\x3BB;|").unwrap(),
            vec![Sexpr::symbol("\u{3bb}")]
        );
        assert_eq!(
            parse_str(r"|\t\t|").unwrap(),
            vec![Sexpr::symbol("\x09\x09")]
        );
    }

    #[test]
    fn can_parse_newline_in_verbatim_symbol() {
        let sexpr = parse_str("|hello\\    \n     world|");
        assert_eq!(sexpr.unwrap(), vec![Sexpr::symbol("helloworld")]);
    }
}
