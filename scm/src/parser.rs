lalrpop_mod!(pub sexpr_grammar); // synthesized by LALRPOP

use crate::scm::Scm;
use crate::shared_string::SharedStr;
use crate::source_map::SourceMap;
use crate::SourceLocation;
use lalrpop_util::{lexer::Token, ParseError};
use std::borrow::Cow;
use std::collections::HashMap;

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

impl Error {
    pub fn is_eof(&self) -> bool {
        matches!(self, Error::UnexpectedEof { .. })
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

pub fn parse_str(src: impl Into<SharedStr>) -> Result<Vec<Scm>> {
    let src = src.into();
    let src_map = SourceMap::new();
    parse_with_map(src, &src_map)
}

pub fn parse_with_map(src: impl Into<SharedStr>, src_map: &SourceMap) -> Result<Vec<Scm>> {
    let src = src.into();
    let context = SourceLocation::new(()).in_string(src.clone());
    Ok(sexpr_grammar::ExplicitSequenceParser::new().parse(
        &context,
        src_map,
        &mut HashMap::new(),
        &src,
    )?)
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
                result.push('\u{0007}');
                section_start = escape_start + 2;
            }
            Some(b'b') => {
                result.push('\u{0008}');
                section_start = escape_start + 2;
            }
            Some(b't') => {
                result.push('\u{0009}');
                section_start = escape_start + 2;
            }
            Some(b'n') => {
                result.push('\u{000A}');
                section_start = escape_start + 2;
            }
            Some(b'r') => {
                result.push('\u{000D}');
                section_start = escape_start + 2;
            }
            Some(b'"') => {
                result.push('"');
                section_start = escape_start + 2;
            }
            Some(b'\\') => {
                result.push('\'');
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
    use sexpr_generics::prelude::*;

    #[test]
    fn can_parse_empty_string() {
        let sexpr = parse_str("");
        assert!(sexpr.unwrap().is_empty());
    }

    #[test]
    fn can_parse_true() {
        let sexpr = parse_str("#t");
        assert_eq!(sexpr.unwrap(), vec![Scm::bool(true)]);
    }

    #[test]
    fn can_parse_false() {
        let sexpr = parse_str("#f");
        assert_eq!(sexpr.unwrap(), vec![Scm::bool(false)]);
    }

    #[test]
    fn can_parse_integer() {
        let sexpr = parse_str("0");
        assert_eq!(sexpr.unwrap(), vec![Scm::int(0)]);
    }

    #[test]
    fn can_parse_characters() {
        assert_eq!(parse_str(r"#\A").unwrap(), vec![Scm::char('A')]);
        assert_eq!(parse_str(r"#\x").unwrap(), vec![Scm::char('x')]);
        assert_eq!(parse_str(r"#\(").unwrap(), vec![Scm::char('(')]);
        assert_eq!(
            parse_str(r"#\ #\5").unwrap(),
            vec![Scm::char(' '), Scm::char('5')]
        );
        assert_eq!(parse_str(r"#\x03bb").unwrap(), vec![Scm::char('λ')]);
        assert_eq!(parse_str(r"#\X03bb").unwrap(), vec![Scm::char('\u{03bb}')]);
    }

    #[test]
    fn can_parse_required_named_characters() {
        for (repr, ch) in vec![
            (r"#\alarm", '\u{0007}'),
            (r"#\backspace", '\u{0008}'),
            (r"#\delete", '\u{007f}'),
            (r"#\escape", '\u{001b}'),
            (r"#\newline", '\u{000a}'),
            (r"#\null", '\u{0000}'),
            (r"#\return", '\u{000d}'),
            (r"#\space", '\u{0020}'),
            (r"#\tab", '\u{0009}'),
            (r"#\alarm", '\u{0007}'),
        ] {
            assert_eq!(parse_str(repr).unwrap(), vec![Scm::char(ch)]);
        }
    }

    #[test]
    fn can_extra_required_named_characters() {
        for (repr, ch) in vec![
            (r"#\alpha", '\u{03b1}'),
            (r"#\beta", '\u{03b2}'),
            (r"#\gamma", '\u{03b3}'),
            (r"#\delta", '\u{03b4}'),
            (r"#\epsilon", '\u{03b5}'),
            (r"#\zeta", '\u{03b6}'),
            (r"#\eta", '\u{03b7}'),
            (r"#\theta", '\u{03b8}'),
            (r"#\iota", '\u{03b9}'),
            (r"#\kappa", '\u{03ba}'),
            (r"#\lambda", '\u{03bb}'),
            (r"#\mu", '\u{03bc}'),
            (r"#\nu", '\u{03bd}'),
            (r"#\xi", '\u{03be}'),
            (r"#\omicron", '\u{03bf}'),
            (r"#\pi", '\u{03c0}'),
            (r"#\rho", '\u{03c1}'),
            (r"#\2sigma", '\u{03c2}'),
            (r"#\sigma", '\u{03c3}'),
            (r"#\tau", '\u{03c4}'),
            (r"#\upsilon", '\u{03c5}'),
            (r"#\phi", '\u{03c6}'),
            (r"#\chi", '\u{03c7}'),
            (r"#\psi", '\u{03c8}'),
            (r"#\omega", '\u{03c9}'),
            (r"#\Alpha", '\u{0391}'),
            (r"#\Beta", '\u{0392}'),
            (r"#\Gamma", '\u{0393}'),
            (r"#\Delta", '\u{0394}'),
            (r"#\Epsilon", '\u{0395}'),
            (r"#\Zeta", '\u{0396}'),
            (r"#\Eta", '\u{0397}'),
            (r"#\Theta", '\u{0398}'),
            (r"#\Iota", '\u{0399}'),
            (r"#\Kappa", '\u{039a}'),
            (r"#\Lambda", '\u{039b}'),
            (r"#\Mu", '\u{039c}'),
            (r"#\Nu", '\u{039d}'),
            (r"#\Xi", '\u{039e}'),
            (r"#\Omicron", '\u{039f}'),
            (r"#\Pi", '\u{03a0}'),
            (r"#\Rho", '\u{03a1}'),
            (r"#\Sigma", '\u{03a3}'),
            (r"#\Tau", '\u{03a4}'),
            (r"#\Upsilon", '\u{03a5}'),
            (r"#\Phi", '\u{03a6}'),
            (r"#\Chi", '\u{03a7}'),
            (r"#\Psi", '\u{03a8}'),
            (r"#\Omega", '\u{03a9}'),
        ] {
            assert_eq!(parse_str(repr).unwrap(), vec![Scm::char(ch)]);
        }
    }

    #[test]
    fn can_parse_symbol() {
        let sexpr = parse_str("foo");
        assert_eq!(sexpr.unwrap(), vec![Scm::symbol("foo")]);
    }

    #[test]
    fn can_parse_string() {
        let sexpr = parse_str("\"foo\"");
        assert_eq!(sexpr.unwrap(), vec![Scm::string("foo")]);
    }

    #[test]
    fn can_parse_pair() {
        let sexpr = &parse_str("(1 . 2)").unwrap()[0];
        assert_eq!(sexpr.car().unwrap(), &Scm::int(1));
        assert_eq!(sexpr.cdr().unwrap(), &Scm::int(2));
    }

    #[test]
    fn can_parse_list() {
        let sexpr = &parse_str("(1 (2 3) 4)").unwrap()[0];
        assert_eq!(sexpr.car().unwrap(), &Scm::int(1));
        assert_eq!(sexpr.caadr().unwrap(), &Scm::int(2));
        assert_eq!(sexpr.cadadr().unwrap(), &Scm::int(3));
        assert_eq!(sexpr.cddadr().unwrap(), &Scm::null());
        assert_eq!(sexpr.caddr().unwrap(), &Scm::int(4));
        assert_eq!(sexpr.cdddr().unwrap(), &Scm::null());
    }

    #[test]
    fn can_parse_improper_list() {
        let sexpr = &parse_str("(1 2 . 3)").unwrap()[0];
        assert_eq!(sexpr.car().unwrap(), &Scm::int(1));
        assert_eq!(sexpr.cadr().unwrap(), &Scm::int(2));
        assert_eq!(sexpr.cddr().unwrap(), &Scm::int(3));
    }

    #[test]
    fn can_parse_special_character_symbols() {
        // note that a single '.' can't be parsed as an identifier
        for ch in "!$%&*+-/:<=>?@^_~".chars().map(|ch| ch.to_string()) {
            let sexpr = parse_str(&ch).map_err(|e| e.in_string(&ch));
            assert_eq!(sexpr.unwrap(), vec![Scm::symbol(&ch)]);
        }
    }

    #[test]
    fn can_parse_special_character_symbols_infix() {
        for ch in "!$%&*+-./:<=>?@^_~".chars().map(|ch| format!("x{}y", ch)) {
            let sexpr = parse_str(&ch).map_err(|e| e.in_string(&ch));
            assert_eq!(sexpr.unwrap(), vec![Scm::symbol(&ch)]);
        }
    }

    #[test]
    fn can_parse_empty_verbatim_symbol() {
        let sexpr = parse_str("||");
        assert_eq!(sexpr.unwrap(), vec![Scm::symbol("")]);
    }

    #[test]
    fn can_parse_whitespace_only_verbatim_symbol() {
        let sexpr = parse_str("|  |");
        assert_eq!(sexpr.unwrap(), vec![Scm::symbol("  ")]);
    }

    #[test]
    fn can_parse_arbitrary_verbatim_symbol() {
        let sexpr = parse_str("|hello, world!|");
        assert_eq!(sexpr.unwrap(), vec![Scm::symbol("hello, world!")]);
    }

    #[test]
    fn can_parse_example_identifiers() {
        // R7RS section 2.1
        assert_eq!(parse_str("...").unwrap(), vec![Scm::symbol("...")]);
        assert_eq!(parse_str("+").unwrap(), vec![Scm::symbol("+")]);
        assert_eq!(parse_str("+soup+").unwrap(), vec![Scm::symbol("+soup+")]);
        assert_eq!(parse_str("<=?").unwrap(), vec![Scm::symbol("<=?")]);
        assert_eq!(
            parse_str("->string").unwrap(),
            vec![Scm::symbol("->string")]
        );
        assert_eq!(
            parse_str("a34kTMNs").unwrap(),
            vec![Scm::symbol("a34kTMNs")]
        );
        assert_eq!(parse_str("lambda").unwrap(), vec![Scm::symbol("lambda")]);
        assert_eq!(
            parse_str("list->vector").unwrap(),
            vec![Scm::symbol("list->vector")]
        );
        assert_eq!(parse_str("q").unwrap(), vec![Scm::symbol("q")]);
        assert_eq!(parse_str("V17a").unwrap(), vec![Scm::symbol("V17a")]);
        assert_eq!(
            parse_str("|two words|").unwrap(),
            vec![Scm::symbol("two words")]
        );
        assert_eq!(
            parse_str(r"|two\x20;words|").unwrap(),
            vec![Scm::symbol("two words")]
        );
        assert_eq!(
            parse_str("the-word-recursion-has-many-meanings").unwrap(),
            vec![Scm::symbol("the-word-recursion-has-many-meanings")]
        );
        assert_eq!(
            parse_str(r"|\x3BB;|").unwrap(),
            vec![Scm::symbol("\u{3bb}")]
        );
        assert_eq!(parse_str(r"|\t\t|").unwrap(), vec![Scm::symbol("\x09\x09")]);
    }

    #[test]
    fn can_parse_newline_in_verbatim_symbol() {
        let sexpr = parse_str("|hello\\    \n     world|");
        assert_eq!(sexpr.unwrap(), vec![Scm::symbol("helloworld")]);
    }
}
