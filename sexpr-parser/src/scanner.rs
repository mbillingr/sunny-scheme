use logos::{Lexer, Logos, Span};

pub type Scanner<'a> = Lexer<'a, Token>;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[regex("[0-9]+", |lex| lex.slice().parse())]
    Int(i64),

    #[regex("[a-zA-Z]+")]
    Symbol,

    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[error]
    Error,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex() {
        use Token::*;
        let mut lex = Token::lexer("foo (bar 42)");
        let tokens: Vec<_> = lex.spanned().collect();
        assert_eq!(
            tokens,
            vec![
                (Symbol, 0..3),
                (LParen, 4..5),
                (Symbol, 5..8),
                (Int(42), 9..11),
                (RParen, 11..12)
            ]
        )
    }
}
