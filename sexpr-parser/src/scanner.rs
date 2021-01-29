use logos::{Lexer, Logos};

pub type Scanner<'a> = Lexer<'a, Token<'a>>;

#[derive(Logos, Debug, PartialEq)]
pub enum Token<'a> {
    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[regex("[0-9]+", |lex| lex.slice().parse())]
    Int(i64),

    #[regex("[a-zA-Z]+")]
    Symbol,

    #[regex("\"[^\"]*\"", |lex| &lex.slice()[1..lex.slice().len()-1])]
    String(&'a str),

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
        let lex = Token::lexer("foo (bar 42)");
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
