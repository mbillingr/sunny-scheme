use logos::{self, Logos};
use std::iter::Peekable;
use std::str::CharIndices;

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Logos, Debug, Clone)]
pub enum Tok<'input> {
    Dummy(&'input str),

    #[token("#f")]
    #[token("#false")]
    False,

    #[token("#t")]
    #[token("#true")]
    True,

    Quotation,
    LabelDef(usize),
    LabelRef(usize),

    #[error]
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexicalError {
    User(String),
}

pub struct Lexer<'input> {
    lex: logos::Lexer<'input, Tok<'input>>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        let lex = Tok::lexer(input);
        Lexer { lex }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Tok<'input>, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.lex.next()?;
        let span = self.lex.span();
        if let Tok::Error = token {
            return Some(Err(LexicalError::User(format!(
                "lexical error: {}",
                self.lex.slice()
            ))));
        }

        Some(Ok((span.start, token, span.end)))
    }
}
