use sunny_sexpr_parser::SourceLocation;

pub type Result<T> = std::result::Result<T, SourceLocation<Error>>;

#[derive(Debug, PartialEq)]
pub enum Error {
    MissingArgument,
    ExpectedSymbol,
    ExpectedList,
    ExpectedEmptyList,
    UnexpectedStatement,
    SyntaxAsValue,
    Expected(Expectation),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expectation {
    EmptyList,
    List,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::MissingArgument => write!(f, "Missing argument"),
            Error::ExpectedSymbol => write!(f, "Expected symbol"),
            Error::ExpectedList => write!(f, "Expected list"),
            Error::ExpectedEmptyList => write!(f, "Expected ()"),
            Error::UnexpectedStatement => write!(f, "Unexpected statement"),
            Error::SyntaxAsValue => write!(f, "Syntax used as value"),
            Error::Expected(ex) => write!(f, "Expected {}", ex),
        }
    }
}

impl std::fmt::Display for Expectation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expectation::EmptyList => write!(f, "()"),
            Expectation::List => write!(f, "<list>"),
        }
    }
}

pub fn error_at<T>(sexpr: &SourceLocation<T>, error: impl Into<Error>) -> SourceLocation<Error> {
    sexpr.map(error.into())
}

pub fn error_after<T>(sexpr: &SourceLocation<T>, error: impl Into<Error>) -> SourceLocation<Error> {
    sexpr.map_after(error.into())
}

impl From<Expectation> for Error {
    fn from(ex: Expectation) -> Self {
        Error::Expected(ex)
    }
}
