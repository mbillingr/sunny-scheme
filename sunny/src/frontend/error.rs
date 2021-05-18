use crate::filesystem::Error as FsError;
use sunny_scm::parser::Error as ParseError;
use sunny_scm::SourceLocation;

pub type Result<T> = std::result::Result<T, SourceLocation<Error>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    ParseError(ParseError),
    IoError(String),
    MissingArgument,
    ExpectedSymbol,
    ExpectedString,
    ExpectedList,
    ExpectedEmptyList,
    UnexpectedStatement,
    SyntaxAsValue,
    InvalidForm,
    InvalidTemplate,
    UnknownSyntaxTransformer,
    UndefinedExport,
    UnknownLibrary,
    WrongNrArgs(usize, usize),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::ParseError(pe) => write!(f, "{}", pe),
            Error::IoError(ioe) => write!(f, "{}", ioe),
            Error::MissingArgument => write!(f, "Missing argument"),
            Error::ExpectedSymbol => write!(f, "Expected symbol"),
            Error::ExpectedString => write!(f, "Expected string"),
            Error::ExpectedList => write!(f, "Expected list"),
            Error::ExpectedEmptyList => write!(f, "Expected ()"),
            Error::UnexpectedStatement => write!(f, "Unexpected statement"),
            Error::SyntaxAsValue => write!(f, "Syntax used as value"),
            Error::InvalidForm => write!(f, "Invalid syntactic form"),
            Error::InvalidTemplate => write!(f, "Invalid syntax template"),
            Error::UnknownSyntaxTransformer => write!(f, "Unknown syntax transformer"),
            Error::UndefinedExport => write!(f, "Undefined export"),
            Error::UnknownLibrary => write!(f, "Unknown library"),
            Error::WrongNrArgs(actual, expect) => write!(
                f,
                "Wrong number of arguments {}, expected {}",
                actual, expect
            ),
        }
    }
}

impl From<FsError> for SourceLocation<Error> {
    fn from(e: FsError) -> Self {
        match e {
            FsError::ParseError(pe) => pe.map(Error::ParseError),
            FsError::IoError(ioe) => SourceLocation::new(Error::IoError(ioe.to_string())),
        }
    }
}

pub fn error_at<T>(sexpr: &SourceLocation<T>, error: impl Into<Error>) -> SourceLocation<Error> {
    sexpr.map_value(error.into())
}

pub fn error_after<T>(sexpr: &SourceLocation<T>, error: impl Into<Error>) -> SourceLocation<Error> {
    sexpr.map_after(error.into())
}
