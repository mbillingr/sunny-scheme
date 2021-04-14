pub use basic_block::{BasicBlock, BlockChain};
pub use primitive::{Primitive, PrimitiveProc};
pub use sunny_memory::rc as mem;
pub use value::{Object, Value, WeakValue};
pub use vm::Vm;

use crate::bytecode::CodePointer;
use sunny_sexpr_parser::Scm;

mod activation;
mod basic_block;
pub mod bytecode;
pub mod bytecode_loader;
mod closure;
mod continuation;
pub mod number;
pub mod optimizations;
mod primitive;
pub mod scm_extension;
mod value;
pub mod vm;

pub type Result<T> = std::result::Result<T, ErrorKind>;
pub type RuntimeResult<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    Generic(Scm),
    Halted,
    StackUnderflow,
    NotCallable,
    TypeError,
    UndefinedVariable,
    TooFewArgs,
    TooManyArgs,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    kind: ErrorKind,
    location: CodePointer,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ErrorKind::Generic(value) => write!(f, "{}", value),
            ErrorKind::Halted => write!(f, "Halted"),
            ErrorKind::StackUnderflow => write!(f, "Stack underflow."),
            ErrorKind::NotCallable => write!(f, "Not callable"),
            ErrorKind::TypeError => write!(f, "Type Error"),
            ErrorKind::UndefinedVariable => write!(f, "Undefined Variable"),
            ErrorKind::TooFewArgs => write!(f, "Not enough arguments"),
            ErrorKind::TooManyArgs => write!(f, "Too many arguments"),
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "{}", self.kind)?;
        writeln!(f, "{}", self.location.pretty_fmt(1, 1))
    }
}
