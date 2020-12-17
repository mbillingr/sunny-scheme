pub mod bytecode;
mod closure;
mod storage;
mod value;
mod record;
pub mod vm;

use sunny_memory::gc as mem;

use crate::bytecode::CodePointer;
pub use value::Value;
pub use vm::Vm;

pub type Result<T> = std::result::Result<T, ErrorKind>;
pub type RuntimeResult<T> = std::result::Result<T, Error>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    StackUnderflow,
    AllocationError,
    NotCallable,
    Halted,
    TypeError,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    kind: ErrorKind,
    location: CodePointer,
}
