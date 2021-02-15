mod activation;
mod basic_block;
pub mod bytecode;
pub mod bytecode_loader;
mod closure;
mod storage;
mod table;
mod value;
pub mod vm;

use sunny_memory::gc as mem;

use crate::bytecode::CodePointer;
pub use storage::ValueStorage;
pub use value::Value;
pub use vm::Vm;

pub type Result<T> = std::result::Result<T, ErrorKind>;
pub type RuntimeResult<T> = std::result::Result<T, Error>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    StackUnderflow,
    NotCallable,
    Halted,
    TypeError,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    kind: ErrorKind,
    location: CodePointer,
}
