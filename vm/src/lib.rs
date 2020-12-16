mod closure;
pub mod code;
mod storage;
mod value;
pub mod vm;

use sunny_memory::gc as mem;

pub use value::Value;
pub use vm::Vm;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Error {
    StackUnderflow,
    AllocationError,
    NotCallable,
    Halted,
    TypeError,
}
