#[macro_use]
extern crate lalrpop_util;

mod cxr;
pub mod parser;
mod scm;
mod shared_string;
mod source_location;
mod source_map;
pub mod str_utils;

type Int = i64;

pub use cxr::CxR;
pub use scm::{HashEqual, HashPtrEq, Scm, ScmHasher, ScmObject, WeakScm};
pub use shared_string::SharedStr;
pub use source_location::{SourceKind, SourceLocation};
pub use source_map::SourceMap;
