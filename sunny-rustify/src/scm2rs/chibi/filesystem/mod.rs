#[allow(unused_imports)] use sunny_core::{Mut, Scm};
mod imports{pub use crate::native::filesystem::exports::*;
}

pub mod exports{pub use super::imports::create_minus_directory_star_ as create_minus_directory_star_;
}

mod globals{
}

pub fn initialize() {
crate::native::filesystem::initialize();
(/*NOP*/);}
