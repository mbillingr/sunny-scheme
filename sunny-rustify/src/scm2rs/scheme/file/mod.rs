#[allow(unused_imports)] use sunny_core::{Mut, Scm};
mod imports{pub use crate::native::file::exports::*;
}

pub mod exports{pub use super::imports::file_minus_exists_p as file_minus_exists_p;
pub use super::imports::open_minus_input_minus_file as open_minus_input_minus_file;
pub use super::imports::open_minus_output_minus_file as open_minus_output_minus_file;
}

mod globals{
}

pub fn initialize() {
crate::native::file::initialize();
(/*NOP*/);}
