#[allow(unused_imports)] use sunny_core::{Mut, Scm};
mod imports{pub use crate::native::write::exports::*;
}

pub mod exports{pub use super::imports::display as display;
pub use super::imports::newline as newline;
pub use super::imports::write as write;
}

mod globals{
}

pub fn initialize() {
crate::native::write::initialize();
(/*NOP*/);}
