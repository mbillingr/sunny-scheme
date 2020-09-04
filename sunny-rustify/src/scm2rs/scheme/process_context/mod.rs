#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::native::process_context::exports::*;
}

pub mod exports {
    pub use super::imports::command_minus_line;
}

mod globals {}

pub fn initialize() {
    crate::native::process_context::initialize();
    (/*NOP*/);
}
