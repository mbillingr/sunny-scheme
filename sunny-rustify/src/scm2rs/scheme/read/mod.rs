#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::native::read::exports::*;
}

pub mod exports {
    pub use super::imports::read;
}

pub fn initialize() {
    crate::native::read::initialize();
    (/*NOP*/);
}
