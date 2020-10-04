#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::native::write::exports::*;
}

pub mod exports {
    pub use super::imports::display;
    pub use super::imports::newline;
    pub use super::imports::write;
}

mod globals {
    use sunny_core::{Mut, Scm};
}

pub fn initialize() {
    crate::native::write::initialize();
    (/*NOP*/);
}
