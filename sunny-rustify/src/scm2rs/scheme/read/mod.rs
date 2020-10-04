#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::native::read::exports::*;
}

pub mod exports {
    pub use super::imports::read;
}

mod globals {
    use sunny_core::{Mut, Scm};
}

pub fn initialize() {
    crate::native::read::initialize();
    (/*NOP*/);
}
