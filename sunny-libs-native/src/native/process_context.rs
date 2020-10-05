pub fn initialize() {}
pub mod exports {
    use std::env;
    use sunny_core::{self, Mut, Scm};

    wrap_fn!{"command-line",
        command_minus_line() {
            let mut arglist = Scm::nil();
            for arg in env::args().rev() {
                arglist = Scm::pair(arg, arglist);
            }
            arglist
        }
    }
}
