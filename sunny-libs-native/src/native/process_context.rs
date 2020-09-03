pub fn initialize() {}
pub mod exports {
    use std::env;
    use sunny_core::{self, Mut, Scm};

    thread_local! {pub static command_minus_line: Mut<Scm> = Mut::new(Scm::func0(_command_line))}

    fn _command_line() -> Scm {
        let mut arglist = Scm::nil();
        for arg in env::args().rev() {
            arglist = Scm::pair(arg, arglist);
        }
        arglist
    }
}
