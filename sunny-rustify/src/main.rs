mod scm2rs;

// the program produced by scm2rs expects the
// module hierarchy to start at crate level.
use scm2rs::*;
use sunny_libs_native::native;

fn main() {
    scm2rs::main()
}
