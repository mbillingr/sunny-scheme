pub fn initialize() {}
pub mod exports {
    use std::fs::create_dir_all;
    use sunny_core::{self, Mut, Scm};

    wrap_fn!{"create-directory*",
        create_minus_directory_star_(path) {
            match create_dir_all(path.as_string().unwrap().as_str()) {
                Ok(_) => Scm::True,
                Err(_) => Scm::False,
            }
        }
    }
}
