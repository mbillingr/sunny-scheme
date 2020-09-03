pub fn initialize() {}
pub mod exports {
    use std::fs::create_dir_all;
    use sunny_core::{self, Mut, Scm};

    thread_local! {pub static create_minus_directory_star_: Mut<Scm> = Mut::new(Scm::func1(_create_directory))}

    fn _create_directory(path: &Scm) -> Scm {
        match create_dir_all(path.as_string().unwrap().as_str()) {
            Ok(_) => Scm::True,
            Err(_) => Scm::False,
        }
    }
}
