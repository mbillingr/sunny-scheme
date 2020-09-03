pub fn initialize() {}
pub mod exports {
    use std::path::Path;
    use sunny_core::port::{FileInputPort, FileOutputPort};
    use sunny_core::{self, Mut, Scm};

    thread_local! {pub static open_minus_input_minus_file: Mut<Scm> = Mut::new(Scm::func1(_open_input_file))}
    thread_local! {pub static open_minus_output_minus_file: Mut<Scm> = Mut::new(Scm::func1(_open_output_file))}
    thread_local! {pub static file_minus_exists_p: Mut<Scm> = Mut::new(Scm::func1(_file_exists))}

    fn _open_input_file(name: &Scm) -> Scm {
        let port = FileInputPort::open(
            name.as_string()
                .expect("Argument to open-input-file must be a string.")
                .as_str(),
        );
        Scm::input_port(port)
    }

    fn _open_output_file(name: &Scm) -> Scm {
        let port = FileOutputPort::open(
            name.as_string()
                .expect("Argument to open-output-file must be a string.")
                .as_str(),
        );
        Scm::output_port(port)
    }

    fn _file_exists(name: &Scm) -> Scm {
        let name = name
            .as_string()
            .expect("Argument to open-input-file must be a string.");
        let path = Path::new(name.as_str());
        Scm::bool(path.exists())
    }
}
