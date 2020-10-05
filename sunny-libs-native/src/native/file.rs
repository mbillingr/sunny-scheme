pub fn initialize() {}
pub mod exports {
    use std::path::Path;
    use sunny_core::port::{FileInputPort, FileOutputPort};
    use sunny_core::{self, Mut, Scm};

    wrap_fn!{"open-input-file",
        open_minus_input_minus_file(name) {
            let port = FileInputPort::open(
                name.as_string()
                    .expect("Argument to open-input-file must be a string.")
                    .as_str(),
            );
            Scm::input_port(port)
        }
    }

    wrap_fn!{"open-output-file",
        open_minus_output_minus_file(name) {
            let port = FileOutputPort::open(
                name.as_string()
                    .expect("Argument to open-output-file must be a string.")
                    .as_str(),
            );
            Scm::output_port(port)
        }
    }

    wrap_fn!{"file-exists?",
        file_minus_exists_p(name) {
            let name = name
                .as_string()
                .expect("Argument to open-input-file must be a string.");
            let path = Path::new(name.as_str());
            Scm::bool(path.exists())
        }
    }
}
