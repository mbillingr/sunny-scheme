pub fn initialize() {}
pub mod exports {
    use std::io::stdin;
    use sunny_core::{self, Mut, Scm};
    use sunny_parse::from_reader;

    thread_local! {pub static read: Mut<Scm> = Mut::new(Scm::func(_read))}

    fn _read(args: &[Scm]) -> Scm {
        if args.len() == 0 {
            match from_reader(stdin()) {
                Ok(x) => x,
                Err(e) if e.is_eof() => Scm::eof(),
                Err(e) => panic!("{:?}", e),
            }
        } else {
            args[0]
                .with_input_port(|port| match from_reader(port) {
                    Ok(x) => x,
                    Err(e) if e.is_eof() => Scm::eof(),
                    Err(e) => panic!("{:?}", e),
                })
                .unwrap_or(Scm::eof())
        }
    }
}
