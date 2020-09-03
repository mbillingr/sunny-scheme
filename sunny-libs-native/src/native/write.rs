pub fn initialize() {}
pub mod exports {
    use sunny_core::{self, Mut, Scm};

    thread_local! {pub static display: Mut<Scm> = Mut::new(Scm::func(_display))}
    thread_local! {pub static newline: Mut<Scm> = Mut::new(Scm::func(_newline))}
    thread_local! {pub static write: Mut<Scm> = Mut::new(Scm::func(_write))}

    fn _display(args: &[Scm]) -> Scm {
        if args.len() == 1 {
            print!("{}", args[0]);
            Scm::Nil
        } else {
            args[1]
                .with_output_port(|port| {
                    write!(port, "{}", args[0]).unwrap();
                })
                .unwrap();
            Scm::Nil
        }
    }

    fn _newline(args: &[Scm]) -> Scm {
        if args.len() == 0 {
            println!();
            Scm::Nil
        } else {
            args[0]
                .with_output_port(|port| {
                    write!(port, "\n").unwrap();
                })
                .unwrap();
            Scm::Nil
        }
    }

    fn _write(args: &[Scm]) -> Scm {
        if args.len() == 1 {
            print!("{:?}", args[0]);
            Scm::Nil
        } else {
            args[1]
                .with_output_port(|port| {
                    write!(port, "{:?}", args[0]).unwrap();
                })
                .unwrap();
            Scm::Nil
        }
    }
}
