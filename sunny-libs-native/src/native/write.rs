pub fn initialize() {}
pub mod exports {
    use sunny_core::{self, Mut, Scm};

    pub fn display(args: &[Scm]) -> Scm {
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

    pub fn newline(args: &[Scm]) -> Scm {
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

    pub fn write(args: &[Scm]) -> Scm {
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
