use scheme::write::exports::*;

pub mod exports {
    pub use super::globals::hello_minus_world;
    pub use super::newline;
}

mod globals {
    use super::*;
    thread_local! {#[allow(non_upper_case_globals)] pub static private: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL private"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static hello_minus_world: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL hello-world"))}
}

fn initialize() {
    {
        {
            // (define (hello-world) (private))
            globals::hello_minus_world.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 0 {
                            panic!("invalid arity")
                        }
                        // (letrec () (private))
                        {
                            // (private)
                            globals::private.with(|value| value.get()).invoke(&[])
                        }
                    })
                })
            })
        };
        // (define (private) (display "Hello, World!"))
        globals::private.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 0 {
                        panic!("invalid arity")
                    }
                    // (letrec () (display "Hello, World!"))
                    {
                        // (display "Hello, World!")
                        display
                            .with(|value| value.get())
                            .invoke(&[Scm::from("Hello, World!")])
                    }
                })
            })
        })
    };
}
