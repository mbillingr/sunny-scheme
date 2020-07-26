#[macro_export]
macro_rules! scm {
    ((define $($args:tt)*)) => {
        scm_definition![$($args)+]
    };

    ((set! $name:ident $value:tt)) => {
        $name.set(scm![$value]);
    };

    ((lambda $($args_and_body:tt)+)) => {
        scm_abstraction![$($args_and_body)+]
    };

    ((begin $($body:tt)+)) => {
        scm_sequence![$($body)+]
    };

    ((quote $arg:tt)) => {
        scm_quote![$arg]
    };

    (($($func_and_args:tt)+)) => {
        scm_application![$($func_and_args)+]
     };

    ($x:ident) => {Scm::from(&$x)};

    ($x:expr) => {Scm::from($x)};
}

#[macro_export]
macro_rules! scm_quote {
    (($first:tt $($rest:tt)*)) => {
        Scm::pair(
            scm_quote![$first],
            scm_quote![($($rest)*)]
        )
    };

    (()) => {
        Scm::Nil
    };

    ($name:ident) => {
        Scm::symbol(stringify!($name))
    };

    ($x:expr) => {
        scm![$x]
    };
}

#[macro_export]
macro_rules! scm_definition {
    (($name:ident $($param:ident)*) $($body:tt)+) => {
        scm_definition![$name (lambda ($($param)*) $($body)+)]
    };

    ($name:ident $value:tt) => {
        #[allow(unused_mut)]
        let mut $name = scm![$value].into_boxed();
    };
}

#[macro_export]
macro_rules! scm_sequence {
    ($($body:tt)+) => {
        { $(scm![$body]);+ }
    };
}

#[macro_export]
macro_rules! scm_abstraction {
    (($($param:ident)*) $($body:tt)+) => {
        Scm::func(move |args: &[Scm]| match args {
            [$($param),*] => {
                //$(#[allow(unused_mut)] let mut $param: Scm = $param.clone();)*
                $(let $param: BoxedScm = $param.clone().into_boxed();)*
                scm_sequence![$($body)+]
            }
            _ => panic!("Incorrect arity")
        })
    };
}

#[macro_export]
macro_rules! scm_application {
    ((lambda ($($param:ident)*) $($body:tt)+) $($arg:tt)*) => {
        // optimize the special case of a lambda form being directly
        // applied (no need to wrap it in an Scm instance)
        (|$($param: &Scm),*| {
            //$(#[allow(unused_mut)] let mut $param: Scm = $param.clone();)*
            $(let $param: BoxedScm = $param.clone().into_boxed();)*
            scm_sequence![$($body)+]
        })($(&scm![$arg]),*)
    };

    ($func:tt $($arg:tt)*) => {
        scm![$func].invoke(&[$(scm![$arg]),*])
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    use sunny_core::{add, cons, square};
    use sunny_core::{BoxedScm, Scm};

    #[test]
    fn integer_constant() {
        assert_eq!(scm![1], Scm::Int(1));
    }

    #[test]
    fn lambda_simple() {
        assert_eq!(scm![(lambda () 0)].invoke(&[]), Scm::Int(0));
    }

    #[test]
    fn lambda_simple_sequence() {
        assert_eq!(scm![(lambda () 1 2 3)].invoke(&[]), Scm::Int(3));
    }

    #[test]
    fn lambda_identity() {
        assert_eq!(scm![(lambda (x) x)].invoke(&[Scm::Int(1)]), Scm::Int(1));
    }

    #[test]
    fn apply_lambda_simple() {
        assert_eq!(scm![((lambda () 0))], Scm::Int(0));
    }

    #[test]
    fn apply_lambda_identity() {
        assert_eq!(scm![((lambda (x) x) 1)], Scm::Int(1));
    }

    #[test]
    fn apply_binary_primitive() {
        assert_eq!(scm![(add 1 2)], Scm::Int(3));
    }

    #[test]
    fn apply_unary_primitive() {
        assert_eq!(scm![(square 3)], Scm::Int(9));
    }

    #[test]
    fn apply_abstraction() {
        assert_eq!(scm![((lambda (x) (add x x)) 3)], Scm::Int(6));
    }

    #[test]
    fn pair() {
        assert_eq!(scm![(cons 1 2)], Scm::pair(1, 2));
    }

    #[test]
    fn quote() {
        assert_eq!(scm![(quote (1 2))], Scm::pair(1, Scm::pair(2, Scm::Nil)));
    }

    #[test]
    fn symbol() {
        assert_eq!(scm![(quote foo)], Scm::symbol("foo"));
    }

    #[test]
    fn first_order_abstractions() {
        assert_eq!(scm![((lambda (f) (f)) (lambda () 1))], Scm::Int(1));
    }

    #[test]
    fn first_order_primitives() {
        assert_eq!(scm![((lambda (f) (f 3)) square)], Scm::Int(9));
    }

    #[test]
    fn apply_abstraction_two_args() {
        assert_eq!(scm![((lambda (a b) (add a b)) 1 2)], Scm::Int(3));
    }

    #[test]
    fn apply_abstraction_four_args() {
        assert_eq!(
            scm![((lambda (a b c d) (add (add a b) (add c d))) 1 2 3 4)],
            Scm::Int(10)
        );
    }

    #[test]
    fn sequence() {
        assert_eq!(scm![(begin 1 2 3)], Scm::Int(3));
    }

    #[test]
    fn top_level_define() {
        scm![(define x 42)];
        assert_eq!(x.get(), Scm::Int(42));
    }

    #[test]
    fn top_level_define_expression() {
        scm![(define x (add 1 2))];
        assert_eq!(x.get(), Scm::Int(3));
    }

    #[test]
    fn define_in_sequence() {
        assert_eq!(
            scm![(begin (define x 42)
                        x)],
            Scm::Int(42)
        );
    }

    #[test]
    fn define_function() {
        assert_eq!(
            scm![(begin (define (foo) 42)
                        (foo))],
            Scm::Int(42)
        );
    }

    #[test]
    #[allow(unused)]
    fn mutate_definition() {
        assert_eq!(
            scm![(begin (define x 1)
                        (set! x 2)
                        x)],
            Scm::Int(2)
        );
    }

    #[test]
    #[allow(unused)]
    fn mutate_local() {
        scm![(define (foo x) (set! x 0) x)];
        assert_eq!(scm![(foo 1)], Scm::Int(0));
    }

    #[test]
    #[allow(unused)]
    fn mutate_local_fixlet() {
        assert_eq!(scm![((lambda (x) (set! x 0) x) 1)], Scm::Int(0));
    }

    #[test]
    fn closure() {
        scm![(define (close x) (lambda () x))];
        scm![(define access (close 5))];
        assert_eq!(scm![(access)], Scm::Int(5));
        assert_eq!(scm![(access)], Scm::Int(5));
    }

    // does not work with non-Copy Scm
    #[test]
    #[cfg(feature = "scm_copy")]
    fn shared_var_in_two_closures() {
        scm![(define (close x) (lambda () x) (lambda () x))];
        scm![(define access (close 5))];
        assert_eq!(scm![(access)], Scm::Int(5));
        assert_eq!(scm![(access)], Scm::Int(5));
    }

    #[test]
    fn mutable_closure() {
        scm![(define (close x) (lambda () (set! x (add x 1)) x))];
        scm![(define access (close 0))];
        assert_eq!(scm![(access)], Scm::Int(1));
        assert_eq!(scm![(access)], Scm::Int(2));
        assert_eq!(scm![(access)], Scm::Int(3));
    }
}
