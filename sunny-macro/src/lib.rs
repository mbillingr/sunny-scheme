/// Evaluate a single Scheme expression.
#[macro_export]
macro_rules! scm {
    ((cond $($clauses:tt)+)) => { scm_cond![$($clauses)+] };
    ((define $($args:tt)*)) => { scm_definition![$($args)+] };
    ((set! $name:ident $value:tt)) => { $name.set(scm![$value]); };
    ((let $defs:tt $($body:tt)+)) => { scm_let![$defs $($body)+] };
    ((if $($args:tt)*)) => { scm_alternative![$($args)+] };
    ((lambda $($args_and_body:tt)+)) => { scm_abstraction![$($args_and_body)+] };
    ((begin $($body:tt)+)) => { scm_sequence![$($body)+] };
    ((quote $arg:tt)) => { scm_quote![$arg] };
    (($($func_and_args:tt)+)) => { scm_application![$($func_and_args)+] };

    (nil) => { Scm::Nil };
    (()) => { Scm::Nil };
    (#t) => { Scm::True };
    (true) => { Scm::True };
    (#f) => { Scm::False };
    (false) => { Scm::False };

    ($x:ident) => {Scm::from(&$x)};
    ($x:expr) => {Scm::from($x)};
}

#[macro_export]
macro_rules! scm_cond {
    ((else $($body:tt)+)) => {
        scm_sequence![$($body)+]
    };

    (($test:tt $($body:tt)+) $($rest:tt)+) =>  {
        scm_alternative![$test (begin $($body)+) (cond $($rest)+)]
    };
}

#[macro_export]
macro_rules! scm_let {
    (($(($var:ident $val:tt))*) $($body:tt)+) => {
        scm_application![(lambda ($($var)*) $($body)+) $($val)*]
    };
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
        let $name = Scm::Nil.into_boxed();
        $name.set(scm![$value]);
    };
}

#[macro_export]
macro_rules! scm_alternative {
    ($cond:tt $cons:tt $alt:tt) => {
        if scm![$cond].is_true() {
            scm![$cons]
        } else {
            scm![$alt]
        }
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

    use sunny_core::{add, cons, less, square, sub};
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

    #[test]
    fn nil_value() {
        assert_eq!(scm![nil], Scm::Nil);
        assert_eq!(scm![()], Scm::Nil);
    }

    #[test]
    fn bool_values() {
        assert_eq!(scm![#t], Scm::True);
        assert_eq!(scm![#f], Scm::False);
    }

    #[test]
    fn alternative() {
        assert_eq!(scm![(if true 1 2)], Scm::Int(1));
        assert_eq!(scm![(if false 1 2)], Scm::Int(2));
        assert_eq!(scm![(if 0 1 2)], Scm::Int(1));
        assert_eq!(scm![(if nil 1 2)], Scm::Int(2));
    }

    #[test]
    #[cfg(feature = "scm_copy")]
    fn fibonacci() {
        scm![(define (fib n)
                 (if (less n 2)
                     1
                     (add (fib (sub n 1))
                          (fib (sub n 2)))))];
        assert_eq!(scm![(fib 5)], Scm::Int(8));
    }

    #[test]
    fn cond_only_else() {
        assert_eq!(scm![(cond (else 42))], Scm::Int(42));
    }

    #[test]
    fn cond_true_or_else() {
        assert_eq!(scm![(cond (true 1) (else 2))], Scm::Int(1));
    }

    #[test]
    fn cond_false_or_else() {
        assert_eq!(scm![(cond (false 1) (else 2))], Scm::Int(2));
    }

    #[test]
    fn cond_multiple() {
        scm![(define (count n)
                (cond ((less n 0) (quote negative))
                      ((less n 1) (quote zero))
                      ((less n 5) (quote few))
                      ((less n 10) (quote some))
                      (else (quote many))))];
        assert_eq!(scm![(count 0)], Scm::symbol("zero"));
        assert_eq!(scm![(count 3)], Scm::symbol("few"));
        assert_eq!(scm![(count 7)], Scm::symbol("some"));
        assert_eq!(scm![(count 11)], Scm::symbol("many"));
    }

    #[test]
    fn let_no_vars() {
        assert_eq!(scm![(let () 0)], Scm::Int(0))
    }

    #[test]
    fn let_one_var() {
        assert_eq!(scm![(let ((x 1)) x)], Scm::Int(1))
    }

    #[test]
    fn let_two_vars_do_not_interfere() {
        scm![(define x 1)];
        scm![(define y 2)];
        assert_eq!(scm![(let ((x y) (y x)) (cons x y))], Scm::pair(2, 1))
    }
}
