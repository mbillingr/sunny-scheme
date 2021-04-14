use super::EvaluatesTo;
use hamcrest2::assert_that;
use hamcrest2::prelude::*;

// TODO: uncomment test cases when their prerequisites are implemented

/// Assert equality given predicate.
///
/// The two arguments are evaluated in separate code segments to make
/// sure they are not constant-folded.
macro_rules! assert_equivalence {
    ($pred:expr, $a:expr, $b: expr) => {
        assert_that!(
            vec![
                concat!("(define a ", $a, ")"),
                concat!("(define b ", $b, ")"),
                concat!(r"(", $pred, " a b)")
            ],
            EvaluatesTo::the_boolean(true)
        );
    };
}

/// Assert inequality given predicate.
///
/// The two arguments are evaluated in separate code segments to make
/// sure they are not constant-folded.
macro_rules! assert_different {
    ($pred:expr, $a:expr, $b: expr) => {
        assert_that!(
            vec![
                concat!("(define a ", $a, ")"),
                concat!("(define b ", $b, ")"),
                concat!(r"(", $pred, " a b)")
            ],
            EvaluatesTo::the_boolean(false)
        );
    };
}

#[test]
fn eqv() {
    assert_equivalence!("eqv?", "#t", "#t");
    assert_equivalence!("eqv?", "#f", "#f");
    assert_equivalence!("eqv?", "'foo", "'foo");
    assert_equivalence!("eqv?", "2", "2");
    //assert_equivalence!("eqv?", "2.0", "2.0");
    //assert_equivalence!("eqv?", r"#\b", r"#\b");
    assert_equivalence!("eqv?", "'()", "'()");
    assert_that!(
        r"(let ((x (cons 1 2))) (eqv? x x))",
        EvaluatesTo::the_boolean(true)
    );
    assert_that!(
        "(let ((x \"foo\")) (eqv? x x))",
        EvaluatesTo::the_boolean(true)
    );
    //assert_equivalence!("eq?", "car", "car");

    assert_different!("eqv?", "'()", "#f");
    assert_different!("eqv?", "#t", "#f");
    assert_different!("eqv?", "'foo", "'bar");
    //assert_different!("eqv?", "2", "2.0");
    assert_different!("eqv?", "2", "3");
    //assert_different!("eqv?", "2.0", "3.0");
    //assert_equivalence!("eqv?", r"#\b", r"#\c");
    assert_different!("eqv?", "'()", "'(())");
    assert_different!("eqv?", "(cons 1 2)", "(cons 1 2)");
    //assert_different!("eqv?", "(vector 1 2)", "(vector 1 2)");
    assert_different!("eqv?", "\"foo\"", "\"bar\"");
    assert_different!("eqv?", "(lambda () 1)", "(lambda () 2)");

    assert_that!(
        vec![
            "(define (gen-counter) (let ((n 0)) (lambda () (set! n (+ n 1)) n)))",
            "(let ((g (gen-counter))) (eqv? g g))"
        ],
        EvaluatesTo::the_boolean(true)
    );

    assert_that!(
        vec![
            "(define (gen-counter) (let ((n 0)) (lambda () (set! n (+ n 1)) n)))",
            "(eqv? (gen-counter) (gen-counter))"
        ],
        EvaluatesTo::the_boolean(false)
    );
}

#[test]
fn eq() {
    assert_equivalence!("eq?", "#t", "#t");
    assert_equivalence!("eq?", "#f", "#f");
    assert_equivalence!("eq?", "'foo", "'foo");
    assert_equivalence!("eq?", "'()", "'()");
    assert_that!(
        r"(let ((x (cons 1 2))) (eq? x x))",
        EvaluatesTo::the_boolean(true)
    );
    assert_that!(
        "(let ((x \"foo\")) (eq? x x))",
        EvaluatesTo::the_boolean(true)
    );
    //assert_equivalence!("eq?", "car", "car");

    assert_different!("eq?", "'()", "#f");
    assert_different!("eq?", "#t", "#f");
    assert_different!("eq?", "'foo", "'bar");
    //assert_different!("eq?", "2", "2.0");
    assert_different!("eq?", "2", "3");
    //assert_different!("eq?", "2.0", "3.0");
    //assert_equivalence!("eq?", r"#\b", r"#\c");
    assert_different!("eq?", "'()", "'(())");
    assert_different!("eq?", "(cons 1 2)", "(cons 1 2)");
    //assert_different!("eq?", "(vector 1 2)", "(vector 1 2)");
    assert_different!("eq?", "\"foo\"", "\"bar\"");
    assert_different!("eq?", "(lambda () 1)", "(lambda () 2)");

    assert_that!(
        vec![
            "(define (gen-counter) (let ((n 0)) (lambda () (set! n (+ n 1)) n)))",
            "(let ((g (gen-counter))) (eq? g g))"
        ],
        EvaluatesTo::the_boolean(true)
    );

    assert_that!(
        vec![
            "(define (gen-counter) (let ((n 0)) (lambda () (set! n (+ n 1)) n)))",
            "(eq? (gen-counter) (gen-counter))"
        ],
        EvaluatesTo::the_boolean(false)
    );
}

#[test]
fn equal() {
    assert_equivalence!("equal?", "#t", "#t");
    assert_equivalence!("equal?", "#f", "#f");
    assert_equivalence!("equal?", "'foo", "'foo");
    assert_equivalence!("equal?", "\"foo\"", "\"foo\"");
    assert_equivalence!("equal?", "2", "2");
    //assert_equivalence!("equal?", "2.0", "2.0");
    //assert_equivalence!("equal?", r"#\b", r"#\b");
    assert_equivalence!("equal?", "'()", "'()");
    assert_equivalence!("equal?", "(cons 1 2)", "(cons 1 2)");
    //assert_equivalence!("equal?", "(vector 1 2)", "(vector 1 2)");
    assert_that!(
        "(let ((x \"foo\")) (equal? x x))",
        EvaluatesTo::the_boolean(true)
    );
    //assert_equivalence!("eq?", "car", "car");

    assert_different!("equal?", "'()", "#f");
    assert_different!("equal?", "#t", "#f");
    assert_different!("equal?", "'foo", "'bar");
    //assert_different!("equal?", "2", "2.0");
    assert_different!("equal?", "2", "3");
    //assert_different!("equal?", "2.0", "3.0");
    //assert_equivalence!("equal?", r"#\b", r"#\c");
    assert_different!("equal?", "'()", "'(())");
    assert_different!("equal?", "\"foo\"", "\"bar\"");
    assert_different!("equal?", "(lambda () 1)", "(lambda () 2)");
}

#[test]
fn equivalence_on_distinct_objects_with_same_value() {
    // R7RS does not specify if this should be true or false.
    // It is an implementation detail that numbers are boxed as separate objects.
    assert_that!(
        vec![r"(define a 3)", r"(define b 3)", r"(eq? a b)"],
        EvaluatesTo::the_boolean(false)
    );
}
