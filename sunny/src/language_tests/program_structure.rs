use super::EvaluatesTo;
use hamcrest2::assert_that;
use hamcrest2::prelude::*;

#[test]
fn inner_defines_can_escape() {
    assert_that!(
        vec!["(define (foo) (define (bar) 0) bar)", "((foo))",],
        EvaluatesTo::the_integer(0)
    );
}

#[test]
fn inner_defines_dont_produce_globals() {
    assert_that!(
        vec!["(define (foo) (define (bar) 0) bar)", "(foo)", "(bar)",],
        EvaluatesTo::an_error()
    );
}
