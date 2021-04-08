use super::EvaluatesTo;
use hamcrest2::assert_that;
use hamcrest2::prelude::*;

#[test]
fn import_statement_accepts_multiple_libraries() {
    assert_that!(
        "(import (scheme base)
                 (scheme base))",
        EvaluatesTo::void()
    );
}

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

#[test]
fn inner_defines_appear_in_the_right_order() {
    assert_that!(
        vec![
            "(define (foo) (define a 1) (define b 2) (define c 3) ((lambda x x) a b c))",
            "(foo)"
        ],
        EvaluatesTo::the_list(vec![1, 2, 3])
    );
}

#[test]
fn inner_defines_support_mutual_recursion() {
    assert_that!(
        vec![
            "(define (is-even? n) (define (even? n) (if (eq? n 0) #t (odd? (- n 1)))) (define (odd? n) (if (eq? n 0) #f (even? (- n 1)))) (even? n))",
            "(is-even? 5)",],
        EvaluatesTo::the_boolean(false)
    );
}
