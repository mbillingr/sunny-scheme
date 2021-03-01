use super::EvaluatesTo;
use hamcrest2::assert_that;
use hamcrest2::prelude::*;

#[test]
fn reading_undefined_globals_is_an_error() {
    assert_that!("x", EvaluatesTo::an_error());
}

#[test]
fn assigning_a_global_defines_it() {
    assert_that!("(begin (set! x 1) x)", EvaluatesTo::the_integer(1));
}

#[test]
fn assignment_evaluates_to_void() {
    assert_that!("(set! x 1)", EvaluatesTo::void());
}
