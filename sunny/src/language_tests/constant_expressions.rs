use super::EvaluatesTo;
use hamcrest2::assert_that;
use hamcrest2::prelude::*;

#[test]
fn an_empty_list_is_an_error() {
    assert_that!("()", EvaluatesTo::an_error());
}

#[test]
fn the_quoted_empty_list_is_nil() {
    assert_that!("'()", EvaluatesTo::nil());
}

#[test]
fn zero_integer() {
    assert_that!("0", EvaluatesTo::the_integer(0));
}

#[test]
fn small_positive_integers() {
    assert_that!("1", EvaluatesTo::the_integer(1));
    assert_that!("23", EvaluatesTo::the_integer(23));
    assert_that!("456", EvaluatesTo::the_integer(456));
}

#[test]
fn large_positive_integers() {
    assert_that!(
        "9223372036854775807",
        EvaluatesTo::the_integer(i64::max_value())
    );
}

#[test]
fn booleans() {
    assert_that!("#t", EvaluatesTo::the_boolean(true));
    assert_that!("#f", EvaluatesTo::the_boolean(false));
}

#[test]
fn quoted_symbols_evaluate_to_symbols() {
    assert_that!("'foo", EvaluatesTo::the_symbol("foo"));
}
