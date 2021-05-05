use super::EvaluatesTo;
use hamcrest2::assert_that;
use hamcrest2::prelude::*;

#[test]
fn boolean_predicate_is_true_only_for_bool_types() {
    assert_that!("(boolean? #t)", EvaluatesTo::the_boolean(true));
    assert_that!("(boolean? #f)", EvaluatesTo::the_boolean(true));
    assert_that!("(boolean? '())", EvaluatesTo::the_boolean(false));
    assert_that!("(boolean? 0)", EvaluatesTo::the_boolean(false));
}

#[test]
fn bytevector_predicate_is_true_only_for_bytevector_types() {
    assert_that!("(bytevector? (bytevector))", EvaluatesTo::the_boolean(true));
    assert_that!(
        "(bytevector? (bytevector 1 2 3))",
        EvaluatesTo::the_boolean(true)
    );
    assert_that!("(bytevector? '())", EvaluatesTo::the_boolean(false));
    assert_that!("(bytevector? 0)", EvaluatesTo::the_boolean(false));
}

#[test]
fn char_predicate_is_true_only_for_character_types() {
    assert_that!(r"(char? #\a)", EvaluatesTo::the_boolean(true));
    assert_that!(r"(char? #\x03bb)", EvaluatesTo::the_boolean(true));
    assert_that!(r"(char? #\alarm)", EvaluatesTo::the_boolean(true));
    assert_that!("(char? '())", EvaluatesTo::the_boolean(false));
    assert_that!("(char? 0)", EvaluatesTo::the_boolean(false));
}

#[test]
fn eof_predicate_is_true_only_for_eof_objects() {
    assert_that!(
        r"(eof-object? (eof-object))",
        EvaluatesTo::the_boolean(true)
    );
    assert_that!("(eof-object? '())", EvaluatesTo::the_boolean(false));
    assert_that!("(eof-object? 0)", EvaluatesTo::the_boolean(false));
}

#[test]
fn null_predicate_is_true_only_for_null_the_empty_list() {
    assert_that!("(null? '())", EvaluatesTo::the_boolean(true));
    assert_that!("(null? 0)", EvaluatesTo::the_boolean(false));
    assert_that!("(null? '(1 . 2))", EvaluatesTo::the_boolean(false));
}

#[test]
fn number_predicate_is_true_only_for_numbers() {
    assert_that!("(number? 0)", EvaluatesTo::the_boolean(true));
    assert_that!("(number? 1)", EvaluatesTo::the_boolean(true));
    assert_that!("(number? 2.3)", EvaluatesTo::the_boolean(true));
    assert_that!("(number? '())", EvaluatesTo::the_boolean(false));
    assert_that!("(number? 'x)", EvaluatesTo::the_boolean(false));
}
