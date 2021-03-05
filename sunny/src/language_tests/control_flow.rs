use super::EvaluatesTo;
use hamcrest2::assert_that;
use hamcrest2::prelude::*;

#[test]
fn empty_sequence_is_an_error() {
    assert_that!("(begin)", EvaluatesTo::an_error());
}

#[test]
fn singleton_sequence() {
    assert_that!("(begin 1)", EvaluatesTo::the_integer(1));
}

#[test]
fn sequence_evaluates_to_value_of_last_item() {
    assert_that!("(begin 1 2 3)", EvaluatesTo::the_integer(3));
}

#[test]
fn sequence_evaluates_all_side_effects_in_order() {
    assert_that!(
        "(begin (set! x 1) (set! x (cons x 2)) x)",
        EvaluatesTo::the_pair(1, 2)
    );
}

#[test]
fn if_evaluates_to_first_expression_on_true() {
    assert_that!("(if #t 1 2)", EvaluatesTo::the_integer(1));
}

#[test]
fn if_evaluates_to_second_expression_on_false() {
    assert_that!("(if #f 1 2)", EvaluatesTo::the_integer(2));
}

#[test]
fn if_treats_any_non_false_condition_as_true() {
    assert_that!("(if '() 1 2)", EvaluatesTo::the_integer(1));
}

#[test]
fn if_does_not_evaluate_the_false_branch_on_true() {
    assert_that!(
        "(begin (set! x 0) (if #t 'ok (set! x 1)) x)",
        EvaluatesTo::the_integer(0)
    );
}

#[test]
fn if_does_not_evaluate_the_true_branch_on_false() {
    assert_that!(
        "(begin (set! x 0) (if #f (set! x 1) 'ok) x)",
        EvaluatesTo::the_integer(0)
    );
}

#[test]
fn if_alternative_is_optional() {
    assert_that!("(if #t 1)", EvaluatesTo::the_integer(1));
}

#[test]
fn if_without_alternative_is_void_on_false() {
    assert_that!("(if #f 1)", EvaluatesTo::void());
}
