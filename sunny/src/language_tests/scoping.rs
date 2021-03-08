use super::EvaluatesTo;
use hamcrest2::assert_that;
use hamcrest2::prelude::*;

#[test]
fn can_get_value_of_variable_in_lexical_scope() {
    assert_that!("((lambda (x) x) 1)", EvaluatesTo::the_integer(1));
}

#[test]
fn can_get_value_of_different_variables_in_lexical_scope() {
    assert_that!("((lambda (x y) x) 1 2)", EvaluatesTo::the_integer(1));
    assert_that!("((lambda (x y) y) 1 2)", EvaluatesTo::the_integer(2));
}

#[test]
fn can_get_value_of_global_scope() {
    assert_that!(
        "(begin (set! x 1) ((lambda () x)))",
        EvaluatesTo::the_integer(1)
    );
}

#[test]
fn can_get_value_of_outer_scope() {
    assert_that!(
        "((lambda (x) ((lambda (y) x) 2)) 1)",
        EvaluatesTo::the_integer(1)
    );
}

#[test]
fn can_capture_value_of_outer_scope() {
    assert_that!(
        "(((lambda (x) (lambda () x)) 1))",
        EvaluatesTo::the_integer(1)
    );
}
