use super::EvaluatesTo;
use hamcrest2::assert_that;
use hamcrest2::prelude::*;

#[test]
fn apply_function_to_empty_list_makes_nullary_call() {
    assert_that!("(apply (lambda () 0) '())", EvaluatesTo::the_integer(0));
}

#[test]
fn apply_puts_items_in_correct_order() {
    assert_that!(
        "(apply (lambda x x) '(1 2 3))",
        EvaluatesTo::the_list(vec![1, 2, 3])
    );
}

#[test]
fn apply_function_to_list_makes_nary_call() {
    assert_that!(
        "(apply (lambda (a b c) b) '(1 2 3))",
        EvaluatesTo::the_integer(2)
    );
}

#[test]
fn call_with_zero_values() {
    assert_that!("(call-with-values values (lambda x x))", EvaluatesTo::nil());
}

#[test]
fn call_with_one_value() {
    assert_that!(
        "(call-with-values (lambda () (values 1)) (lambda x x))",
        EvaluatesTo::the_list(vec![1])
    );
}

#[test]
fn call_with_three_value() {
    assert_that!(
        "(call-with-values (lambda () (values 1 2 3)) (lambda x x))",
        EvaluatesTo::the_list(vec![1, 2, 3])
    );
}

#[test]
fn call_with_one_normal_value() {
    assert_that!(
        "(call-with-values (lambda () 1) (lambda x x))",
        EvaluatesTo::the_list(vec![1])
    );
}
