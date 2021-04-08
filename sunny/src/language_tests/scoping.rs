use super::EvaluatesTo;
use hamcrest2::assert_that;
use hamcrest2::prelude::*;

#[test]
fn can_get_value_of_variable_in_lexical_scope() {
    assert_that!("((lambda (x) x) 1)", EvaluatesTo::the_integer(1));
}

#[test]
fn can_set_value_of_variable_in_lexical_scope() {
    assert_that!("((lambda (x) (set! x 2) x) 1)", EvaluatesTo::the_integer(2));
}

#[test]
fn can_get_value_of_different_variables_in_lexical_scope() {
    assert_that!("((lambda (x y) x) 1 2)", EvaluatesTo::the_integer(1));
    assert_that!("((lambda (x y) y) 1 2)", EvaluatesTo::the_integer(2));
}

#[test]
fn can_set_value_of_different_variables_in_lexical_scope() {
    assert_that!(
        "((lambda (x y) (set! x 3) x) 1 2)",
        EvaluatesTo::the_integer(3)
    );
    assert_that!(
        "((lambda (x y) (set! x 3) y) 1 2)",
        EvaluatesTo::the_integer(2)
    );
    assert_that!(
        "((lambda (x y) (set! y 3) x) 1 2)",
        EvaluatesTo::the_integer(1)
    );
    assert_that!(
        "((lambda (x y) (set! y 3) y) 1 2)",
        EvaluatesTo::the_integer(3)
    );
}

#[test]
fn get_value_of_undefined_variable_is_an_error() {
    assert_that!("((lambda () x))", EvaluatesTo::an_error());
}

#[test]
fn can_get_value_of_global_scope() {
    assert_that!(
        "(begin (define x 1) ((lambda () x)))",
        EvaluatesTo::the_integer(1)
    );
}

#[test]
fn can_set_value_of_global_scope() {
    assert_that!(
        "(begin (define x 1) ((lambda () (set! x 2))) x)",
        EvaluatesTo::the_integer(2)
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
fn can_set_value_of_outer_scope() {
    assert_that!(
        "((lambda (x) ((lambda () (set! x 2))) x) 1)",
        EvaluatesTo::the_integer(2)
    );
}

#[test]
fn can_capture_value_of_outer_scope() {
    assert_that!(
        "(((lambda (x) (lambda () x)) 1))",
        EvaluatesTo::the_integer(1)
    );
}

#[test]
fn can_access_values_of_various_nested_scopes() {
    assert_that!(
        "((lambda (a)
            ((lambda (b)
                ((lambda (c)
                    ((lambda x x) a b c))
                 3))
             2))
          1)",
        EvaluatesTo::the_list(vec![1, 2, 3])
    );
}

#[test]
fn can_access_values_through_empty_scopes() {
    assert_that!(
        "((lambda ()
            ((lambda (a b)
                ((lambda (c)
                    ((lambda x x) a b c))
                 3))
             1 2))
          )",
        EvaluatesTo::the_list(vec![1, 2, 3])
    );
}

#[test]
fn let_binds_new_values() {
    assert_that!(
        vec![
            "(define x 42)",
            "(let ((x 1) (y 2))
               (+ x y))"
        ],
        EvaluatesTo::the_integer(3)
    );
}

#[test]
fn let_preserves_shadowed_values() {
    assert_that!(
        vec![
            "(define x 42)",
            "(let ((x 1) (y 2))
               (+ x y))",
            "x"
        ],
        EvaluatesTo::the_integer(42)
    );
}
