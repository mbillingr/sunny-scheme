use super::EvaluatesTo;
use hamcrest2::assert_that;
use hamcrest2::prelude::*;

#[test]
fn using_extended_identifier_characters_is_not_an_error() {
    // R7RS section 2.1
    assert_that!(
        vec![
            "(define ! 0)",
            "(define $ 0)",
            "(define % 0)",
            "(define & 0)",
            "(define * 0)",
            "(define + 0)",
            "(define - 0)",
            "(define .. 0)", // a single . has special meaning
            "(define / 0)",
            "(define : 0)",
            "(define < 0)",
            "(define = 0)",
            "(define > 0)",
            "(define ? 0)",
            "(define @ 0)",
            "(define ^ 0)",
            "(define _ 0)",
            "(define ~ 0)",
        ],
        EvaluatesTo::void()
    );
}

#[test]
fn pipes_escape_verbatim_identifiers() {
    // R7RS section 2.1
    assert_that!(
        vec![
            "(define || 0)",
            "(define |.| 0)",
            "(define |Hello World| 0)",
        ],
        EvaluatesTo::void()
    );
}

#[test]
fn line_comments_are_ignored() {
    // R7RS section 2.2
    assert_that!("1 ; 2 3", EvaluatesTo::the_integer(1));
}

#[test]
fn commented_data_are_ignored() {
    // R7RS section 2.2
    assert_that!("1 #;2 3", EvaluatesTo::the_integer(3));
    assert_that!("1 #; 2 3", EvaluatesTo::the_integer(3));
    assert_that!("'(1 #; 2 3)", EvaluatesTo::the_list(vec![1, 3]));
}

#[test]
fn block_comments_are_ignored() {
    // R7RS section 2.2
    assert_that!("1 #| |# 3", EvaluatesTo::the_integer(3));
    assert_that!("1 #| 2 |# 3", EvaluatesTo::the_integer(3));
}

#[test]
fn block_comments_match_shortest_possibility() {
    // R7RS section 2.2
    assert_that!("#| 1 |# 2 #| 3 |#", EvaluatesTo::the_integer(2));
}

#[test]
fn can_label_data() {
    // R7RS section 2.4
    assert_that!("#42=0", EvaluatesTo::the_integer(0));
}

#[test]
fn can_reference_datum_labels() {
    // R7RS section 2.4
    assert_that!("'(#42=0 #42#)", EvaluatesTo::the_list(vec![0, 0]));
}
