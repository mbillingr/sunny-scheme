use super::EvaluatesTo;
use crate::frontend::error::Error as FrontendError;
use crate::language_tests::given;
use hamcrest2::assert_that;
use hamcrest2::prelude::*;

#[test]
fn library_definition_evaluates_to_void() {
    assert_that!(SIMPLE_LIBRARY_DEFINITION, EvaluatesTo::void());
}

#[test]
fn can_import_after_library_was_defined() {
    assert_that!(
        vec![SIMPLE_LIBRARY_DEFINITION, "(import (foo bar))"],
        EvaluatesTo::void()
    );
}

#[test]
fn reexports_target_same_variable() {
    assert_that!(
        vec![
            // todo: use renaming import or export when they are implemented
            "(define-library (foo) (import (sunny core)) (export x) (begin (define x 1)))",
            "(define-library (bar) (export x) (import (foo)))",
            "(import (bar))",
            "(set! x 42)",
            "(import (foo))",
            "x",
        ],
        EvaluatesTo::the_integer(42)
    );
}

#[test]
fn can_import_macros_from_library() {
    assert_that!(
        vec![
            SIMPLE_LIBRARY_DEFINITION,
            "(import (foo bar))",
            "(get-private)"
        ],
        EvaluatesTo::the_integer(123)
    );
}

#[test]
fn importing_nonexisting_library_fails() {
    assert_that!(
        "(import (foo bar))",
        EvaluatesTo::the_frontend_error(FrontendError::UnknownLibrary)
    );
}

#[test]
fn import_loads_library_from_file_system() {
    assert_that!(
        given()
            .file_contains("foo/bar.sld", SIMPLE_LIBRARY_DEFINITION.as_bytes())
            .then("(import (foo bar))"),
        EvaluatesTo::void()
    );
}

#[test]
fn import_loads_library_and_imports_values() {
    assert_that!(
        given()
            .file_contains("foo/bar.sld", SIMPLE_LIBRARY_DEFINITION.as_bytes())
            .then(vec!["(import (foo bar))", "baz"]),
        EvaluatesTo::the_integer(42)
    );
}

#[test]
fn can_import_standard_libraries() {
    assert_that!("(import (sunny time))", EvaluatesTo::void());
}

#[test]
fn imports_from_library_definitions_are_resolved_correctly() {
    assert_that!(
        given()
            .file_contains("lib/b.sld", LIB_B.as_bytes())
            .file_contains("lib/a.sld", LIB_A.as_bytes())
            .then("(import (lib b))"),
        EvaluatesTo::void()
    );
}

const SIMPLE_LIBRARY_DEFINITION: &str = "
(define-library (foo bar)
    (import (sunny core))
    (export baz get-private)
    (begin
        (define baz 42)
        (define private 123)
        (define-syntax get-private (simple-macro () private))
    )
)";

const LIB_A: &str = "
(define-library (lib a)
    (import (sunny core))
    (export foo)
    (begin
        (define foo 42)
    )
)";

const LIB_B: &str = "
(define-library (lib b)
    (import (sunny core) (lib a))
    (export foo bar)
    (begin
        (define bar 123)
    )
)";
