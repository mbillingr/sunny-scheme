#[macro_use]
pub mod ast;
pub mod environment;
pub mod error;
pub mod library;
pub mod syntactic_closure;
pub mod syntax_forms;

use crate::frontend::environment::Environment;
use crate::frontend::syntax_forms::Import;
use crate::frontend::{
    ast::AstNode, environment::Env, error::Result, syntax_forms::LibraryDefinition,
};
use sunny_sexpr_parser::{Scm, SourceMap};

pub trait SyntaxExpander: std::fmt::Debug {
    fn expand(&self, sexpr: &Scm, src_map: &SourceMap, env: &Env) -> Result<AstNode>;

    fn description(&self) -> String {
        format!("<native syntax {:p}>", self)
    }
}

pub fn base_environment(name: impl ToString) -> Env {
    let global = Environment::Empty
        .add_binding("define-library", LibraryDefinition)
        .add_binding("import", Import);

    Env::new(name.to_string(), global, Environment::Empty)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::environment::EnvBinding;
    use crate::frontend::error::Error;
    use crate::frontend::syntax_forms::{
        Assignment, Begin, Branch, Definition, Expression, Lambda, LibraryDefinition, Quotation,
        SyntaxDefinition,
    };
    use ast::Ast;
    use sunny_sexpr_parser::SourceLocation;

    fn minimal_syntax_environment(name: impl ToString) -> Env {
        let global = Environment::Empty
            .add_binding("begin", Begin)
            .add_binding("define", Definition)
            .add_binding("define-library", LibraryDefinition)
            .add_binding("define-syntax", SyntaxDefinition)
            .add_binding("if", Branch)
            .add_binding("import", Import)
            .add_binding("lambda", Lambda)
            .add_binding("quote", Quotation)
            .add_binding("set!", Assignment);

        Env::new(name.to_string(), global, Environment::Empty)
    }

    macro_rules! sexpr {
        ($t:ty:()) => { <$t>::null() };

        ($t:ty:($x:tt . $y:tt)) => {
            <$t>::cons(
                sexpr![$t:$x],
                sexpr![$t:$y],
            )
        };

        ($t:ty:($x:tt $($rest:tt)*)) => {
            <$t>::cons(
                sexpr![$t:$x],
                sexpr![$t:($($rest)*)],
            )
        };

        ($t:ty:[$x:expr]) => { <$t>::symbol($x) };

        ($t:ty:$x:ident) => { <$t>::symbol(stringify!($x)) };

        ($t:ty:$x:expr) => { <$t>::from($x) };
    }

    macro_rules! meaning_of {
        ($expr:tt) => {{
            let env = minimal_syntax_environment("test");
            meaning_of![env @ $expr]
        }};

        ($env:tt @ $expr:tt) => {{
            let sexpr = sexpr![Scm: $expr];
            Expression.expand(&sexpr.into(), &SourceMap::new(), &$env)
        }};
    }

    #[test]
    fn meaning_of_syntax_as_value_is_an_error() {
        assert_eq!(
            meaning_of![begin],
            Err(SourceLocation::new(Error::SyntaxAsValue))
        );
    }

    #[test]
    fn meaning_of_constant() {
        assert_eq!(meaning_of![1], Ok(ast!(const 1)));
    }

    #[test]
    fn meaning_of_global_symbol() {
        assert_eq!(meaning_of![x], Ok(ast!(gref "test.x")));
    }

    #[test]
    fn meaning_of_local_symbol() {
        assert_eq!(meaning_of![(lambda (x) x)], Ok(ast!(lambda 1 (ref 0))));
    }

    #[test]
    fn meaning_of_global_set() {
        assert_eq!(
            meaning_of![(["set!"] x 42)],
            Ok(ast!(gset "test.x" (const 42)))
        );
    }

    #[test]
    fn meaning_of_local_set() {
        assert_eq!(
            meaning_of![(lambda (x) ("set!" x 42))],
            Ok(ast!(lambda 1 (set 0 (const 42))))
        );
    }

    #[test]
    fn meaning_of_an_intrinsic() {
        let env = minimal_syntax_environment("test");
        env.add_global_binding(
            "cons",
            EnvBinding::Intrinsic("cons", 2, Box::new(EnvBinding::Variable)),
        );
        assert_eq!(
            meaning_of![env @ (cons 1 2)],
            Ok(ast!(intrinsic cons (const 1) (const 2)))
        );
    }

    #[test]
    fn meaning_of_quote() {
        assert_eq!(meaning_of![(quote x)], Ok(ast!(const "x")));
    }

    #[test]
    fn meaning_of_if() {
        assert_eq!(
            meaning_of![(if 1 2 3)],
            Ok(ast!(if (const 1) (const 2) (const 3)))
        );
    }

    #[test]
    fn meaning_of_trivial_lambda() {
        assert_eq!(meaning_of![(lambda () 0)], Ok(ast!(lambda 0 (const 0))));
    }

    #[test]
    fn meaning_of_lambda_application() {
        assert_eq!(
            meaning_of![((lambda () 1))],
            Ok(ast!(invoke (lambda 0 (const 1))))
        );
    }

    #[test]
    fn meaning_of_variable_application() {
        assert_eq!(meaning_of![(foo)], Ok(ast!(invoke (gref "test.foo"))));
    }

    #[test]
    fn meaning_of_lambda_identity() {
        assert_eq!(meaning_of![(lambda (x) x)], Ok(ast!(lambda 1 (ref 0))));
    }

    #[test]
    fn meaning_of_global_ref_in_lambda() {
        assert_eq!(
            meaning_of![(lambda (y) x)],
            Ok(ast!(lambda 1 (gref "test.x")))
        );
    }

    #[test]
    fn meaning_of_lambda_with_multiple_arguments() {
        assert_eq!(meaning_of![(lambda (x y) x)], Ok(ast!(lambda 2 (ref 0))));
        assert_eq!(meaning_of![(lambda (x y) y)], Ok(ast!(lambda 2 (ref 1))));
    }

    #[test]
    fn meaning_of_lambda_with_variable_arguments() {
        assert_eq!(meaning_of![(lambda x x)], Ok(ast!(lambda-var 0 (ref 0))));
        assert_eq!(
            meaning_of![(lambda (x . y) x)],
            Ok(ast!(lambda-var 1 (ref 0)))
        );
        assert_eq!(
            meaning_of![(lambda (x . y) y)],
            Ok(ast!(lambda-var 1 (ref 1)))
        );
    }

    #[test]
    fn meaning_of_begin() {
        assert_eq!(
            meaning_of![(begin 1 2 3)],
            Ok(ast!(begin (const 1) (const 2) (const 3)))
        );
    }

    #[test]
    fn meaning_of_singleton_begin_is_meaning_of_inner_expression() {
        assert_eq!(meaning_of![(begin 1)], Ok(ast!(const 1)));
    }

    #[test]
    fn meaning_of_value_definition() {
        assert_eq!(meaning_of![(define x 1)], Ok(ast!(gset "test.x" (const 1))));
    }

    #[test]
    fn meaning_of_function_definition() {
        assert_eq!(
            meaning_of![(define (x y) y)],
            Ok(ast!(gset "test.x" (lambda 1 (ref 0))))
        );
    }

    #[test]
    fn meaning_of_library_definition_without_exports() {
        let meaning = meaning_of![("define-library" (foo bar) (begin 0))];
        let expected = Ok(ast!(module "(foo bar)" (const 0)));
        assert_eq!(meaning, expected);
    }

    #[test]
    fn import_produces_no_code() {
        let env = base_environment("test");
        meaning_of![env @ ("define-library" (foo bar) (export baz) (begin (define baz 42)))]
            .unwrap();

        assert_eq!(meaning_of![env @ (import (foo bar))], Ok(ast!(void)));
    }

    #[test]
    fn import_extends_the_environment_with_existing_variables() {
        let env = base_environment("test");
        meaning_of![env @ ("define-library" (foo bar) (export baz) (begin (define baz 42)))]
            .unwrap();
        meaning_of![env @ (import (foo bar))].unwrap();

        assert_eq!(
            env.lookup_global_variable("baz")
                .and_then(|b| b.as_global().map(str::to_string))
                .unwrap(),
            "(foo bar).baz"
        );
    }
}
