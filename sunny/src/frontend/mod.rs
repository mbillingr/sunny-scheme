#[macro_use]
pub mod ast;
pub mod environment;
pub mod error;
pub mod syntactic_closure;
pub mod syntax_forms;

use crate::frontend::environment::Environment;
use crate::frontend::{
    ast::AstNode,
    environment::Env,
    error::Result,
    syntax_forms::{
        Assignment, Begin, Branch, Cons, Definition, Expression, Lambda, LibraryDefinition,
        Quotation, SyntaxDefinition,
    },
};
use sunny_sexpr_parser::SrcExpr;

pub trait SyntaxExpander: std::fmt::Debug {
    fn expand(&self, sexpr: &SrcExpr, env: &Env) -> Result<AstNode>;
}

pub fn base_environment() -> Env {
    let global = Environment::Empty
        .add_binding("begin", Begin)
        .add_binding("cons", Cons)
        .add_binding("define", Definition)
        .add_binding("define-library", LibraryDefinition)
        .add_binding("define-syntax", SyntaxDefinition)
        .add_binding("if", Branch)
        .add_binding("lambda", Lambda)
        .add_binding("quote", Quotation)
        .add_binding("set!", Assignment);

    Env::new(dbg!(global), Environment::Empty)
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::Ast;
    use sunny_sexpr_parser::Sexpr;
    use sunny_sexpr_parser::SourceLocation;

    macro_rules! sexpr {
        ($t:ty:()) => { <$t>::nil() };

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
            let sexpr = sexpr![Sexpr: $expr];
            let env = dbg!(base_environment());
            Expression.expand(&sexpr.into(), &env)
        }};
    }

    #[test]
    fn meaning_of_constant() {
        assert_eq!(meaning_of![1], Ok(ast!(const 1)));
    }

    #[test]
    fn meaning_of_symbol() {
        assert_eq!(meaning_of![x], Ok(ast!(ref 0 0)));
    }

    #[test]
    fn meaning_of_cons() {
        assert_eq!(meaning_of![(cons 1 2)], Ok(ast!(cons (const 1) (const 2))));
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
    fn meaning_of_set() {
        assert_eq!(meaning_of![(["set!"] x 42)], Ok(ast!(set 0 0 (const 42))));
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
        assert_eq!(meaning_of![(foo)], Ok(ast!(invoke (ref 0 0))));
    }

    #[test]
    fn meaning_of_lambda_identity() {
        assert_eq!(meaning_of![(lambda (x) x)], Ok(ast!(lambda 1 (ref 0 0))));
    }

    #[test]
    fn meaning_of_global_ref_in_lambda() {
        assert_eq!(meaning_of![(lambda (y) x)], Ok(ast!(lambda 1 (ref 0 1))));
    }

    #[test]
    fn meaning_of_lambda_with_multiple_arguments() {
        assert_eq!(meaning_of![(lambda (x y) x)], Ok(ast!(lambda 2 (ref 0 0))));
        assert_eq!(meaning_of![(lambda (x y) y)], Ok(ast!(lambda 2 (ref 0 1))));
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
}
