use crate::backend::Backend;
use sunny_sexpr_parser::{Sexpr, SourceLocation};

pub type AstNode<'src> = Box<Ast<'src>>;

#[derive(Debug, PartialEq)]
pub enum Ast<'src> {
    Const(SourceLocation<Sexpr<'src>>), // todo: don't store constants as strings
    Fetch(SourceLocation<()>, usize, usize),
    Store(SourceLocation<()>, usize, usize, AstNode<'src>),
    Cons(SourceLocation<()>, AstNode<'src>, AstNode<'src>),
    Sequence(AstNode<'src>, AstNode<'src>),
    If(
        SourceLocation<()>,
        AstNode<'src>,
        AstNode<'src>,
        AstNode<'src>,
    ),
    Lambda(SourceLocation<()>, usize, AstNode<'src>),
    Invoke(SourceLocation<()>, Vec<AstNode<'src>>),
    Module(AstNode<'src>),
    Export,
}

impl<'src> Ast<'src> {
    pub fn begin_module() {}

    pub fn end_module(content: AstNode<'src>) -> AstNode<'src> {
        Box::new(Ast::Module(content))
    }

    pub fn constant(sexpr: SourceLocation<Sexpr<'src>>) -> AstNode<'src> {
        Box::new(Ast::Const(sexpr))
    }

    pub fn fetch(context: SourceLocation<()>, depth: usize, idx: usize) -> AstNode<'src> {
        Box::new(Ast::Fetch(context, depth, idx))
    }

    pub fn store(
        context: SourceLocation<()>,
        depth: usize,
        idx: usize,
        val: AstNode<'src>,
    ) -> AstNode<'src> {
        Box::new(Ast::Store(context, depth, idx, val))
    }

    pub fn cons(
        context: SourceLocation<()>,
        first: AstNode<'src>,
        second: AstNode<'src>,
    ) -> AstNode<'src> {
        Box::new(Ast::Cons(context, first, second))
    }

    pub fn sequence(first: AstNode<'src>, next: AstNode<'src>) -> AstNode<'src> {
        Box::new(Ast::Sequence(first, next))
    }

    pub fn ifexpr(
        context: SourceLocation<()>,
        condition: AstNode<'src>,
        consequent: AstNode<'src>,
        alternative: AstNode<'src>,
    ) -> AstNode<'src> {
        Box::new(Ast::If(context, condition, consequent, alternative))
    }

    pub fn lambda(
        context: SourceLocation<()>,
        n_args: usize,
        body: AstNode<'src>,
    ) -> AstNode<'src> {
        Box::new(Ast::Lambda(context, n_args, body))
    }

    pub fn invoke(context: SourceLocation<()>, args: Vec<AstNode<'src>>) -> AstNode<'src> {
        Box::new(Ast::Invoke(context, args))
    }

    pub fn export(_exports: Vec<(&str, usize)>) -> AstNode<'src> {
        unimplemented!()
    }

    pub fn build<B: Backend>(&self, backend: &mut B) -> B::Ir {
        match self {
            Ast::Const(sexpr) => backend.constant(sexpr.map(()), sexpr.get_value()),
            Ast::Fetch(ctx, depth, idx) => backend.fetch(ctx.clone(), *depth, *idx),
            Ast::Store(ctx, depth, idx, value) => {
                let value = value.build(backend);
                backend.store(ctx.clone(), *depth, *idx, value)
            }
            Ast::Cons(ctx, car, cdr) => {
                let car = car.build(backend);
                let cdr = cdr.build(backend);
                backend.cons(ctx.clone(), car, cdr)
            }
            Ast::Sequence(first, next) => {
                let first = first.build(backend);
                let next = next.build(backend);
                backend.sequence(first, next)
            }
            Ast::If(ctx, cond, consequence, alternative) => {
                let condition = cond.build(backend);
                let consequence = consequence.build(backend);
                let alternative = alternative.build(backend);
                backend.ifexpr(ctx.clone(), condition, consequence, alternative)
            }
            Ast::Lambda(ctx, nparams, body) => {
                let body = body.build(backend);
                backend.lambda(ctx.clone(), *nparams, body)
            }
            Ast::Invoke(ctx, args) => {
                let args = args.iter().map(|arg| arg.build(backend)).collect();
                backend.invoke(ctx.clone(), args)
            }
            _ => unimplemented!("{:?}", self),
        }
    }
}

#[macro_export]
macro_rules! ast {
    (($($parts:tt)*)) => {ast![$($parts)*]};
    (const $x:expr) => {Ast::constant(SourceLocation::new(Sexpr::from($x)))};
    (ref $d:tt $i:tt) => {Ast::fetch(SourceLocation::new(()), $d, $i)};
    (set $d:tt $i:tt $x:tt) => {Ast::store(SourceLocation::new(()), $d, $i, ast![$x])};
    (cons $a:tt $b:tt) => {Ast::cons(SourceLocation::new(()), ast![$a], ast![$b])};
    (begin $a:tt $b:tt) => {Ast::sequence(ast![$a], ast![$b])};
    (begin $a:tt $($b:tt)+) => {Ast::sequence(ast![$a], ast![begin $($b)+])};
    (if $a:tt $b:tt $c:tt) => {Ast::ifexpr(SourceLocation::new(()), ast![$a], ast![$b], ast![$c])};
    (lambda $p:tt $b:tt) => {Ast::lambda(SourceLocation::new(()), $p, ast![$b])};
    (invoke $($a:tt)*) => {Ast::invoke(SourceLocation::new(()), vec![$(ast![$a]),*])};
}
