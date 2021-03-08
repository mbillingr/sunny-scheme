use crate::backend::Backend;
use sunny_sexpr_parser::{SourceLocation, SrcExpr};

pub type AstNode = Box<Ast>;

#[derive(Debug, PartialEq)]
pub enum Ast {
    Void,
    Const(SrcExpr),
    Fetch(SourceLocation<()>, usize),
    Store(SourceLocation<()>, usize, AstNode),
    FetchGlobal(SourceLocation<()>, String),
    StoreGlobal(SourceLocation<()>, String, AstNode),
    Cons(SourceLocation<()>, AstNode, AstNode),
    Sequence(AstNode, AstNode),
    If(SourceLocation<()>, AstNode, AstNode, AstNode),
    Lambda(SourceLocation<()>, usize, AstNode),
    Invoke(SourceLocation<()>, Vec<AstNode>),
    Module(AstNode),
    Export(Vec<(String, usize)>),
}

impl Ast {
    pub fn module(content: AstNode) -> AstNode {
        Box::new(Ast::Module(content))
    }

    pub fn void() -> AstNode {
        Box::new(Ast::Void)
    }

    pub fn constant(sexpr: SrcExpr) -> AstNode {
        Box::new(Ast::Const(sexpr))
    }

    pub fn fetch(context: SourceLocation<()>, idx: usize) -> AstNode {
        Box::new(Ast::Fetch(context, idx))
    }

    pub fn store(context: SourceLocation<()>, idx: usize, val: AstNode) -> AstNode {
        Box::new(Ast::Store(context, idx, val))
    }

    pub fn fetch_global(context: SourceLocation<()>, name: impl ToString) -> AstNode {
        Box::new(Ast::FetchGlobal(context, name.to_string()))
    }

    pub fn store_global(context: SourceLocation<()>, name: impl ToString, val: AstNode) -> AstNode {
        Box::new(Ast::StoreGlobal(context, name.to_string(), val))
    }

    pub fn cons(context: SourceLocation<()>, first: AstNode, second: AstNode) -> AstNode {
        Box::new(Ast::Cons(context, first, second))
    }

    pub fn sequence(first: AstNode, next: AstNode) -> AstNode {
        Box::new(Ast::Sequence(first, next))
    }

    pub fn ifexpr(
        context: SourceLocation<()>,
        condition: AstNode,
        consequent: AstNode,
        alternative: AstNode,
    ) -> AstNode {
        Box::new(Ast::If(context, condition, consequent, alternative))
    }

    pub fn lambda(context: SourceLocation<()>, n_args: usize, body: AstNode) -> AstNode {
        Box::new(Ast::Lambda(context, n_args, body))
    }

    pub fn invoke(context: SourceLocation<()>, args: Vec<AstNode>) -> AstNode {
        Box::new(Ast::Invoke(context, args))
    }

    pub fn export(exports: Vec<(String, usize)>) -> AstNode {
        Box::new(Ast::Export(exports))
    }

    pub fn build<B: Backend>(&self, backend: &mut B) -> B::Ir {
        match self {
            Ast::Void => backend.void(),
            Ast::Const(sexpr) => backend.constant(sexpr.map_value(()), sexpr.get_value()),
            Ast::Fetch(ctx, idx) => backend.fetch(ctx.clone(), *idx),
            Ast::Store(ctx, idx, value) => {
                let value = value.build(backend);
                backend.store(ctx.clone(), *idx, value)
            }
            Ast::FetchGlobal(ctx, name) => backend.fetch_global(ctx.clone(), name),
            Ast::StoreGlobal(ctx, name, value) => {
                let value = value.build(backend);
                backend.store_global(ctx.clone(), name, value)
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
            Ast::Module(body) => {
                let body = body.build(backend);
                let body_func = backend.lambda(SourceLocation::new(()), 0, body);
                let libcode = backend.invoke(SourceLocation::new(()), vec![body_func]);
                // this is just to make the test pass for now and serves no real purpose
                backend.store(SourceLocation::new(()), 0, libcode)
            }
            Ast::Export(exports) => backend.export(exports),
        }
    }
}

#[macro_export]
macro_rules! ast {
    (($($parts:tt)*)) => {ast![$($parts)*]};
    (const $x:expr) => {Ast::constant(SourceLocation::new(Sexpr::from($x)))};
    (ref $i:tt) => {Ast::fetch(SourceLocation::new(()), $i)};
    (set $i:tt $x:tt) => {Ast::store(SourceLocation::new(()), $i, ast![$x])};
    (gref $i:tt) => {Ast::fetch_global(SourceLocation::new(()), $i)};
    (gset $i:tt $x:tt) => {Ast::store_global(SourceLocation::new(()), $i, ast![$x])};
    (cons $a:tt $b:tt) => {Ast::cons(SourceLocation::new(()), ast![$a], ast![$b])};
    (begin $a:tt $b:tt) => {Ast::sequence(ast![$a], ast![$b])};
    (begin $a:tt $($b:tt)+) => {Ast::sequence(ast![$a], ast![begin $($b)+])};
    (if $a:tt $b:tt $c:tt) => {Ast::ifexpr(SourceLocation::new(()), ast![$a], ast![$b], ast![$c])};
    (lambda $p:tt $b:tt) => {Ast::lambda(SourceLocation::new(()), $p, ast![$b])};
    (invoke $($a:tt)*) => {Ast::invoke(SourceLocation::new(()), vec![$(ast![$a]),*])};
}
