use crate::backend::Backend;
use crate::frontend::library::Export;
use sunny_sexpr_parser::{Scm, SourceLocation};

pub type AstNode = Box<Ast>;

#[derive(Debug, PartialEq)]
pub enum Ast {
    Void,
    Const(SourceLocation<()>, Scm),
    Fetch(SourceLocation<()>, usize),
    Store(SourceLocation<()>, usize, AstNode),
    FetchGlobal(SourceLocation<()>, String),
    StoreGlobal(SourceLocation<()>, String, AstNode),
    Sequence(AstNode, AstNode),
    If(SourceLocation<()>, AstNode, AstNode, AstNode),
    Lambda(SourceLocation<()>, usize, AstNode),
    LambdaVararg(SourceLocation<()>, usize, AstNode),
    Invoke(SourceLocation<()>, Vec<AstNode>),
    Intrinsic(SourceLocation<()>, &'static str, Vec<AstNode>),
    Module(String, AstNode, Vec<Export>),
}

impl Ast {
    pub fn module(name: impl ToString, content: AstNode, exports: Vec<Export>) -> AstNode {
        Box::new(Ast::Module(name.to_string(), content, exports))
    }

    pub fn void() -> AstNode {
        Box::new(Ast::Void)
    }

    pub fn constant(context: SourceLocation<()>, sexpr: Scm) -> AstNode {
        Box::new(Ast::Const(context, sexpr))
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

    pub fn sequence(first: AstNode, next: AstNode) -> AstNode {
        if *first == Ast::Void {
            return next;
        }
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

    pub fn lambda_vararg(context: SourceLocation<()>, n_args: usize, body: AstNode) -> AstNode {
        Box::new(Ast::LambdaVararg(context, n_args, body))
    }

    pub fn invoke(context: SourceLocation<()>, args: Vec<AstNode>) -> AstNode {
        Box::new(Ast::Invoke(context, args))
    }

    pub fn invoke_intrinsic(
        context: SourceLocation<()>,
        name: &'static str,
        args: Vec<AstNode>,
    ) -> AstNode {
        Box::new(Ast::Intrinsic(context, name, args))
    }

    pub fn build<B: Backend>(&self, backend: &mut B) -> B::Ir {
        match self {
            Ast::Void => backend.void(),
            Ast::Const(ctx, sexpr) => backend.constant(ctx.clone(), sexpr),
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
                backend.lambda(ctx.clone(), *nparams, false, body)
            }
            Ast::LambdaVararg(ctx, nparams, body) => {
                let body = body.build(backend);
                backend.lambda(ctx.clone(), *nparams, true, body)
            }
            Ast::Invoke(ctx, args) => {
                let args = args.iter().map(|arg| arg.build(backend)).collect();
                backend.invoke(ctx.clone(), args)
            }
            Ast::Intrinsic(ctx, name, args) => {
                let args = args.iter().map(|arg| arg.build(backend)).collect();
                backend.intrinsic(ctx.clone(), name, args)
            }
            Ast::Module(name, body, exports) => {
                let body = body.build(backend);
                backend.module(name, body, exports)
            }
        }
    }

    fn pretty_fmt(&self, f: &mut std::fmt::Formatter, indent: usize) -> std::fmt::Result {
        match self {
            Ast::Void => writeln!(f, "{: <1$}void", "", indent),
            Ast::Const(_, c) => writeln!(f, "{: <1$}const {2}", "", indent, c),
            Ast::Fetch(_, idx) => writeln!(f, "{: <1$}fetch {2}", "", indent, idx),
            Ast::Store(_, idx, val) => {
                writeln!(f, "{: <1$}store {2}", "", indent, idx)?;
                val.pretty_fmt(f, indent + 4)
            }
            Ast::FetchGlobal(_, idx) => writeln!(f, "{: <1$}global-fetch {2}", "", indent, idx),
            Ast::StoreGlobal(_, idx, val) => {
                writeln!(f, "{: <1$}global-store {2}", "", indent, idx)?;
                val.pretty_fmt(f, indent + 4)
            }
            Ast::Sequence(first, next) => {
                writeln!(f, "{: <1$}sequence", "", indent)?;
                first.pretty_fmt(f, indent + 4)?;
                next.pretty_fmt(f, indent + 4)
            }
            Ast::If(_, a, b, c) => {
                writeln!(f, "{: <1$}if", "", indent)?;
                a.pretty_fmt(f, indent + 4)?;
                writeln!(f, "{: <1$}then", "", indent)?;
                b.pretty_fmt(f, indent + 4)?;
                writeln!(f, "{: <1$}else", "", indent)?;
                c.pretty_fmt(f, indent + 4)
            }
            Ast::Lambda(_, n, body) => {
                writeln!(f, "{: <1$}lambda {2}", "", indent, n)?;
                body.pretty_fmt(f, indent + 4)
            }
            Ast::LambdaVararg(_, n, body) => {
                writeln!(f, "{: <1$}lambda {2}+n", "", indent, n)?;
                body.pretty_fmt(f, indent + 4)
            }
            Ast::Invoke(_, args) => {
                writeln!(f, "{: <1$}invoke", "", indent)?;
                for arg in args {
                    arg.pretty_fmt(f, indent + 4)?;
                }
                Ok(())
            }
            Ast::Intrinsic(_, name, args) => {
                writeln!(f, "{: <1$}intrinsic {2}", "", indent, name)?;
                for arg in args {
                    arg.pretty_fmt(f, indent + 4)?;
                }
                Ok(())
            }
            Ast::Module(name, body, exports) => {
                writeln!(f, "{: <1$}module {2} {3:?}", "", indent, name, exports)?;
                body.pretty_fmt(f, indent + 4)
            }
        }
    }
}

impl std::fmt::Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.pretty_fmt(f, 0)
    }
}

#[macro_export]
macro_rules! ast {
    (($($parts:tt)*)) => {ast![$($parts)*]};
    (void) => {Ast::void()};
    (const $x:expr) => {Ast::constant(SourceLocation::new(()), Scm::from($x))};
    (ref $i:tt) => {Ast::fetch(SourceLocation::new(()), $i)};
    (set $i:tt $x:tt) => {Ast::store(SourceLocation::new(()), $i, ast![$x])};
    (gref $i:tt) => {Ast::fetch_global(SourceLocation::new(()), $i)};
    (gset $i:tt $x:tt) => {Ast::store_global(SourceLocation::new(()), $i, ast![$x])};
    (begin $a:tt $b:tt) => {Ast::sequence(ast![$a], ast![$b])};
    (begin $a:tt $($b:tt)+) => {Ast::sequence(ast![$a], ast![begin $($b)+])};
    (if $a:tt $b:tt $c:tt) => {Ast::ifexpr(SourceLocation::new(()), ast![$a], ast![$b], ast![$c])};
    (lambda $p:tt $b:tt) => {Ast::lambda(SourceLocation::new(()), $p, ast![$b])};
    (lambda-var $p:tt $b:tt) => {Ast::lambda_vararg(SourceLocation::new(()), $p, ast![$b])};
    (invoke $($a:tt)*) => {Ast::invoke(SourceLocation::new(()), vec![$(ast![$a]),*])};
    (intrinsic $x:tt $($a:tt)*) => {Ast::invoke_intrinsic(SourceLocation::new(()), stringify!($x), vec![$(ast![$a]),*])};
    (module $n:tt $x:tt $(($v:tt $e:tt))*) => {Ast::module($n, ast![$x], vec![$(crate::frontend::library::Export::new($v, crate::frontend::environment::EnvBinding::global($e))),*])};
}
