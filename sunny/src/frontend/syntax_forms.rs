use crate::frontend::{
    ast::{Ast, AstNode},
    base_environment,
    environment::{Env, EnvBinding},
    error::{error_at, Error, Result},
    SyntaxExpander,
};
use log::warn;
use sunny_sexpr_parser::{CxR, RefExpr};

macro_rules! match_sexpr {
    ([$expr:tt: $($rules:tt)*]) => {
        _match_sexpr![$expr; $($rules)*]
    };

    ([$($first:tt)*] $($rest:tt)*) => {{
        match match_sexpr![[$($first)*]] {
            Some(r) => Some(r),
            None => match_sexpr![$($rest)*],
        }
    }};
}

macro_rules! _match_sexpr {
    ($expr:tt; _ => $action:block) => {{ let _ = $expr; Some($action) }};

    ($expr:tt; $x:ident => $action:block) => {{ let $x = $expr; Some($action) }};

    ($expr:tt; $x:ident: List => $action:block) => {
        if $expr.is_pair() {
            Some($action)
        } else {
            None
        }
    };

    ($expr:tt; $x:ident: Symbol => $action:block) => {
        if let Some($x) = $expr.as_symbol() {
            Some($action)
        } else {
            None
        }
    };

    ($expr:tt; () => $action:block) => {
        if $expr.is_null() {
            Some($action)
        } else {
            None
        }
    };

    ($expr:tt; ($first:tt $(:$typ1:tt)? . $second:tt $(:$typ2:tt)?) => $action:block) => {
        if $expr.is_pair() {
            let car = $expr.car().unwrap();
            let cdr = $expr.cdr().unwrap();
            _match_sexpr![car; $first $(:$typ1)? => {
                _match_sexpr![cdr; $second $(:$typ2)? => $action]
            }].flatten()
        } else {
            None
        }
    };

    ($expr:tt; ($first:tt $(:$typ:tt)?) => $action:block) => {
        if $expr.is_pair() {
            let car = $expr.car().unwrap();
            let cdr = $expr.cdr().unwrap();
            _match_sexpr![cdr; () => {
                _match_sexpr![car; $first $(:$typ)? => $action]
            }].flatten()
        } else {
            None
        }
    };

    ($expr:tt; ($first:tt $(:$typ:tt)?, $($rest:tt)*) => $action:block) => {
        if $expr.is_pair() {
            let car = $expr.car().unwrap();
            let cdr = $expr.cdr().unwrap();
            _match_sexpr![car; $first $(:$typ)? => {
                _match_sexpr![cdr; ($($rest)*) => $action]
            }].flatten()
        } else {
            None
        }
    };
}

macro_rules! declare_form {
    ($t:ident($xpr:ident, $env:ident): $([$($rules:tt)*])+) => {
        pub struct $t;
        impl SyntaxExpander for $t {
            fn expand<'src>(&self, $xpr: RefExpr<'src>, $env: &Env) -> Result<AstNode<'src>> {
                match_sexpr![
                    $([$xpr: $($rules)*])+
                ]
                .unwrap_or_else(|| Err($xpr.map(Error::InvalidForm)))
            }
        }
    }
}

//////////////////////////////////////////////////////////////////////////////////////////////////

declare_form! {
    Expression(sexpr, env):
        [(f: Symbol . _) => {
            if let Some(sx) = env.lookup_syntax(f) {
                sx.expand(sexpr, env)
            } else {
                Expression.expand_application(sexpr, env)
            }
        }]
        [list: List => {
            Expression.expand_application(sexpr, env)
        }]
        [name: Symbol => {
            let (depth, binding) = env.lookup_or_insert_global(name);
            binding.expand_reference(sexpr.map(()), depth)
        }]
        [_ => {
            Ok(Ast::constant(sexpr.clone()))
        }]
}

impl Expression {
    fn expand_application<'src>(&self, sexpr: RefExpr<'src>, env: &Env) -> Result<AstNode<'src>> {
        let mut args = vec![];
        for a in sexpr.iter() {
            args.push(self.expand(a, env)?);
        }
        Ok(Ast::invoke(sexpr.map(()), args))
    }
}

declare_form! {
    Begin(sexpr, env):
        [(_ . rest) => { Sequence.expand(rest, env) }]
}

declare_form! {
    Quotation(sexpr, _env):
        [(_, value) => { Ok(Ast::constant(value.clone())) }]
}

declare_form! {
    Assignment(sexpr, env):
        [(_, name: Symbol, value) => {
            let (depth, binding) = env.lookup_or_insert_global(name);
            let value = Expression.expand(value, env)?;
            binding.expand_assignment(sexpr.map(()), depth, value)
        }]
}

declare_form! {
    Definition(sexpr, env):
        [(_, name: Symbol, value) => {
            let value = Expression.expand(value, env)?;
            let (depth, binding) = env.ensure_global(name);
            binding.expand_assignment(sexpr.map(()), depth, value)
        }]
        [(_, (name: Symbol . args) . body) => {
            let body_env = env.extend(args)?;
            let body = Sequence.expand(body, &body_env)?;
            let function = Ast::lambda(sexpr.map(()), args.len(), body);

            let (depth, binding) = env.ensure_global(name);
            binding.expand_assignment(sexpr.map(()), depth, function)
        }]
}

declare_form! {
    Sequence(sexpr, env):
        [(expr) => { Expression.expand(expr, env) }]
        [(expr . rest) => {
            let first = Expression.expand(expr, env)?;
            let rest = Sequence.expand(rest, env)?;
            Ok(Ast::sequence(first, rest))
        }]
}

declare_form! {
    Branch(sexpr, env):
        [(_, condition, consequence, alternative) => {
            let condition = Expression.expand(condition, env)?;
            let consequence = Expression.expand(consequence, env)?;
            let alternative = Expression.expand(alternative, env)?;
            Ok(Ast::ifexpr(sexpr.map(()), condition, consequence, alternative))
        }]
        [(_, condition, consequence) => {
            let condition = Expression.expand(condition, env)?;
            let consequence = Expression.expand(consequence, env)?;
            let alternative = Ast::void();
            Ok(Ast::ifexpr(sexpr.map(()), condition, consequence, alternative))
        }]
}

declare_form! {
    Lambda(sexpr, env):
        [(_, params . body) => {
            let body_env = env.extend(params)?;
            let body = Sequence.expand(body, &body_env)?;

            Ok(Ast::lambda(sexpr.map(()), params.len(), body))
        }]
}

declare_form! {
    Cons(sexpr, env):
       [(_, car, cdr) => {
            let car = Expression.expand(car, env)?;
            let cdr = Expression.expand(cdr, env)?;
            Ok(Ast::cons(sexpr.map(()), car, cdr))
        }]
}

pub struct LibraryDefinition;

impl SyntaxExpander for LibraryDefinition {
    fn expand<'src>(&self, sexpr: RefExpr<'src>, _env: &Env) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (_, libname: List . statements) => {
                let lib_env = base_environment();

                let mut exports = vec![];
                let mut body_parts = vec![];
                for stmt in statements.iter() {
                    match stmt.car().and_then(|s| s.as_symbol()) {
                        Some("import") => warn!("Ignoring (import ...) statement in library definition"),
                        Some("export") => {
                            for export_item in stmt.cdr().unwrap().iter() {
                                let export_name = export_item
                                    .as_symbol()
                                    .ok_or_else(|| error_at(export_item, Error::ExpectedSymbol))?;
                                let (_, binding) = lib_env.ensure_global(export_name);
                                if let EnvBinding::Variable(var_idx) = binding {
                                    exports.push((export_name, var_idx));
                                } else {
                                    unimplemented!()
                                }
                            }
                        }
                        Some("begin") => {
                            body_parts.push(Sequence.expand(stmt.cdr().unwrap(), &lib_env)?)
                        }
                        _ => return Err(error_at(stmt, Error::UnexpectedStatement)),
                    }
                }

                let mut body = body_parts.pop().expect("Empty library body");
                while let Some(prev_part) = body_parts.pop() {
                    body = Ast::sequence(prev_part, body);
                }

                let meaning_exports = Ast::export(exports);
                body = Ast::sequence(body, meaning_exports);

                Ok(Ast::module(body))
            }]
        ]
            .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}
