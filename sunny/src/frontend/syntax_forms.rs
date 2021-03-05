use crate::frontend::{
    ast::{Ast, AstNode},
    base_environment,
    environment::{Env, EnvBinding},
    error::{error_at, Error, Result},
    SyntaxExpander,
};
use log::warn;
use sunny_sexpr_parser::{CxR, Sexpr, SourceLocation};

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

pub struct Begin;

impl SyntaxExpander for Begin {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        further: &dyn SyntaxExpander,
        env: &Env,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (_ . rest) => {
                Sequence.expand(rest, further, env)
            }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}

pub struct Quotation;

impl SyntaxExpander for Quotation {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        _further: &dyn SyntaxExpander,
        _env: &Env,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (_, arg) => { Ok(Ast::constant(arg.clone())) }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}

pub struct Assignment;

impl SyntaxExpander for Assignment {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        further: &dyn SyntaxExpander,
        env: &Env,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (_, name: Symbol, value) => {
                let (depth, binding) = env.lookup_or_insert_global(name);
                let value = further.expand(value, further, env)?;
                binding.meaning_assignment(sexpr.map(()), depth, value)
            }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}

pub struct Definition;

impl SyntaxExpander for Definition {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        further: &dyn SyntaxExpander,
        env: &Env,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (_, name: Symbol, value) => {
                let value = further.expand(value, further, env)?;
                let (depth, binding) = env.ensure_global(name);
                binding.meaning_assignment(sexpr.map(()), depth, value)
            }]
            [sexpr: (_, (name: Symbol . args) . body) => {
                let body_env = env.extend(args)?;
                let body = Sequence.expand(body, further, &body_env)?;
                let function = Ast::lambda(sexpr.map(()), args.len(), body);

                let (depth, binding) = env.ensure_global(name);
                binding.meaning_assignment(sexpr.map(()), depth, function)
            }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}

pub struct Sequence;

impl SyntaxExpander for Sequence {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        further: &dyn SyntaxExpander,
        env: &Env,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (expr) => { further.expand(expr, further, env) }]
            [sexpr: (expr . rest) => {
                let first = further.expand(expr, further, env)?;
                let rest = self.expand(rest, further, env)?;
                Ok(Ast::sequence(first, rest))
            }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}

pub struct Branch;

impl SyntaxExpander for Branch {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        further: &dyn SyntaxExpander,
        env: &Env,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (_, condition, consequence, alternative) => {
                let condition = further.expand(condition, further, env)?;
                let consequence = further.expand(consequence, further, env)?;
                let alternative = further.expand(alternative, further, env)?;
                Ok(Ast::ifexpr(sexpr.map(()), condition, consequence, alternative))
            }]
            [sexpr: (_, condition, consequence) => {
                let condition = further.expand(condition, further, env)?;
                let consequence = further.expand(consequence, further, env)?;
                let alternative = Ast::void();
                Ok(Ast::ifexpr(sexpr.map(()), condition, consequence, alternative))
            }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}

pub struct Lambda;

impl SyntaxExpander for Lambda {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        further: &dyn SyntaxExpander,
        env: &Env,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (_, params . body) => {
                let body_env = env.extend(params)?;
                let body = Sequence.expand(body, further, &body_env)?;

                Ok(Ast::lambda(sexpr.map(()), params.len(), body))
            }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}

pub struct Cons;

impl SyntaxExpander for Cons {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        further: &dyn SyntaxExpander,
        env: &Env,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (_, car, cdr) => {
                let car = further.expand(car, further, env)?;
                let cdr = further.expand(cdr, further, env)?;
                Ok(Ast::cons(sexpr.map(()), car, cdr))
            }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}

pub struct LibraryDefinition;

impl SyntaxExpander for LibraryDefinition {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        _further: &dyn SyntaxExpander,
        _env: &Env,
    ) -> Result<AstNode<'src>> {
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
                            body_parts.push(Sequence.expand(stmt.cdr().unwrap(), &Expression, &lib_env)?)
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

pub struct Expression;

impl SyntaxExpander for Expression {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        _further: &dyn SyntaxExpander,
        env: &Env,
    ) -> Result<AstNode<'src>> {
        return match_sexpr![
            [sexpr: (f: Symbol . _) => {
                if let Some(sx) = env.lookup_syntax(f) {
                    sx.expand(sexpr, &Expression, env)
                } else {
                    self.expand_application(sexpr, env)
                }
            }]
            [sexpr: list: List => {
                self.expand_application(sexpr, env)
            }]
            [sexpr: name: Symbol => {
                let (depth, binding) = env.lookup_or_insert_global(name);
                binding.meaning_reference(sexpr.map(()), depth)
            }]
            [sexpr: _ => {
                Ok(Ast::constant(sexpr.clone()))
            }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)));
    }
}

impl Expression {
    fn expand_application<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        env: &Env,
    ) -> Result<AstNode<'src>> {
        let mut args = vec![];
        for a in sexpr.iter() {
            args.push(self.expand(a, self, env)?);
        }
        Ok(Ast::invoke(sexpr.map(()), args))
    }
}
