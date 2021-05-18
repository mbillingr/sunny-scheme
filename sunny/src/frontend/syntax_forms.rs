use std::collections::HashMap;
use std::rc::Rc;

use crate::frontend::macros::pattern::PatternMatcher;
use crate::frontend::macros::template::Transcriber;
use crate::frontend::{
    ast::{Ast, AstNode},
    base_environment,
    environment::{Env, EnvBinding},
    error::{error_at, Error, Result},
    library::{libname_to_string, Export},
    syntactic_closure::SyntacticClosure,
    ExpansionContext, SyntaxExpander,
};
use sexpr_generics::prelude::*;
use sexpr_generics::{lists, with_sexpr_matcher};
use sunny_scm::{Scm, SourceLocation, SourceMap};

macro_rules! define_form {
    ($t:ident($xpr:ident, $ctx:ident, $env:ident): $($rules:tt)*) => {
        #[derive(Debug)]
        pub struct $t;
        impl SyntaxExpander for $t {
            fn expand(&self, $xpr: &Scm, $ctx: &mut ExpansionContext, $env: &Env) -> Result<AstNode> {
                with_sexpr_matcher! {
                    match $xpr, {
                        $($rules)*
                        _ => { Err($ctx.src_map.get($xpr).map_value(Error::InvalidForm)) }
                    }
                }
            }
        }
    }
}

//////////////////////////////////////////////////////////////////////////////////////////////////

define_form! {
    Expression(sexpr, ctx, env):
        ({f: SymbolName} . _) => {
            match env.lookup_variable(f) {
                Some(EnvBinding::Syntax(sx)) => sx.expand(sexpr, ctx, env),
                Some(EnvBinding::Intrinsic(name, n_params, _)) => Expression.expand_intrinsic_application(name, n_params, sexpr, env, ctx),
                _ => Expression.expand_application(sexpr, ctx, env),
            }
        }
        ({sc: Obj<SyntacticClosure>} . _) => {
            if let Some(f) = sc.raw_expr().to_symbol() {
                match sc.env().lookup_variable(f) {
                    Some(EnvBinding::Syntax(sx)) => sx.expand(sexpr, ctx, env),
                    Some(EnvBinding::Intrinsic(name, n_params, _)) => Expression.expand_intrinsic_application(name, n_params, sexpr, env, ctx),
                    _ => Expression.expand_application(sexpr, ctx, env),
                }
            } else {
                Expression.expand_application(sexpr, ctx, env)
            }
        }
        () => {
            Err(ctx.src_map.get(sexpr).map(|_|Error::InvalidForm))
        }
        {list: List} => {
            Expression.expand_application(list, ctx, env)
        }
        {name: SymbolName} => {
            use EnvBinding::*;
            let context = ctx.src_map.get(sexpr);
            match env.lookup_variable(name) {
                Some(Variable) => {
                    let offset = env.lookup_variable_index(name).unwrap();
                    Ok(Ast::fetch(context, offset))
                }
                Some(Global(full_name)) => Ok(Ast::fetch_global(context, full_name)),
                Some(Syntax(_)) => Err(error_at(&context, Error::SyntaxAsValue)),
                Some(Intrinsic(name, _, reified)) => {
                    if let Global(full_name) = *reified {
                        Ok(Ast::fetch_global(context, full_name))
                    } else {
                        panic!("Invalid intrinsic: {}", name)
                    }
                    /*Ok(Ast::lambda(SourceLocation::new(()), n_params,
                                   Ast::invoke_intrinsic(SourceLocation::new(()), name,
                                                         (0..n_params).map(|n| Ast::fetch(SourceLocation::new(()), n)).collect())))}*/
                }
                None => {
                    env.ensure_global_variable(name);
                    Expression.expand(sexpr, ctx, env)
                }
            }
        }
        {sc: Obj<SyntacticClosure>} => {
            sc.expand(&Expression, ctx, env)
        }
        _ => {
            Ok(Ast::constant(ctx.src_map.get(sexpr), sexpr.clone()))
        }
}

impl Expression {
    fn expand_application(
        &self,
        sexpr: &Scm,
        ctx: &mut ExpansionContext,
        env: &Env,
    ) -> Result<AstNode> {
        let mut args = vec![];
        for a in lists::iter(sexpr) {
            args.push(self.expand(&a, ctx, env)?);
        }
        Ok(Ast::invoke(ctx.src_map.get(sexpr), args))
    }

    fn expand_intrinsic_application(
        &self,
        name: &'static str,
        n_params: usize,
        sexpr: &Scm,
        env: &Env,
        ctx: &mut ExpansionContext,
    ) -> Result<AstNode> {
        let mut args = vec![];
        for a in lists::iter(sexpr).skip(1) {
            args.push(self.expand(&a, ctx, env)?);
        }
        if args.len() != n_params {
            return Err(error_at(
                &ctx.src_map.get(sexpr),
                Error::WrongNrArgs(args.len(), n_params),
            ));
        }
        Ok(Ast::invoke_intrinsic(ctx.src_map.get(sexpr), name, args))
    }
}

define_form! {
    Begin(sexpr, ctx, env):
        (_ . rest) => { Sequence.expand(rest, ctx, env) }
}

define_form! {
    Quotation(sexpr, ctx, _env):
        (_ value) => {
            Ok(Ast::constant(ctx.src_map.get(value), value.quote()))
        }
}

define_form! {
    Assignment(sexpr, ctx, env):
        (_ {name: SymbolName} value) => {
            let context = ctx.src_map.get(sexpr);
            env.ensure_variable(name);
            let value = Expression.expand(value, ctx, env)?;
            if let Some(full_name) = env.lookup_variable(name).unwrap().as_global() {
                Ok(Ast::store_global(context, full_name, value))
            } else {
                let offset = env.lookup_variable_index(name).unwrap();
                Ok(Ast::store(context, offset, value))
            }
        }
}

define_form! {
    Definition(sexpr, ctx, env):
        (_ {name: SymbolName} value) => {
            let value = Expression.expand(value, ctx, env)?;
            env.ensure_global_variable(name);
            let binding = env.lookup_global_variable(name).unwrap();
            let full_name = binding.as_global().unwrap();
            Ok(Ast::store_global(ctx.src_map.get(sexpr), full_name, value))
        }
        (_ ({name: SymbolName} . args) . body) => {
            env.ensure_global_variable(name);

            let function = Lambda::build_ast(ctx.src_map.get(sexpr), args, body, env, ctx)?;

            let binding = env.lookup_global_variable(name).unwrap();
            let full_name = binding.as_global().unwrap();
            Ok(Ast::store_global(ctx.src_map.get(sexpr), full_name, function))
        }
}

define_form! {
    LocalDefinition(sexpr, ctx, env):
        (_ {name: SymbolName} value) => {
            let value = Expression.expand(value, ctx, env)?;
            let idx = env.lookup_variable_index(name).unwrap();
            Ok(Ast::store(ctx.src_map.get(sexpr), idx, value))
        }
        (_ ({name: SymbolName} . args) . body) => {
            let function = Lambda::build_ast(ctx.src_map.get(sexpr), args, body, env, ctx)?;
            let idx = env.lookup_variable_index(name).unwrap();
            Ok(Ast::store(ctx.src_map.get(sexpr), idx, function))
        }
}

define_form! {
    Sequence(sexpr, ctx, env):
        (expr) => { Expression.expand(expr, ctx, env) }
        (expr . rest) => {
            let first = Expression.expand(expr, ctx, env)?;
            let rest = Sequence.expand(rest, ctx, env)?;
            Ok(Ast::sequence(first, rest))
        }
}

define_form! {
    Branch(sexpr, ctx, env):
        (_ condition consequence alternative) => {
            let condition = Expression.expand(condition, ctx, env)?;
            let consequence = Expression.expand(consequence, ctx, env)?;
            let alternative = Expression.expand(alternative, ctx, env)?;
            Ok(Ast::ifexpr(ctx.src_map.get(sexpr), condition, consequence, alternative))
        }
        (_ condition consequence) => {
            let condition = Expression.expand(condition, ctx, env)?;
            let consequence = Expression.expand(consequence, ctx, env)?;
            let alternative = Ast::void();
            Ok(Ast::ifexpr(ctx.src_map.get(sexpr), condition, consequence, alternative))
        }
}

define_form! {
    Let(sexpr, ctx, env):
        (_ bindings . body) => {
            let mut vars = vec![];
            let mut values = vec![];

            for binding in lists::iter(bindings) {
                with_sexpr_matcher! {
                    match binding, {
                        ({var: SymbolName} val) => {
                            vars.push(var);
                            values.push(val);
                        }
                        _ => { return Err(ctx.src_map.get(&binding).map_value(Error::InvalidForm)) }
                    }
                }
            }

            let body_env = env.extend_vars(vars.into_iter());
            let body = Body.expand(body, ctx, &body_env)?;

            let func = Ast::lambda(ctx.src_map.get(sexpr), values.len(), body);

            let mut args = vec![func];
            for val in values {
                args.push(Expression.expand(val, ctx, env).unwrap());
            }

            Ok(Ast::invoke(ctx.src_map.get(sexpr), args))
        }
}

define_form! {
    Lambda(sexpr, ctx, env):
        (_ params . body) => {
            Self::build_ast(ctx.src_map.get(sexpr), params, body, env, ctx)
        }
}

impl Lambda {
    fn build_ast(
        context: SourceLocation<()>,
        params: &Scm,
        body: &Scm,
        env: &Env,
        ctx: &mut ExpansionContext,
    ) -> Result<AstNode> {
        let body_env = env.extend_from_sexpr(params, &ctx.src_map)?;
        let body = Body.expand(body, ctx, &body_env)?;

        if params.last_cdr().is_null() {
            Ok(Ast::lambda(context, lists::length(params), body))
        } else {
            Ok(Ast::lambda_vararg(context, lists::length(params), body))
        }
    }
}

#[derive(Debug)]
pub struct Body;

impl SyntaxExpander for Body {
    fn expand(&self, sexpr: &Scm, ctx: &mut ExpansionContext, env: &Env) -> Result<AstNode> {
        let mut body = vec![];
        let mut definition_names = vec![];
        let mut definition_exprs = vec![];
        for exp in lists::iter(sexpr) {
            if is_definition(&exp) {
                definition_names.push(definition_name(&exp).unwrap());
                definition_exprs.push(definition_value_expr(&exp).unwrap());
            } else {
                body.push(exp.clone());
            }
        }

        if definition_names.is_empty() {
            Sequence.expand(sexpr, ctx, env)
        } else {
            LetRec::build_ast(
                ctx.src_map.get(sexpr),
                &definition_names,
                definition_exprs.into_iter(),
                body.into_iter(),
                env,
                ctx,
            )
        }
    }
}

fn is_definition(expr: &Scm) -> bool {
    with_sexpr_matcher! {
        match expr, {
            ({:define} . _) => { true }
            _ => { false }
        }
    }
}

fn definition_name(expr: &Scm) -> Option<&str> {
    with_sexpr_matcher! {
        match expr, {
            (_ {name: SymbolName} _) => { Some(name) }
            (_ ({name: SymbolName} . _) . _) => { Some(name) }
            _ => { None }
        }
    }
}

fn definition_value_expr(expr: &Scm) -> Option<Scm> {
    with_sexpr_matcher! {
        match expr, {
            (_ {_: Symbol} value) => { Some(value.clone()) }
            (_ (_ . args) . body) => {
                Some(Scm::cons(Scm::symbol("lambda"),
                               Scm::cons(args.clone(),
                                         body.clone())))
            }
            _ => { None }
        }
    }
}

define_form! {
    LetRec(_sexpr, _src_map, _env):
        _ => { unimplemented!() }
}

impl LetRec {
    fn build_ast(
        context: SourceLocation<()>,
        names: &[&str],
        defs: impl DoubleEndedIterator<Item = Scm>,
        body: impl DoubleEndedIterator<Item = Scm>,
        env: &Env,
        ctx: &mut ExpansionContext,
    ) -> Result<AstNode> {
        let n_vars = names.len();
        let body_env = env.extend_vars(names.iter());

        let body = body.rfold(Scm::null(), |acc, stmt| Scm::cons(stmt, acc));

        let mut body_ast = Sequence.expand(&body, ctx, &body_env)?;

        for (name, exp) in names.iter().zip(defs) {
            let idx = body_env.lookup_variable_index(name).unwrap();
            let def = Expression.expand(&exp, ctx, &body_env)?;
            let store = Ast::store(context.clone(), idx, def);
            body_ast = Ast::sequence(store, body_ast);
        }

        let lambda = Ast::lambda(context.clone(), n_vars, body_ast);

        let mut args = vec![lambda];
        for _ in 0..n_vars {
            args.push(Ast::void());
        }

        let call = Ast::invoke(context, args);

        Ok(call)
    }
}

define_form! {
    SyntaxDefinition(sexpr, ctx, env):
       (_ {keyword: SymbolName} transformer_spec) => {
            let transformer = SyntaxTransformer.build(transformer_spec, env, ctx)?;
            env.add_global_binding(keyword, transformer);
            Ok(Ast::void())
       }
}

define_form! {
    Import(sexpr, ctx, env):
       (_ . import_sets) => {
           let mut import_ast = Ast::void();
           for import_set in lists::iter(import_sets) {
               let set_ast = Self::process_import_set(&import_set, env, ctx)?;
               import_ast = Ast::sequence(import_ast, set_ast)
           }
           Ok(import_ast)
       }
}

impl Import {
    pub fn process_import_set(
        import_set: &Scm,
        env: &Env,
        ctx: &mut ExpansionContext,
    ) -> Result<AstNode> {
        let libname = import_set;
        let libstr = libname_to_string(libname);

        if Self::import_all(&libstr, env) {
            return Ok(Ast::void());
        }

        let libexpr = env
            .parse_library(&libstr, &ctx.src_map)?
            .ok_or_else(|| error_at(&ctx.src_map.get(libname), Error::UnknownLibrary))?;
        let libast = LibraryDefinition.expand(&libexpr, ctx, env)?;

        assert!(Self::import_all(&libstr, env));

        Ok(libast)
    }

    pub fn import_all(libname: &str, env: &Env) -> bool {
        if let Some(lib) = env.find_library(&libname) {
            for export in lib.exports() {
                env.add_global_binding(export.export_name.to_string(), export.binding.clone());
            }
            true
        } else {
            false
        }
    }
}

pub struct SyntaxTransformer;

impl SyntaxTransformer {
    fn build(
        &self,
        spec: &Scm,
        env: &Env,
        ctx: &mut ExpansionContext,
    ) -> Result<Rc<dyn SyntaxExpander>> {
        with_sexpr_matcher! {
            match spec, {
                ({:"simple-macro"} args body) => {
                    Ok(Rc::new(SimpleMacro::new(args, body, env, &ctx.src_map)?) as Rc<dyn SyntaxExpander>)
                }
                ({:"syntax-rules"} {ellipsis: Symbol} {literals: List} . rules) => {
                    Ok(Rc::new(SyntaxRules::new(ellipsis, literals, rules, env)?) as Rc<dyn SyntaxExpander>)
                }
                ({:"syntax-rules"} {literals: List} . rules) => {
                    let ellipsis = Scm::symbol("...");
                    Ok(Rc::new(SyntaxRules::new(&ellipsis, literals, rules, env)?) as Rc<dyn SyntaxExpander>)
                }
                _ => { Err(ctx.src_map.get(spec).map_value(Error::InvalidForm)) }
            }
        }
    }
}

#[derive(Debug)]
pub struct SimpleMacro {
    args: Vec<String>,
    template: SourceLocation<Scm>,
}

impl SyntaxExpander for SimpleMacro {
    fn expand(&self, sexpr: &Scm, ctx: &mut ExpansionContext, env: &Env) -> Result<AstNode> {
        let mut substitutions = HashMap::new();
        let mut more_args = sexpr
            .cdr()
            .ok_or_else(|| ctx.src_map.get(sexpr).map_value(Error::MissingArgument))?;

        for name in &self.args {
            let arg = more_args
                .car()
                .cloned()
                .map(|a| SyntacticClosure::new(a, env.clone()))
                .map(Scm::obj)
                .ok_or_else(|| ctx.src_map.get(more_args).map_value(Error::MissingArgument))?;
            more_args = more_args.cdr().unwrap();
            substitutions.insert(name.as_str(), arg);
        }

        let new_sexpr = Scm::substitute(&self.template, &substitutions);
        Expression.expand(&new_sexpr, ctx, env)
    }
}

impl SimpleMacro {
    pub fn new(args: &Scm, body: &Scm, env: &Env, src_map: &SourceMap) -> Result<Self> {
        let mut argnames = vec![];
        for arg in lists::iter(args) {
            if let Some(name) = arg.as_symbol() {
                argnames.push(name.to_string());
            } else {
                return Err(src_map.get(&arg).map_value(Error::ExpectedSymbol));
            }
        }

        let template =
            SourceLocation::new(Scm::obj(SyntacticClosure::new(body.clone(), env.clone())));

        Ok(SimpleMacro {
            args: argnames,
            template,
        })
    }
}

#[derive(Debug)]
pub struct SyntaxRules {
    rules: Vec<(PatternMatcher, Scm)>,
    transcriber: Transcriber,
}

impl SyntaxExpander for SyntaxRules {
    fn expand(&self, expr: &Scm, ctx: &mut ExpansionContext, env: &Env) -> Result<AstNode> {
        let sexpr = SyntacticClosure::new_scm(expr.clone(), env.clone());
        for (matcher, template) in &self.rules {
            if let Some(bindings) = matcher.match_value(&sexpr, env) {
                let expanded_sexpr = self
                    .transcriber
                    .transcribe(template, &bindings)
                    .ok_or_else(|| error_at(&ctx.src_map.get(template), Error::InvalidTemplate))?;
                //println!("{} => {}", sexpr, expanded_sexpr);
                return Expression.expand(&expanded_sexpr, ctx, env);
            }
        }
        Err(error_at(&ctx.src_map.get(expr), Error::InvalidForm))
    }
}

impl SyntaxRules {
    pub fn new(ellipsis: &Scm, literals: &Scm, rules: &Scm, env: &Env) -> Result<Self> {
        let rules = Self::parse_rules(ellipsis, literals, rules, env).ok_or(Error::InvalidForm)?;
        let transcriber = Transcriber::new(ellipsis.clone());
        Ok(SyntaxRules { rules, transcriber })
    }

    fn parse_rules(
        ellipsis: &Scm,
        literals: &Scm,
        rules: &Scm,
        env: &Env,
    ) -> Option<Vec<(PatternMatcher, Scm)>> {
        let mut matchers = vec![];
        for rule in lists::iter(rules) {
            let pattern = rule.car()?.clone();
            let template = rule.cadr()?.clone();

            let matcher = PatternMatcher::new(pattern, ellipsis.clone(), literals.clone());
            let template = Scm::from(SyntacticClosure::new(template, env.clone()));

            matchers.push((matcher, template));
        }
        Some(matchers)
    }
}

#[derive(Debug)]
pub struct LibraryDefinition;

impl SyntaxExpander for LibraryDefinition {
    fn expand(&self, sexpr: &Scm, ctx: &mut ExpansionContext, env: &Env) -> Result<AstNode> {
        with_sexpr_matcher! {
            match sexpr, {
                (_ libname . statements) => {
                    let mut lib_env = base_environment(libname);
                    lib_env.use_libraries_from(env);

                    for stmt in lists::iter(statements) {
                        match stmt.car().and_then(|s| s.as_symbol()) {
                            Some("import") => {Import.expand(&stmt, ctx, &lib_env)?;}
                            Some("export") => {}
                            Some("begin") => {}
                            _ => return Err(error_at(&ctx.src_map.get(&stmt), Error::UnexpectedStatement)),
                        }
                    }

                    let mut body_parts = vec![];
                    for stmt in lists::iter(statements) {
                        if let Some("begin") = stmt.car().and_then(|s| s.as_symbol()) {
                            body_parts.push(Sequence.expand(stmt.cdr().unwrap(), ctx, &lib_env)?)
                        }
                    }

                    let mut exports = vec![];
                    for stmt in lists::iter(statements) {
                        if let Some("export") = stmt.car().and_then(|s| s.as_symbol()) {
                            for export_item in lists::iter(stmt.cdr().unwrap()) {
                                let export_name = export_item
                                    .as_symbol()
                                    .ok_or_else(|| error_at(&ctx.src_map.get(&export_item), Error::ExpectedSymbol))?;
                                let binding = lib_env.lookup_global_variable(export_name).ok_or_else(||error_at(&ctx.src_map.get(&export_item), Error::UndefinedExport))?;
                                exports.push(Export::new(export_name, binding));
                            }
                        }
                    }

                    let export_vars = exports.iter().filter(|exp| exp.binding.is_global()).cloned().collect();

                    env.define_library(libname, exports);

                    let mut body = body_parts.pop().unwrap_or_else(Ast::void);
                    while let Some(prev_part) = body_parts.pop() {
                        body = Ast::sequence(prev_part, body);
                    }

                    Ok(Ast::module(libname, body, export_vars))
                }
                _ => { Err(ctx.src_map.get(sexpr).map_value(Error::InvalidForm)) }
            }
        }
    }
}
