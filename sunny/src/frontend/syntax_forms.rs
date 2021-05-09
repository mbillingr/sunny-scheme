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
    SyntaxExpander,
};
use sexpr_generics::prelude::*;
use sexpr_generics::{lists, with_sexpr_matcher};
use sunny_scm::{Scm, SourceLocation, SourceMap};

macro_rules! define_form {
    ($t:ident($xpr:ident, $map:ident, $env:ident): $($rules:tt)*) => {
        #[derive(Debug)]
        pub struct $t;
        impl SyntaxExpander for $t {
            fn expand(&self, $xpr: &Scm, $map: &SourceMap, $env: &Env) -> Result<AstNode> {
                with_sexpr_matcher! {
                    match $xpr, {
                        $($rules)*
                        _ => { Err($map.get($xpr).map_value(Error::InvalidForm)) }
                    }
                }
            }
        }
    }
}

//////////////////////////////////////////////////////////////////////////////////////////////////

define_form! {
    Expression(sexpr, src_map, env):
        ({f: SymbolName} . _) => {
            match env.lookup_variable(f) {
                Some(EnvBinding::Syntax(sx)) => sx.expand(sexpr, src_map, env),
                Some(EnvBinding::Intrinsic(name, n_params, _)) => Expression.expand_intrinsic_application(name, n_params, sexpr, env, src_map),
                _ => Expression.expand_application(sexpr, src_map, env),
            }
        }
        ({sc: Obj<SyntacticClosure>} . _) => {
            if let Some(f) = sc.raw_expr().to_symbol() {
                match sc.env().lookup_variable(f) {
                    Some(EnvBinding::Syntax(sx)) => sx.expand(sexpr, src_map, env),
                    Some(EnvBinding::Intrinsic(name, n_params, _)) => Expression.expand_intrinsic_application(name, n_params, sexpr, env, src_map),
                    _ => Expression.expand_application(sexpr, src_map, env),
                }
            } else {
                Expression.expand_application(sexpr, src_map, env)
            }
        }
        () => {
            Err(src_map.get(sexpr).map(|_|Error::InvalidForm))
        }
        {list: List} => {
            Expression.expand_application(list, src_map, env)
        }
        {name: SymbolName} => {
            use EnvBinding::*;
            let context = src_map.get(sexpr);
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
                    Expression.expand(sexpr, src_map, env)
                }
            }
        }
        {sc: Obj<SyntacticClosure>} => {
            sc.expand(&Expression, src_map, env)
        }
        _ => {
            Ok(Ast::constant(src_map.get(sexpr), sexpr.clone()))
        }
}

impl Expression {
    fn expand_application(&self, sexpr: &Scm, src_map: &SourceMap, env: &Env) -> Result<AstNode> {
        let mut args = vec![];
        for a in lists::iter(sexpr) {
            args.push(self.expand(&a, src_map, env)?);
        }
        Ok(Ast::invoke(src_map.get(sexpr), args))
    }

    fn expand_intrinsic_application(
        &self,
        name: &'static str,
        n_params: usize,
        sexpr: &Scm,
        env: &Env,
        src_map: &SourceMap,
    ) -> Result<AstNode> {
        let mut args = vec![];
        for a in lists::iter(sexpr).skip(1) {
            args.push(self.expand(&a, src_map, env)?);
        }
        if args.len() != n_params {
            return Err(error_at(
                &src_map.get(sexpr),
                Error::WrongNrArgs(args.len(), n_params),
            ));
        }
        Ok(Ast::invoke_intrinsic(src_map.get(sexpr), name, args))
    }
}

define_form! {
    Begin(sexpr, src_map, env):
        (_ . rest) => { Sequence.expand(rest, src_map, env) }
}

define_form! {
    Quotation(sexpr, src_map, _env):
        (_ value) => {
            Ok(Ast::constant(src_map.get(value), value.quote()))
        }
}

define_form! {
    Assignment(sexpr, src_map, env):
        (_ {name: SymbolName} value) => {
            let context = src_map.get(sexpr);
            env.ensure_variable(name);
            let value = Expression.expand(value, src_map, env)?;
            if let Some(full_name) = env.lookup_variable(name).unwrap().as_global() {
                Ok(Ast::store_global(context, full_name, value))
            } else {
                let offset = env.lookup_variable_index(name).unwrap();
                Ok(Ast::store(context, offset, value))
            }
        }
}

define_form! {
    Definition(sexpr, src_map, env):
        (_ {name: SymbolName} value) => {
            let value = Expression.expand(value, src_map, env)?;
            env.ensure_global_variable(name);
            let binding = env.lookup_global_variable(name).unwrap();
            let full_name = binding.as_global().unwrap();
            Ok(Ast::store_global(src_map.get(sexpr), full_name, value))
        }
        (_ ({name: SymbolName} . args) . body) => {
            env.ensure_global_variable(name);

            let function = Lambda::build_ast(src_map.get(sexpr), args, body, env, src_map)?;

            let binding = env.lookup_global_variable(name).unwrap();
            let full_name = binding.as_global().unwrap();
            Ok(Ast::store_global(src_map.get(sexpr), full_name, function))
        }
}

define_form! {
    LocalDefinition(sexpr, src_map, env):
        (_ {name: SymbolName} value) => {
            let value = Expression.expand(value, src_map, env)?;
            let idx = env.lookup_variable_index(name).unwrap();
            Ok(Ast::store(src_map.get(sexpr), idx, value))
        }
        (_ ({name: SymbolName} . args) . body) => {
            let function = Lambda::build_ast(src_map.get(sexpr), args, body, env, src_map)?;
            let idx = env.lookup_variable_index(name).unwrap();
            Ok(Ast::store(src_map.get(sexpr), idx, function))
        }
}

define_form! {
    Sequence(sexpr, src_map, env):
        (expr) => { Expression.expand(expr, src_map, env) }
        (expr . rest) => {
            let first = Expression.expand(expr, src_map, env)?;
            let rest = Sequence.expand(rest, src_map, env)?;
            Ok(Ast::sequence(first, rest))
        }
}

define_form! {
    Branch(sexpr, src_map, env):
        (_ condition consequence alternative) => {
            let condition = Expression.expand(condition, src_map, env)?;
            let consequence = Expression.expand(consequence, src_map, env)?;
            let alternative = Expression.expand(alternative, src_map, env)?;
            Ok(Ast::ifexpr(src_map.get(sexpr), condition, consequence, alternative))
        }
        (_ condition consequence) => {
            let condition = Expression.expand(condition, src_map, env)?;
            let consequence = Expression.expand(consequence, src_map, env)?;
            let alternative = Ast::void();
            Ok(Ast::ifexpr(src_map.get(sexpr), condition, consequence, alternative))
        }
}

define_form! {
    Let(sexpr, src_map, env):
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
                        _ => { return Err(src_map.get(&binding).map_value(Error::InvalidForm)) }
                    }
                }
            }

            let body_env = env.extend_vars(vars.into_iter());
            let body = Body.expand(body, src_map, &body_env)?;

            let func = Ast::lambda(src_map.get(sexpr), values.len(), body);

            let mut args = vec![func];
            for val in values {
                args.push(Expression.expand(val, src_map, env).unwrap());
            }

            Ok(Ast::invoke(src_map.get(sexpr), args))
        }
}

define_form! {
    Lambda(sexpr, src_map, env):
        (_ params . body) => {
            println!("expanding lambda: {}\n in {:?}", sexpr, env);
            Self::build_ast(src_map.get(sexpr), params, body, env, src_map)
        }
}

impl Lambda {
    fn build_ast(
        context: SourceLocation<()>,
        params: &Scm,
        body: &Scm,
        env: &Env,
        src_map: &SourceMap,
    ) -> Result<AstNode> {
        let body_env = env.extend_from_sexpr(params, src_map)?;
        let body = Body.expand(body, src_map, &body_env)?;

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
    fn expand(&self, sexpr: &Scm, src_map: &SourceMap, env: &Env) -> Result<AstNode> {
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
            Sequence.expand(sexpr, src_map, env)
        } else {
            LetRec::build_ast(
                src_map.get(sexpr),
                &definition_names,
                definition_exprs.into_iter(),
                body.into_iter(),
                env,
                src_map,
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
        src_map: &SourceMap,
    ) -> Result<AstNode> {
        let n_vars = names.len();
        let body_env = env.extend_vars(names.iter());

        let body = body.rfold(Scm::null(), |acc, stmt| Scm::cons(stmt, acc));

        let mut body_ast = Sequence.expand(&body, src_map, &body_env)?;

        for (name, exp) in names.iter().zip(defs) {
            let idx = body_env.lookup_variable_index(name).unwrap();
            let def = Expression.expand(&exp, src_map, &body_env)?;
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
    SyntaxDefinition(sexpr, src_map, env):
       (_ {keyword: SymbolName} transformer_spec) => {
            let transformer = SyntaxTransformer.build(transformer_spec, env, src_map)?;
            env.add_global_binding(keyword, transformer);
            Ok(Ast::void())
       }
}

define_form! {
    Import(sexpr, src_map, env):
       (_ . import_sets) => {
           let mut import_ast = Ast::void();
           for import_set in lists::iter(import_sets) {
               let set_ast = Self::process_import_set(&import_set, env, src_map)?;
               import_ast = Ast::sequence(import_ast, set_ast)
           }
           Ok(import_ast)
       }
}

impl Import {
    pub fn process_import_set(import_set: &Scm, env: &Env, src_map: &SourceMap) -> Result<AstNode> {
        let libname = import_set;
        let libstr = libname_to_string(libname);

        if Self::import_all(&libstr, env) {
            return Ok(Ast::void());
        }

        let libexpr = env
            .parse_library(&libstr, src_map)?
            .ok_or_else(|| error_at(&src_map.get(libname), Error::UnknownLibrary))?;
        let libast = LibraryDefinition.expand(&libexpr, src_map, env)?;

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
    fn build(&self, spec: &Scm, env: &Env, src_map: &SourceMap) -> Result<Rc<dyn SyntaxExpander>> {
        with_sexpr_matcher! {
            match spec, {
                ({:"simple-macro"} args body) => {
                    Ok(Rc::new(SimpleMacro::new(args, body, env, src_map)?) as Rc<dyn SyntaxExpander>)
                }
                ({:"syntax-rules"} {ellipsis: Symbol} {literals: List} . rules) => {
                    Ok(Rc::new(SyntaxRules::new(ellipsis, literals, rules, env)?) as Rc<dyn SyntaxExpander>)
                }
                ({:"syntax-rules"} {literals: List} . rules) => {
                    let ellipsis = Scm::symbol("...");
                    Ok(Rc::new(SyntaxRules::new(&ellipsis, literals, rules, env)?) as Rc<dyn SyntaxExpander>)
                }
                _ => { Err(src_map.get(spec).map_value(Error::InvalidForm)) }
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
    fn expand(&self, sexpr: &Scm, src_map: &SourceMap, env: &Env) -> Result<AstNode> {
        let mut substitutions = HashMap::new();
        let mut more_args = sexpr
            .cdr()
            .ok_or_else(|| src_map.get(sexpr).map_value(Error::MissingArgument))?;

        for name in &self.args {
            let arg = more_args
                .car()
                .cloned()
                .map(|a| SyntacticClosure::new(a, env.clone()))
                .map(Scm::obj)
                .ok_or_else(|| src_map.get(more_args).map_value(Error::MissingArgument))?;
            more_args = more_args.cdr().unwrap();
            substitutions.insert(name.as_str(), arg);
        }

        let new_sexpr = Scm::substitute(&self.template, &substitutions);
        Expression.expand(&new_sexpr, src_map, env)
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
    fn expand(&self, expr: &Scm, src_map: &SourceMap, env: &Env) -> Result<AstNode> {
        let sexpr = SyntacticClosure::new_scm(expr.clone(), env.clone());
        for (matcher, template) in &self.rules {
            if let Some(bindings) = matcher.match_value(&sexpr, env) {
                let expanded_sexpr = self
                    .transcriber
                    .transcribe(template, &bindings)
                    .ok_or_else(|| error_at(&src_map.get(template), Error::InvalidTemplate))?;
                //println!("{} => {}", sexpr, expanded_sexpr);
                return Expression.expand(&expanded_sexpr, src_map, env);
            }
        }
        Err(error_at(&src_map.get(expr), Error::InvalidForm))
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
    fn expand(&self, sexpr: &Scm, src_map: &SourceMap, env: &Env) -> Result<AstNode> {
        with_sexpr_matcher! {
            match sexpr, {
                (_ libname . statements) => {
                    let mut lib_env = base_environment(libname);
                    lib_env.use_libraries_from(env);

                    for stmt in lists::iter(statements) {
                        match stmt.car().and_then(|s| s.as_symbol()) {
                            Some("import") => {Import.expand(&stmt, src_map, &lib_env)?;}
                            Some("export") => {}
                            Some("begin") => {}
                            _ => return Err(error_at(&src_map.get(&stmt), Error::UnexpectedStatement)),
                        }
                    }

                    let mut body_parts = vec![];
                    for stmt in lists::iter(statements) {
                        if let Some("begin") = stmt.car().and_then(|s| s.as_symbol()) {
                            body_parts.push(Sequence.expand(stmt.cdr().unwrap(), src_map, &lib_env)?)
                        }
                    }

                    let mut exports = vec![];
                    for stmt in lists::iter(statements) {
                        if let Some("export") = stmt.car().and_then(|s| s.as_symbol()) {
                            for export_item in lists::iter(stmt.cdr().unwrap()) {
                                let export_name = export_item
                                    .as_symbol()
                                    .ok_or_else(|| error_at(&src_map.get(&export_item), Error::ExpectedSymbol))?;
                                let binding = lib_env.lookup_global_variable(export_name).ok_or_else(||error_at(&src_map.get(&export_item), Error::UndefinedExport))?;
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
                _ => { Err(src_map.get(sexpr).map_value(Error::InvalidForm)) }
            }
        }
    }
}
