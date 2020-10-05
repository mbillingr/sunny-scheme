#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::sunny::ast::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::extract_minus_definitions;
}

pub fn extract_minus_definitions(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let node = args[0].clone();{
// (letrec ((transform (lambda (node transform-children) (cond ((eq? (node (quote kind)) (quote DEFINITION)) (extract-definition node)) (else (transform-children))))) (extract-definition (lambda (node) (let ((val (node (quote get-val)))) (cond ((eq? (quote CLOSURE) (val (quote kind))) (if (not (null? (val (quote free-vars)))) (error "Definition with free variables" val)) (global-function-set-value! (node (quote get-var)) (val (quote inner-function))) (make-nop)) (else (make-definition (node (quote get-name)) (node (quote get-var)) val))))))) (node (quote transform) transform))
{
// (let ((transform (quote *uninitialized*)) (extract-definition (quote *uninitialized*))) (begin (set! transform (lambda (node transform-children) (cond ((eq? (node (quote kind)) (quote DEFINITION)) (extract-definition node)) (else (transform-children))))) (set! extract-definition (lambda (node) (let ((val (node (quote get-val)))) (cond ((eq? (quote CLOSURE) (val (quote kind))) (if (not (null? (val (quote free-vars)))) (error "Definition with free variables" val)) (global-function-set-value! (node (quote get-var)) (val (quote inner-function))) (make-nop)) (else (make-definition (node (quote get-name)) (node (quote get-var)) val)))))) (node (quote transform) transform)))
{let [transform, extract_minus_definition, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let extract_minus_definition = extract_minus_definition.into_boxed();{let transform = transform.into_boxed();{transform.set({// Closure
let extract_minus_definition = extract_minus_definition.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let node = args[0].clone();let transform_minus_children = args[1].clone();{
// (cond ...)
if ({
// (eq? (node (quote kind)) (quote DEFINITION))
imports::eq_p.with(|value| value.get()).invoke(&[{
// (node (quote kind))
node.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("DEFINITION")])}).is_true() {{
// (extract-definition node)
extract_minus_definition.get().invoke(&[node.clone()])}} else {{
// (transform-children)
transform_minus_children.clone().invoke(&[])}}}})});extract_minus_definition.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let node = args[0].clone();{
// (let ((val (node (quote get-val)))) (cond ((eq? (quote CLOSURE) (val (quote kind))) (if (not (null? (val (quote free-vars)))) (error "Definition with free variables" val)) (global-function-set-value! (node (quote get-var)) (val (quote inner-function))) (make-nop)) (else (make-definition (node (quote get-name)) (node (quote get-var)) val))))
{let val = {
// (node (quote get-val))
node.clone().invoke(&[Scm::symbol("get-val")])};{
// (cond ...)
if ({
// (eq? (quote CLOSURE) (val (quote kind)))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("CLOSURE"),{
// (val (quote kind))
val.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{if ({
// (not (null? (val (quote free-vars))))
imports::not.with(|value| value.get()).invoke(&[{
// (null? (val (quote free-vars)))
imports::null_p.with(|value| value.get()).invoke(&[{
// (val (quote free-vars))
val.clone().invoke(&[Scm::symbol("free-vars")])}])}])}).is_true() {{
// (error "Definition with free variables" val)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Definition with free variables"),val.clone()])}} else {Scm::symbol("*UNSPECIFIED*")};{
// (global-function-set-value! (node (quote get-var)) (val (quote inner-function)))
imports::global_minus_function_minus_set_minus_value_i.with(|value| value.get()).invoke(&[{
// (node (quote get-var))
node.clone().invoke(&[Scm::symbol("get-var")])},{
// (val (quote inner-function))
val.clone().invoke(&[Scm::symbol("inner-function")])}])};{
// (make-nop)
imports::make_minus_nop.with(|value| value.get()).invoke(&[])}}} else {{
// (make-definition (node (quote get-name)) (node (quote get-var)) val)
imports::make_minus_definition.with(|value| value.get()).invoke(&[{
// (node (quote get-name))
node.clone().invoke(&[Scm::symbol("get-name")])},{
// (node (quote get-var))
node.clone().invoke(&[Scm::symbol("get-var")])},val.clone()])}}}}}})});{
// (node (quote transform) transform)
node.clone().invoke(&[Scm::symbol("transform"),transform.get()])}}}}}}}}.into()
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    crate::sunny::ast::initialize();
    crate::sunny::variable::initialize();
    {
        (/*NOP*/);
        {
            // (define (extract-definitions node) ...)
            (/*NOP*/)
        }
    };
}
