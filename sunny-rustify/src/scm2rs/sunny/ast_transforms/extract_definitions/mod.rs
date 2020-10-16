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
    {if args.len() != 1{panic!("invalid arity")}let node_14 = args[0].clone();{
// (letrec ((transform (lambda (node transform-children) (cond ((eq? (node (quote kind)) (quote DEFINITION)) (extract-definition node)) (else (transform-children))))) (extract-definition (lambda (node) (let ((val (node (quote get-val)))) (cond ((eq? (quote CLOSURE) (val (quote kind))) (if (not (null? (val (quote free-vars)))) (error "Definition with free variables" (variable-name (node (quote get-var))) (val (quote free-vars)))) (global-function-set-value! (node (quote get-var)) (val (quote inner-function))) (make-nop)) (else (make-definition (node (quote get-var)) val))))))) (node (quote transform) transform))
{
// (let ((transform (quote *uninitialized*)) (extract-definition (quote *uninitialized*))) (begin (set! transform (lambda (node transform-children) (cond ((eq? (node (quote kind)) (quote DEFINITION)) (extract-definition node)) (else (transform-children))))) (set! extract-definition (lambda (node) (let ((val (node (quote get-val)))) (cond ((eq? (quote CLOSURE) (val (quote kind))) (if (not (null? (val (quote free-vars)))) (error "Definition with free variables" (variable-name (node (quote get-var))) (val (quote free-vars)))) (global-function-set-value! (node (quote get-var)) (val (quote inner-function))) (make-nop)) (else (make-definition (node (quote get-var)) val)))))) (node (quote transform) transform)))
{let [transform_27, extract_minus_definition_0, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let extract_minus_definition_0 = extract_minus_definition_0.into_boxed();{let transform_27 = transform_27.into_boxed();{transform_27.set({// Closure
let extract_minus_definition_0 = extract_minus_definition_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let node_15 = args[0].clone();let transform_minus_children_1 = args[1].clone();{
// (cond ...)
if ({
// (eq? (node (quote kind)) (quote DEFINITION))
imports::eq_p(&[{
// (node (quote kind))
node_15.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("DEFINITION")])}).is_true() {{
// (extract-definition node)
extract_minus_definition_0.get().invoke(&[node_15.clone()])}} else {{
// (transform-children)
transform_minus_children_1.clone().invoke(&[])}}}})});Scm::anything();extract_minus_definition_0.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let node_16 = args[0].clone();{
// (let ((val (node (quote get-val)))) (cond ((eq? (quote CLOSURE) (val (quote kind))) (if (not (null? (val (quote free-vars)))) (error "Definition with free variables" (variable-name (node (quote get-var))) (val (quote free-vars)))) (global-function-set-value! (node (quote get-var)) (val (quote inner-function))) (make-nop)) (else (make-definition (node (quote get-var)) val))))
{let val_6 = {
// (node (quote get-val))
node_16.clone().invoke(&[Scm::symbol("get-val")])};{
// (cond ...)
if ({
// (eq? (quote CLOSURE) (val (quote kind)))
imports::eq_p(&[Scm::symbol("CLOSURE"),{
// (val (quote kind))
val_6.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{if ({
// (not (null? (val (quote free-vars))))
imports::not(&[{
// (null? (val (quote free-vars)))
imports::null_p(&[{
// (val (quote free-vars))
val_6.clone().invoke(&[Scm::symbol("free-vars")])}])}])}).is_true() {{
// (error "Definition with free variables" (variable-name (node (quote get-var))) (val (quote free-vars)))
imports::error(&[Scm::from("Definition with free variables"),{
// (variable-name (node (quote get-var)))
imports::variable_minus_name(&[{
// (node (quote get-var))
node_16.clone().invoke(&[Scm::symbol("get-var")])}])},{
// (val (quote free-vars))
val_6.clone().invoke(&[Scm::symbol("free-vars")])}])}} else {Scm::symbol("*UNSPECIFIED*")};{
// (global-function-set-value! (node (quote get-var)) (val (quote inner-function)))
imports::global_minus_function_minus_set_minus_value_i(&[{
// (node (quote get-var))
node_16.clone().invoke(&[Scm::symbol("get-var")])},{
// (val (quote inner-function))
val_6.clone().invoke(&[Scm::symbol("inner-function")])}])};{
// (make-nop)
imports::make_minus_nop(&[])}}} else {{
// (make-definition (node (quote get-var)) val)
imports::make_minus_definition(&[{
// (node (quote get-var))
node_16.clone().invoke(&[Scm::symbol("get-var")])},val_6.clone()])}}}}}})});Scm::anything();{
// (node (quote transform) transform)
node_14.clone().invoke(&[Scm::symbol("transform"),transform_27.get()])}}}}}}}}.into()
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
