#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::sunny::ast::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::rename_minus_vars;
}

pub fn rename_minus_vars(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let rename_0 = args[0].clone();let node_17 = args[1].clone();{
// (letrec ((renamed (quote ())) (renamed? (lambda (var) (memq var renamed))) (do-rename! (lambda (var) (if (renamed? var) (quote DONE) (begin (variable-set-name! var (rename (variable-name var) var)) (set! renamed (cons var renamed)))))) (transform (lambda (node transform-children) (cond ((eq? (node (quote kind)) (quote REFERENCE)) (do-rename! (node (quote get-var)))) ((eq? (node (quote kind)) (quote ASSIGNMENT)) (do-rename! (node (quote get-var)))) ((eq? (node (quote kind)) (quote DEFINITION)) (do-rename! (node (quote get-var)))) ((eq? (node (quote kind)) (quote FN-APPLICATION)) (do-rename! (node (quote get-var)))) ((eq? (node (quote kind)) (quote EXPORT)) (do-rename! (node (quote get-var)))) ((eq? (node (quote kind)) (quote FIXLET)) (for-each do-rename! (node (quote get-vars)))) ((eq? (node (quote kind)) (quote ABSTRACTION)) (for-each do-rename! (node (quote get-vars)))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (for-each do-rename! (cons (node (quote get-varvar)) (node (quote get-vars)))))) (transform-children)))) (node (quote transform) transform))
{
// (let ((renamed (quote *uninitialized*)) (renamed? (quote *uninitialized*)) (do-rename! (quote *uninitialized*)) (transform (quote *uninitialized*))) (begin (set! renamed (quote ())) (set! renamed? (lambda (var) (memq var renamed))) (set! do-rename! (lambda (var) (if (renamed? var) (quote DONE) (begin (variable-set-name! var (rename (variable-name var) var)) (set! renamed (cons var renamed)))))) (set! transform (lambda (node transform-children) (cond ((eq? (node (quote kind)) (quote REFERENCE)) (do-rename! (node (quote get-var)))) ((eq? (node (quote kind)) (quote ASSIGNMENT)) (do-rename! (node (quote get-var)))) ((eq? (node (quote kind)) (quote DEFINITION)) (do-rename! (node (quote get-var)))) ((eq? (node (quote kind)) (quote FN-APPLICATION)) (do-rename! (node (quote get-var)))) ((eq? (node (quote kind)) (quote EXPORT)) (do-rename! (node (quote get-var)))) ((eq? (node (quote kind)) (quote FIXLET)) (for-each do-rename! (node (quote get-vars)))) ((eq? (node (quote kind)) (quote ABSTRACTION)) (for-each do-rename! (node (quote get-vars)))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (for-each do-rename! (cons (node (quote get-varvar)) (node (quote get-vars)))))) (transform-children))) (node (quote transform) transform)))
{let [renamed_0, renamed_p_0, do_minus_rename_i_0, transform_28, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let transform_28 = transform_28.into_boxed();{let do_minus_rename_i_0 = do_minus_rename_i_0.into_boxed();{let renamed_p_0 = renamed_p_0.into_boxed();{let renamed_0 = renamed_0.into_boxed();{renamed_0.set(Scm::Nil);Scm::anything();renamed_p_0.set({// Closure
let renamed_0 = renamed_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let var_21 = args[0].clone();{
// (memq var renamed)
imports::memq(&[var_21.clone(),renamed_0.get()])}})});Scm::anything();do_minus_rename_i_0.set({// Closure
let renamed_p_0 = renamed_p_0.clone();let rename_0 = rename_0.clone();let renamed_0 = renamed_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let var_22 = args[0].clone();if ({
// (renamed? var)
renamed_p_0.get().invoke(&[var_22.clone()])}).is_true() {Scm::symbol("DONE")} else {{{
// (variable-set-name! var (rename (variable-name var) var))
imports::variable_minus_set_minus_name_i(&[var_22.clone(),{
// (rename (variable-name var) var)
rename_0.clone().invoke(&[{
// (variable-name var)
imports::variable_minus_name(&[var_22.clone()])},var_22.clone()])}])};renamed_0.set({
// (cons var renamed)
imports::cons(&[var_22.clone(),renamed_0.get()])});Scm::anything()}}})});Scm::anything();transform_28.set({// Closure
let do_minus_rename_i_0 = do_minus_rename_i_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let node_18 = args[0].clone();let transform_minus_children_2 = args[1].clone();{{
// (cond ...)
if ({
// (eq? (node (quote kind)) (quote REFERENCE))
imports::eq_p(&[{
// (node (quote kind))
node_18.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("REFERENCE")])}).is_true() {{
// (do-rename! (node (quote get-var)))
do_minus_rename_i_0.get().invoke(&[{
// (node (quote get-var))
node_18.clone().invoke(&[Scm::symbol("get-var")])}])}} else if ({
// (eq? (node (quote kind)) (quote ASSIGNMENT))
imports::eq_p(&[{
// (node (quote kind))
node_18.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("ASSIGNMENT")])}).is_true() {{
// (do-rename! (node (quote get-var)))
do_minus_rename_i_0.get().invoke(&[{
// (node (quote get-var))
node_18.clone().invoke(&[Scm::symbol("get-var")])}])}} else if ({
// (eq? (node (quote kind)) (quote DEFINITION))
imports::eq_p(&[{
// (node (quote kind))
node_18.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("DEFINITION")])}).is_true() {{
// (do-rename! (node (quote get-var)))
do_minus_rename_i_0.get().invoke(&[{
// (node (quote get-var))
node_18.clone().invoke(&[Scm::symbol("get-var")])}])}} else if ({
// (eq? (node (quote kind)) (quote FN-APPLICATION))
imports::eq_p(&[{
// (node (quote kind))
node_18.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("FN-APPLICATION")])}).is_true() {{
// (do-rename! (node (quote get-var)))
do_minus_rename_i_0.get().invoke(&[{
// (node (quote get-var))
node_18.clone().invoke(&[Scm::symbol("get-var")])}])}} else if ({
// (eq? (node (quote kind)) (quote EXPORT))
imports::eq_p(&[{
// (node (quote kind))
node_18.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("EXPORT")])}).is_true() {{
// (do-rename! (node (quote get-var)))
do_minus_rename_i_0.get().invoke(&[{
// (node (quote get-var))
node_18.clone().invoke(&[Scm::symbol("get-var")])}])}} else if ({
// (eq? (node (quote kind)) (quote FIXLET))
imports::eq_p(&[{
// (node (quote kind))
node_18.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("FIXLET")])}).is_true() {{
// (for-each do-rename! (node (quote get-vars)))
imports::for_minus_each(&[do_minus_rename_i_0.get(),{
// (node (quote get-vars))
node_18.clone().invoke(&[Scm::symbol("get-vars")])}])}} else if ({
// (eq? (node (quote kind)) (quote ABSTRACTION))
imports::eq_p(&[{
// (node (quote kind))
node_18.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("ABSTRACTION")])}).is_true() {{
// (for-each do-rename! (node (quote get-vars)))
imports::for_minus_each(&[do_minus_rename_i_0.get(),{
// (node (quote get-vars))
node_18.clone().invoke(&[Scm::symbol("get-vars")])}])}} else if ({
// (eq? (node (quote kind)) (quote VARARG-ABSTRACTION))
imports::eq_p(&[{
// (node (quote kind))
node_18.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("VARARG-ABSTRACTION")])}).is_true() {{
// (for-each do-rename! (cons (node (quote get-varvar)) (node (quote get-vars))))
imports::for_minus_each(&[do_minus_rename_i_0.get(),{
// (cons (node (quote get-varvar)) (node (quote get-vars)))
imports::cons(&[{
// (node (quote get-varvar))
node_18.clone().invoke(&[Scm::symbol("get-varvar")])},{
// (node (quote get-vars))
node_18.clone().invoke(&[Scm::symbol("get-vars")])}])}])}} else {Scm::symbol("*UNSPECIFIED*")}};{
// (transform-children)
transform_minus_children_2.clone().invoke(&[])}}})});Scm::anything();{
// (node (quote transform) transform)
node_17.clone().invoke(&[Scm::symbol("transform"),transform_28.get()])}}}}}}}}}}.into()
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
            // (define (rename-vars rename node) ...)
            (/*NOP*/)
        }
    };
}
