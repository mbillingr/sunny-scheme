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
    {if args.len() != 2{panic!("invalid arity")}let rename__0 = args[0].clone();let node__18 = args[1].clone();{
// (letrec ((renamed (quote ())) (renamed? (lambda (var) (memq var renamed))) (do-rename! (lambda (var) (if (renamed? var) (quote DONE) (begin (variable-set-name! var (rename (variable-name var) var)) (set! renamed (cons var renamed)))))) (transform (lambda (node transform-children) (cond ((eq? (node (quote kind)) (quote REFERENCE)) (do-rename! (node (quote get-var)))) ((eq? (node (quote kind)) (quote ASSIGNMENT)) (do-rename! (node (quote get-var))))) (transform-children)))) (node (quote transform) transform))
{
// (let ((renamed (quote *uninitialized*)) (renamed? (quote *uninitialized*)) (do-rename! (quote *uninitialized*)) (transform (quote *uninitialized*))) (begin (set! renamed (quote ())) (set! renamed? (lambda (var) (memq var renamed))) (set! do-rename! (lambda (var) (if (renamed? var) (quote DONE) (begin (variable-set-name! var (rename (variable-name var) var)) (set! renamed (cons var renamed)))))) (set! transform (lambda (node transform-children) (cond ((eq? (node (quote kind)) (quote REFERENCE)) (do-rename! (node (quote get-var)))) ((eq? (node (quote kind)) (quote ASSIGNMENT)) (do-rename! (node (quote get-var))))) (transform-children))) (node (quote transform) transform)))
{let [renamed__0, renamed_p__0, do_minus_rename_i__0, transform__28, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let transform__28 = transform__28.into_boxed();{let do_minus_rename_i__0 = do_minus_rename_i__0.into_boxed();{let renamed_p__0 = renamed_p__0.into_boxed();{let renamed__0 = renamed__0.into_boxed();{renamed__0.set(Scm::Nil);Scm::anything();renamed_p__0.set({// Closure
let renamed__0 = renamed__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let var__21 = args[0].clone();{
// (memq var renamed)
imports::memq(&[var__21.clone(),renamed__0.get()])}})});Scm::anything();do_minus_rename_i__0.set({// Closure
let renamed_p__0 = renamed_p__0.clone();let rename__0 = rename__0.clone();let renamed__0 = renamed__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let var__22 = args[0].clone();if ({
// (renamed? var)
renamed_p__0.get().invoke(&[var__22.clone()])}).is_true() {Scm::symbol("DONE")} else {{{
// (variable-set-name! var (rename (variable-name var) var))
imports::variable_minus_set_minus_name_i(&[var__22.clone(),{
// (rename (variable-name var) var)
rename__0.clone().invoke(&[{
// (variable-name var)
imports::variable_minus_name(&[var__22.clone()])},var__22.clone()])}])};renamed__0.set({
// (cons var renamed)
imports::cons(&[var__22.clone(),renamed__0.get()])});Scm::anything()}}})});Scm::anything();transform__28.set({// Closure
let do_minus_rename_i__0 = do_minus_rename_i__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let node__17 = args[0].clone();let transform_minus_children__2 = args[1].clone();{{
// (cond ...)
if ({
// (eq? (node (quote kind)) (quote REFERENCE))
imports::eq_p(&[{
// (node (quote kind))
node__17.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("REFERENCE")])}).is_true() {{
// (do-rename! (node (quote get-var)))
do_minus_rename_i__0.get().invoke(&[{
// (node (quote get-var))
node__17.clone().invoke(&[Scm::symbol("get-var")])}])}} else if ({
// (eq? (node (quote kind)) (quote ASSIGNMENT))
imports::eq_p(&[{
// (node (quote kind))
node__17.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("ASSIGNMENT")])}).is_true() {{
// (do-rename! (node (quote get-var)))
do_minus_rename_i__0.get().invoke(&[{
// (node (quote get-var))
node__17.clone().invoke(&[Scm::symbol("get-var")])}])}} else {Scm::symbol("*UNSPECIFIED*")}};{
// (transform-children)
transform_minus_children__2.clone().invoke(&[])}}})});Scm::anything();{
// (node (quote transform) transform)
node__18.clone().invoke(&[Scm::symbol("transform"),transform__28.get()])}}}}}}}}}}.into()
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
