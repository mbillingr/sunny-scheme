#[allow(unused_imports)] use sunny_core::{Mut, Scm};
mod imports{pub use crate::scheme::base::exports::{car, cdr, caar, cadr, cdar, cddr, };
}

pub mod exports{pub use super::globals::caaar as caaar;
pub use super::globals::caadr as caadr;
pub use super::globals::cadar as cadar;
pub use super::globals::caddr as caddr;
pub use super::globals::cdaar as cdaar;
pub use super::globals::cdadr as cdadr;
pub use super::globals::cddar as cddar;
pub use super::globals::cdddr as cdddr;
pub use super::globals::caaaar as caaaar;
pub use super::globals::caaadr as caaadr;
pub use super::globals::caadar as caadar;
pub use super::globals::caaddr as caaddr;
pub use super::globals::cadaar as cadaar;
pub use super::globals::cadadr as cadadr;
pub use super::globals::caddar as caddar;
pub use super::globals::cadddr as cadddr;
pub use super::globals::cdaaar as cdaaar;
pub use super::globals::cdaadr as cdaadr;
pub use super::globals::cdadar as cdadar;
pub use super::globals::cdaddr as cdaddr;
pub use super::globals::cddaar as cddaar;
pub use super::globals::cddadr as cddadr;
pub use super::globals::cdddar as cdddar;
pub use super::globals::cddddr as cddddr;
}

mod globals{use sunny_core::{Mut, Scm};
thread_local!{#[allow(non_upper_case_globals)] pub static cddddr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cddddr"))}
thread_local!{#[allow(non_upper_case_globals)] pub static cdddar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdddar"))}
thread_local!{#[allow(non_upper_case_globals)] pub static cddadr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cddadr"))}
thread_local!{#[allow(non_upper_case_globals)] pub static cddaar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cddaar"))}
thread_local!{#[allow(non_upper_case_globals)] pub static cdaddr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdaddr"))}
thread_local!{#[allow(non_upper_case_globals)] pub static cdadar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdadar"))}
thread_local!{#[allow(non_upper_case_globals)] pub static cdaadr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdaadr"))}
thread_local!{#[allow(non_upper_case_globals)] pub static cdaaar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdaaar"))}
thread_local!{#[allow(non_upper_case_globals)] pub static cadddr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cadddr"))}
thread_local!{#[allow(non_upper_case_globals)] pub static caddar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caddar"))}
thread_local!{#[allow(non_upper_case_globals)] pub static cadadr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cadadr"))}
thread_local!{#[allow(non_upper_case_globals)] pub static cadaar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cadaar"))}
thread_local!{#[allow(non_upper_case_globals)] pub static caaddr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caaddr"))}
thread_local!{#[allow(non_upper_case_globals)] pub static caadar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caadar"))}
thread_local!{#[allow(non_upper_case_globals)] pub static caaadr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caaadr"))}
thread_local!{#[allow(non_upper_case_globals)] pub static caaaar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caaaar"))}
thread_local!{#[allow(non_upper_case_globals)] pub static cdddr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdddr"))}
thread_local!{#[allow(non_upper_case_globals)] pub static cddar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cddar"))}
thread_local!{#[allow(non_upper_case_globals)] pub static cdadr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdadr"))}
thread_local!{#[allow(non_upper_case_globals)] pub static cdaar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdaar"))}
thread_local!{#[allow(non_upper_case_globals)] pub static caddr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caddr"))}
thread_local!{#[allow(non_upper_case_globals)] pub static cadar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cadar"))}
thread_local!{#[allow(non_upper_case_globals)] pub static caadr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caadr"))}
thread_local!{#[allow(non_upper_case_globals)] pub static caaar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caaar"))}

}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
if INITIALIZED.with(|x| x.get()) { return }
INITIALIZED.with(|x| x.set(true));

crate::scheme::base::initialize();
{(/*NOP*/);
// (define (caaar x) (car (caar x)))
globals::caaar.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (car (caar x)))
{
// (car (caar x))
imports::car.with(|value| value.get()).invoke(&[
// (caar x)
imports::caar.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (caadr x) (car (cadr x)))
globals::caadr.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (car (cadr x)))
{
// (car (cadr x))
imports::car.with(|value| value.get()).invoke(&[
// (cadr x)
imports::cadr.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (cadar x) (car (cdar x)))
globals::cadar.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (car (cdar x)))
{
// (car (cdar x))
imports::car.with(|value| value.get()).invoke(&[
// (cdar x)
imports::cdar.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (caddr x) (car (cddr x)))
globals::caddr.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (car (cddr x)))
{
// (car (cddr x))
imports::car.with(|value| value.get()).invoke(&[
// (cddr x)
imports::cddr.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (cdaar x) (cdr (caar x)))
globals::cdaar.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (cdr (caar x)))
{
// (cdr (caar x))
imports::cdr.with(|value| value.get()).invoke(&[
// (caar x)
imports::caar.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (cdadr x) (cdr (cadr x)))
globals::cdadr.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (cdr (cadr x)))
{
// (cdr (cadr x))
imports::cdr.with(|value| value.get()).invoke(&[
// (cadr x)
imports::cadr.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (cddar x) (cdr (cdar x)))
globals::cddar.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (cdr (cdar x)))
{
// (cdr (cdar x))
imports::cdr.with(|value| value.get()).invoke(&[
// (cdar x)
imports::cdar.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (cdddr x) (cdr (cddr x)))
globals::cdddr.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (cdr (cddr x)))
{
// (cdr (cddr x))
imports::cdr.with(|value| value.get()).invoke(&[
// (cddr x)
imports::cddr.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (caaaar x) (caar (caar x)))
globals::caaaar.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (caar (caar x)))
{
// (caar (caar x))
imports::caar.with(|value| value.get()).invoke(&[
// (caar x)
imports::caar.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (caaadr x) (caar (cadr x)))
globals::caaadr.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (caar (cadr x)))
{
// (caar (cadr x))
imports::caar.with(|value| value.get()).invoke(&[
// (cadr x)
imports::cadr.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (caadar x) (caar (cdar x)))
globals::caadar.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (caar (cdar x)))
{
// (caar (cdar x))
imports::caar.with(|value| value.get()).invoke(&[
// (cdar x)
imports::cdar.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (caaddr x) (caar (cddr x)))
globals::caaddr.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (caar (cddr x)))
{
// (caar (cddr x))
imports::caar.with(|value| value.get()).invoke(&[
// (cddr x)
imports::cddr.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (cadaar x) (cadr (caar x)))
globals::cadaar.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (cadr (caar x)))
{
// (cadr (caar x))
imports::cadr.with(|value| value.get()).invoke(&[
// (caar x)
imports::caar.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (cadadr x) (cadr (cadr x)))
globals::cadadr.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (cadr (cadr x)))
{
// (cadr (cadr x))
imports::cadr.with(|value| value.get()).invoke(&[
// (cadr x)
imports::cadr.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (caddar x) (cadr (cdar x)))
globals::caddar.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (cadr (cdar x)))
{
// (cadr (cdar x))
imports::cadr.with(|value| value.get()).invoke(&[
// (cdar x)
imports::cdar.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (cadddr x) (cadr (cddr x)))
globals::cadddr.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (cadr (cddr x)))
{
// (cadr (cddr x))
imports::cadr.with(|value| value.get()).invoke(&[
// (cddr x)
imports::cddr.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (cdaaar x) (cdar (caar x)))
globals::cdaaar.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (cdar (caar x)))
{
// (cdar (caar x))
imports::cdar.with(|value| value.get()).invoke(&[
// (caar x)
imports::caar.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (cdaadr x) (cdar (cadr x)))
globals::cdaadr.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (cdar (cadr x)))
{
// (cdar (cadr x))
imports::cdar.with(|value| value.get()).invoke(&[
// (cadr x)
imports::cadr.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (cdadar x) (cdar (cdar x)))
globals::cdadar.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (cdar (cdar x)))
{
// (cdar (cdar x))
imports::cdar.with(|value| value.get()).invoke(&[
// (cdar x)
imports::cdar.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (cdaddr x) (cdar (cddr x)))
globals::cdaddr.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (cdar (cddr x)))
{
// (cdar (cddr x))
imports::cdar.with(|value| value.get()).invoke(&[
// (cddr x)
imports::cddr.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (cddaar x) (cddr (caar x)))
globals::cddaar.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (cddr (caar x)))
{
// (cddr (caar x))
imports::cddr.with(|value| value.get()).invoke(&[
// (caar x)
imports::caar.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (cddadr x) (cddr (cadr x)))
globals::cddadr.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (cddr (cadr x)))
{
// (cddr (cadr x))
imports::cddr.with(|value| value.get()).invoke(&[
// (cadr x)
imports::cadr.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (cdddar x) (cddr (cdar x)))
globals::cdddar.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (cddr (cdar x)))
{
// (cddr (cdar x))
imports::cddr.with(|value| value.get()).invoke(&[
// (cdar x)
imports::cdar.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}));
// (define (cddddr x) (cddr (cddr x)))
globals::cddddr.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let x = args[0].clone();
// (letrec () (cddr (cddr x)))
{
// (cddr (cddr x))
imports::cddr.with(|value| value.get()).invoke(&[
// (cddr x)
imports::cddr.with(|value| value.get()).invoke(&[x.clone(), ]), ])}})}))};}
