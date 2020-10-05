#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
}

pub mod exports {
    pub use super::ancestor_p;
    pub use super::call_minus_method;
    pub use super::clone;
    pub use super::fields;
    pub use super::get_minus_field;
    pub use super::make_minus_table;
    pub use super::parent;
    pub use super::replace_minus_table_i;
    pub use super::run_minus_tests;
    pub use super::set_minus_field_i;
    pub use super::set_minus_parent_i;
    pub use super::table_p;
}

thread_local! {#[allow(non_upper_case_globals)] pub static TABLE_minus_ID: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE TABLE-ID"))}
pub fn ancestor_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let obj = args[0].clone();
        let ancestor = args[1].clone();
        {
            // (and (table? obj) (if (eq? (parent obj) ancestor) #t (ancestor? (parent obj) ancestor)))
            if ({
                // (table? obj)
                Scm::func(table_p).invoke(&[obj.clone()])
            })
            .is_true()
            {
                if ({
                    // (eq? (parent obj) ancestor)
                    imports::eq_p.with(|value| value.get()).invoke(&[
                        {
                            // (parent obj)
                            Scm::func(parent).invoke(&[obj.clone()])
                        },
                        ancestor.clone(),
                    ])
                })
                .is_true()
                {
                    Scm::True
                } else {
                    {
                        // (ancestor? (parent obj) ancestor)
                        Scm::func(ancestor_p).invoke(&[
                            {
                                // (parent obj)
                                Scm::func(parent).invoke(&[obj.clone()])
                            },
                            ancestor.clone(),
                        ])
                    }
                }
            } else {
                Scm::False
            }
        }
    }
    .into()
}
pub fn call_minus_method(args: &[Scm]) -> Scm {
    {
        if args.len() < 2 {
            panic!("not enough args")
        }
        let table = args[0].clone();
        let key = args[1].clone();
        let args_ = Scm::list(&args[2..]);
        {
            // (apply (get-field table key) table args)
            imports::apply.with(|value| value.get()).invoke(&[
                {
                    // (get-field table key)
                    Scm::func(get_minus_field).invoke(&[table.clone(), key.clone()])
                },
                table.clone(),
                args_.clone(),
            ])
        }
    }
    .into()
}
pub fn clone(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let table = args[0].clone();
        {
            // (list TABLE-ID table)
            imports::list
                .with(|value| value.get())
                .invoke(&[TABLE_minus_ID.with(|value| value.get()), table.clone()])
        }
    }
    .into()
}
pub fn fields(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let table = args[0].clone();
        {
            // (cddr table)
            imports::cddr
                .with(|value| value.get())
                .invoke(&[table.clone()])
        }
    }
    .into()
}
pub fn get_minus_field(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let table = args[0].clone();
        let key = args[1].clone();
        {
            // (let ((entry (assq key (fields table)))) (cond (entry (cdr entry)) ((parent table) (get-field (parent table) key)) (else #f)))
            {
                let entry = {
                    // (assq key (fields table))
                    imports::assq
                        .with(|value| value.get())
                        .invoke(&[key.clone(), {
                            // (fields table)
                            Scm::func(fields).invoke(&[table.clone()])
                        }])
                };
                {
                    // (cond ...)
                    if (entry.clone()).is_true() {
                        {
                            // (cdr entry)
                            imports::cdr
                                .with(|value| value.get())
                                .invoke(&[entry.clone()])
                        }
                    } else if ({
                        // (parent table)
                        Scm::func(parent).invoke(&[table.clone()])
                    })
                    .is_true()
                    {
                        {
                            // (get-field (parent table) key)
                            Scm::func(get_minus_field).invoke(&[
                                {
                                    // (parent table)
                                    Scm::func(parent).invoke(&[table.clone()])
                                },
                                key.clone(),
                            ])
                        }
                    } else {
                        Scm::False
                    }
                }
            }
        }
    }
    .into()
}
pub fn make_minus_table(args: &[Scm]) -> Scm {
    {
        if args.len() != 0 {
            panic!("invalid arity")
        }
        {
            // (list TABLE-ID #f)
            imports::list
                .with(|value| value.get())
                .invoke(&[TABLE_minus_ID.with(|value| value.get()), Scm::False])
        }
    }
    .into()
}
pub fn parent(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let table = args[0].clone();
        {
            // (cadr table)
            imports::cadr
                .with(|value| value.get())
                .invoke(&[table.clone()])
        }
    }
    .into()
}
pub fn replace_minus_table_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let table = args[0].clone();
        let source = args[1].clone();
        {
            {
                // (set-parent! table (parent source))
                Scm::func(set_minus_parent_i).invoke(&[table.clone(), {
                    // (parent source)
                    Scm::func(parent).invoke(&[source.clone()])
                }])
            };
            {
                // (set-fields! table (fields source))
                Scm::func(set_minus_fields_i).invoke(&[table.clone(), {
                    // (fields source)
                    Scm::func(fields).invoke(&[source.clone()])
                }])
            }
        }
    }
    .into()
}
pub fn run_minus_tests(args: &[Scm]) -> Scm {
    {if args.len() != 0{panic!("invalid arity")}#[cfg(test)]
mod tests {
use super::*;
#[test]
fn new_table() {
super::initialize();
{
// (let* ((t (make-table))) (begin (assert (table? t))))
{
// (let ((t (make-table))) (begin (begin (assert (table? t)))))
{let t = {
// (make-table)
Scm::func(make_minus_table).invoke(&[])};assert!({
// (table? t)
Scm::func(table_p).invoke(&[t.clone()])}.is_true());
}}}}
#[test]
fn cant_fake_tables() {
super::initialize();
{
// (let* ((t (list (cons (quote <table>) (quote ()))))) (begin (assert (not (table? t)))))
{
// (let ((t (list (cons (quote <table>) (quote ()))))) (begin (begin (assert (not (table? t))))))
{let t = {
// (list (cons (quote <table>) (quote ())))
imports::list.with(|value| value.get()).invoke(&[{
// (cons (quote <table>) (quote ()))
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("<table>"),Scm::Nil])}])};assert!({
// (not (table? t))
imports::not.with(|value| value.get()).invoke(&[{
// (table? t)
Scm::func(table_p).invoke(&[t.clone()])}])}.is_true());
}}}}
#[test]
fn empty_table_has_no_parent() {
super::initialize();
{
// (let* ((t (make-table))) (begin (assert (not (parent t)))))
{
// (let ((t (make-table))) (begin (begin (assert (not (parent t))))))
{let t = {
// (make-table)
Scm::func(make_minus_table).invoke(&[])};assert!({
// (not (parent t))
imports::not.with(|value| value.get()).invoke(&[{
// (parent t)
Scm::func(parent).invoke(&[t.clone()])}])}.is_true());
}}}}
#[test]
fn cloned_table_has_parent() {
super::initialize();
{
// (let* ((t (make-table))) (let ((s (clone t))) (begin (assert (eq? (parent s) t)))))
{
// (let ((t (make-table))) (begin (let ((s (clone t))) (begin (assert (eq? (parent s) t))))))
{let t = {
// (make-table)
Scm::func(make_minus_table).invoke(&[])};
// (let ((s (clone t))) (begin (assert (eq? (parent s) t))))
let s = {
// (clone t)
Scm::func(clone).invoke(&[t.clone()])};assert!({
// (eq? (parent s) t)
imports::eq_p.with(|value| value.get()).invoke(&[{
// (parent s)
Scm::func(parent).invoke(&[s.clone()])},t.clone()])}.is_true());
}}}}
#[test]
fn empty_table_has_no_fields() {
super::initialize();
{
// (let* ((t (make-table))) (let ((f (fields t))) (begin (assert (null? f)))))
{
// (let ((t (make-table))) (begin (let ((f (fields t))) (begin (assert (null? f))))))
{let t = {
// (make-table)
Scm::func(make_minus_table).invoke(&[])};
// (let ((f (fields t))) (begin (assert (null? f))))
let f = {
// (fields t)
Scm::func(fields).invoke(&[t.clone()])};assert!({
// (null? f)
imports::null_p.with(|value| value.get()).invoke(&[f.clone()])}.is_true());
}}}}
#[test]
fn access_missing_field() {
super::initialize();
{
// (let* ((t (make-table))) (let ((value (get-field t (quote x)))) (begin (assert (not value)))))
{
// (let ((t (make-table))) (begin (let ((value (get-field t (quote x)))) (begin (assert (not value))))))
{let t = {
// (make-table)
Scm::func(make_minus_table).invoke(&[])};
// (let ((value (get-field t (quote x)))) (begin (assert (not value))))
let value = {
// (get-field t (quote x))
Scm::func(get_minus_field).invoke(&[t.clone(),Scm::symbol("x")])};assert!({
// (not value)
imports::not.with(|value| value.get()).invoke(&[value.clone()])}.is_true());
}}}}
#[test]
fn insert_and_retrieve_field() {
super::initialize();
{
// (let* ((t (make-table))) (begin (set-field! t (quote x) 1) (begin (assert (= (get-field t (quote x)) 1)))))
{
// (let ((t (make-table))) (begin (begin (set-field! t (quote x) 1) (begin (assert (= (get-field t (quote x)) 1))))))
{let t = {
// (make-table)
Scm::func(make_minus_table).invoke(&[])};{{
// (set-field! t (quote x) 1)
Scm::func(set_minus_field_i).invoke(&[t.clone(),Scm::symbol("x"),Scm::from(1)])};assert!({
// (= (get-field t (quote x)) 1)
imports::_e_.with(|value| value.get()).invoke(&[{
// (get-field t (quote x))
Scm::func(get_minus_field).invoke(&[t.clone(),Scm::symbol("x")])},Scm::from(1)])}.is_true());
}}}}}
#[test]
fn inherit_field_from_parent() {
super::initialize();
{
// (let* ((t (make-table))) (begin (set-field! t (quote x) 1) (let ((s (clone t))) (begin (assert (= (get-field s (quote x)) 1))))))
{
// (let ((t (make-table))) (begin (begin (set-field! t (quote x) 1) (let ((s (clone t))) (begin (assert (= (get-field s (quote x)) 1)))))))
{let t = {
// (make-table)
Scm::func(make_minus_table).invoke(&[])};{{
// (set-field! t (quote x) 1)
Scm::func(set_minus_field_i).invoke(&[t.clone(),Scm::symbol("x"),Scm::from(1)])};{
// (let ((s (clone t))) (begin (assert (= (get-field s (quote x)) 1))))
{let s = {
// (clone t)
Scm::func(clone).invoke(&[t.clone()])};assert!({
// (= (get-field s (quote x)) 1)
imports::_e_.with(|value| value.get()).invoke(&[{
// (get-field s (quote x))
Scm::func(get_minus_field).invoke(&[s.clone(),Scm::symbol("x")])},Scm::from(1)])}.is_true());
}}}}}}}
#[test]
fn setting_child_field_does_not_affect_parent() {
super::initialize();
{
// (let* ((t (make-table))) (begin (set-field! t (quote x) 1) (let ((s (clone t))) (begin (set-field! s (quote x) 2) (begin (assert (= (get-field t (quote x)) 1)))))))
{
// (let ((t (make-table))) (begin (begin (set-field! t (quote x) 1) (let ((s (clone t))) (begin (set-field! s (quote x) 2) (begin (assert (= (get-field t (quote x)) 1))))))))
{let t = {
// (make-table)
Scm::func(make_minus_table).invoke(&[])};{{
// (set-field! t (quote x) 1)
Scm::func(set_minus_field_i).invoke(&[t.clone(),Scm::symbol("x"),Scm::from(1)])};{
// (let ((s (clone t))) (begin (set-field! s (quote x) 2) (begin (assert (= (get-field t (quote x)) 1)))))
{let s = {
// (clone t)
Scm::func(clone).invoke(&[t.clone()])};{{
// (set-field! s (quote x) 2)
Scm::func(set_minus_field_i).invoke(&[s.clone(),Scm::symbol("x"),Scm::from(2)])};assert!({
// (= (get-field t (quote x)) 1)
imports::_e_.with(|value| value.get()).invoke(&[{
// (get-field t (quote x))
Scm::func(get_minus_field).invoke(&[t.clone(),Scm::symbol("x")])},Scm::from(1)])}.is_true());
}}}}}}}}
#[test]
fn call_unary_method() {
super::initialize();
{
// (let* ((t (let ((t (make-table))) (set-field! t (quote count) 0) (set-field! t (quote inc) (lambda (self) (set-field! self (quote count) (+ 1 (get-field self (quote count)))))) t))) (begin (call-method t (quote inc)) (begin (assert (= (get-field t (quote count)) 1)))))
{
// (let ((t (let ((t (make-table))) (set-field! t (quote count) 0) (set-field! t (quote inc) (lambda (self) (set-field! self (quote count) (+ 1 (get-field self (quote count)))))) t))) (begin (begin (call-method t (quote inc)) (begin (assert (= (get-field t (quote count)) 1))))))
{let t = {
// (let ((t (make-table))) (set-field! t (quote count) 0) (set-field! t (quote inc) (lambda (self) (set-field! self (quote count) (+ 1 (get-field self (quote count)))))) t)
{let t = {
// (make-table)
Scm::func(make_minus_table).invoke(&[])};{{
// (set-field! t (quote count) 0)
Scm::func(set_minus_field_i).invoke(&[t.clone(),Scm::symbol("count"),Scm::from(0)])};{
// (set-field! t (quote inc) (lambda (self) (set-field! self (quote count) (+ 1 (get-field self (quote count))))))
Scm::func(set_minus_field_i).invoke(&[t.clone(),Scm::symbol("inc"),{// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let self_ = args[0].clone();{
// (set-field! self (quote count) (+ 1 (get-field self (quote count))))
Scm::func(set_minus_field_i).invoke(&[self_.clone(),Scm::symbol("count"),{
// (+ 1 (get-field self (quote count)))
imports::_plus_.with(|value| value.get()).invoke(&[Scm::from(1),{
// (get-field self (quote count))
Scm::func(get_minus_field).invoke(&[self_.clone(),Scm::symbol("count")])}])}])}})}])};t.clone()}}};{{
// (call-method t (quote inc))
Scm::func(call_minus_method).invoke(&[t.clone(),Scm::symbol("inc")])};assert!({
// (= (get-field t (quote count)) 1)
imports::_e_.with(|value| value.get()).invoke(&[{
// (get-field t (quote count))
Scm::func(get_minus_field).invoke(&[t.clone(),Scm::symbol("count")])},Scm::from(1)])}.is_true());
}}}}}
#[test]
fn call_binary_method() {
super::initialize();
{
// (let* ((t (let ((t (make-table))) (set-field! t (quote value) 1) (set-field! t (quote add) (lambda (self other) (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value)))))) t))) (begin (call-method t (quote add) t) (begin (assert (= (get-field t (quote value)) 2)))))
{
// (let ((t (let ((t (make-table))) (set-field! t (quote value) 1) (set-field! t (quote add) (lambda (self other) (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value)))))) t))) (begin (begin (call-method t (quote add) t) (begin (assert (= (get-field t (quote value)) 2))))))
{let t = {
// (let ((t (make-table))) (set-field! t (quote value) 1) (set-field! t (quote add) (lambda (self other) (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value)))))) t)
{let t = {
// (make-table)
Scm::func(make_minus_table).invoke(&[])};{{
// (set-field! t (quote value) 1)
Scm::func(set_minus_field_i).invoke(&[t.clone(),Scm::symbol("value"),Scm::from(1)])};{
// (set-field! t (quote add) (lambda (self other) (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value))))))
Scm::func(set_minus_field_i).invoke(&[t.clone(),Scm::symbol("add"),{// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let self_ = args[0].clone();let other = args[1].clone();{
// (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value))))
Scm::func(set_minus_field_i).invoke(&[self_.clone(),Scm::symbol("value"),{
// (+ (get-field self (quote value)) (get-field other (quote value)))
imports::_plus_.with(|value| value.get()).invoke(&[{
// (get-field self (quote value))
Scm::func(get_minus_field).invoke(&[self_.clone(),Scm::symbol("value")])},{
// (get-field other (quote value))
Scm::func(get_minus_field).invoke(&[other.clone(),Scm::symbol("value")])}])}])}})}])};t.clone()}}};{{
// (call-method t (quote add) t)
Scm::func(call_minus_method).invoke(&[t.clone(),Scm::symbol("add"),t.clone()])};assert!({
// (= (get-field t (quote value)) 2)
imports::_e_.with(|value| value.get()).invoke(&[{
// (get-field t (quote value))
Scm::func(get_minus_field).invoke(&[t.clone(),Scm::symbol("value")])},Scm::from(2)])}.is_true());
}}}}}
#[test]
fn big_example() {
super::initialize();
{
// (let* ((goblin (let ((goblin (make-table))) (set-field! goblin (quote health) 30) (set-field! goblin (quote armor) 10) (set-field! goblin (quote alive?) (lambda (self) (> (get-field self (quote health)) 0))) (set-field! goblin (quote take-damage!) (lambda (self amount) (if (> amount (get-field self (quote armor))) (set-field! self (quote health) (- (get-field self (quote health)) (- amount (get-field self (quote armor)))))))) (set-field! goblin (quote spawn) (lambda (self) (clone self))) goblin)) (goblin-wizard (let ((goblin-wizard (clone goblin))) (set-field! goblin-wizard (quote health) 20) (set-field! goblin-wizard (quote armor) 0) goblin-wizard))) (let ((krog (call-method goblin (quote spawn)))) (let ((kold (call-method goblin (quote spawn)))) (let ((vard (call-method goblin (quote spawn)))) (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?))))))))))))))
{
// (let ((goblin (let ((goblin (make-table))) (set-field! goblin (quote health) 30) (set-field! goblin (quote armor) 10) (set-field! goblin (quote alive?) (lambda (self) (> (get-field self (quote health)) 0))) (set-field! goblin (quote take-damage!) (lambda (self amount) (if (> amount (get-field self (quote armor))) (set-field! self (quote health) (- (get-field self (quote health)) (- amount (get-field self (quote armor)))))))) (set-field! goblin (quote spawn) (lambda (self) (clone self))) goblin))) (let ((goblin-wizard (let ((goblin-wizard (clone goblin))) (set-field! goblin-wizard (quote health) 20) (set-field! goblin-wizard (quote armor) 0) goblin-wizard))) (begin (let ((krog (call-method goblin (quote spawn)))) (let ((kold (call-method goblin (quote spawn)))) (let ((vard (call-method goblin (quote spawn)))) (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?))))))))))))))))
{let goblin = {
// (let ((goblin (make-table))) (set-field! goblin (quote health) 30) (set-field! goblin (quote armor) 10) (set-field! goblin (quote alive?) (lambda (self) (> (get-field self (quote health)) 0))) (set-field! goblin (quote take-damage!) (lambda (self amount) (if (> amount (get-field self (quote armor))) (set-field! self (quote health) (- (get-field self (quote health)) (- amount (get-field self (quote armor)))))))) (set-field! goblin (quote spawn) (lambda (self) (clone self))) goblin)
{let goblin = {
// (make-table)
Scm::func(make_minus_table).invoke(&[])};{{
// (set-field! goblin (quote health) 30)
Scm::func(set_minus_field_i).invoke(&[goblin.clone(),Scm::symbol("health"),Scm::from(30)])};{
// (set-field! goblin (quote armor) 10)
Scm::func(set_minus_field_i).invoke(&[goblin.clone(),Scm::symbol("armor"),Scm::from(10)])};{
// (set-field! goblin (quote alive?) (lambda (self) (> (get-field self (quote health)) 0)))
Scm::func(set_minus_field_i).invoke(&[goblin.clone(),Scm::symbol("alive?"),{// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let self_ = args[0].clone();{
// (> (get-field self (quote health)) 0)
imports::_g_.with(|value| value.get()).invoke(&[{
// (get-field self (quote health))
Scm::func(get_minus_field).invoke(&[self_.clone(),Scm::symbol("health")])},Scm::from(0)])}})}])};{
// (set-field! goblin (quote take-damage!) (lambda (self amount) (if (> amount (get-field self (quote armor))) (set-field! self (quote health) (- (get-field self (quote health)) (- amount (get-field self (quote armor))))))))
Scm::func(set_minus_field_i).invoke(&[goblin.clone(),Scm::symbol("take-damage!"),{// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let self_ = args[0].clone();let amount = args[1].clone();if ({
// (> amount (get-field self (quote armor)))
imports::_g_.with(|value| value.get()).invoke(&[amount.clone(),{
// (get-field self (quote armor))
Scm::func(get_minus_field).invoke(&[self_.clone(),Scm::symbol("armor")])}])}).is_true() {{
// (set-field! self (quote health) (- (get-field self (quote health)) (- amount (get-field self (quote armor)))))
Scm::func(set_minus_field_i).invoke(&[self_.clone(),Scm::symbol("health"),{
// (- (get-field self (quote health)) (- amount (get-field self (quote armor))))
imports::_minus_.with(|value| value.get()).invoke(&[{
// (get-field self (quote health))
Scm::func(get_minus_field).invoke(&[self_.clone(),Scm::symbol("health")])},{
// (- amount (get-field self (quote armor)))
imports::_minus_.with(|value| value.get()).invoke(&[amount.clone(),{
// (get-field self (quote armor))
Scm::func(get_minus_field).invoke(&[self_.clone(),Scm::symbol("armor")])}])}])}])}} else {Scm::symbol("*UNSPECIFIED*")}})}])};{
// (set-field! goblin (quote spawn) (lambda (self) (clone self)))
Scm::func(set_minus_field_i).invoke(&[goblin.clone(),Scm::symbol("spawn"),{// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let self_ = args[0].clone();{
// (clone self)
Scm::func(clone).invoke(&[self_.clone()])}})}])};goblin.clone()}}};
// (let ((goblin-wizard (let ((goblin-wizard (clone goblin))) (set-field! goblin-wizard (quote health) 20) (set-field! goblin-wizard (quote armor) 0) goblin-wizard))) (begin (let ((krog (call-method goblin (quote spawn)))) (let ((kold (call-method goblin (quote spawn)))) (let ((vard (call-method goblin (quote spawn)))) (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?)))))))))))))))
let goblin_minus_wizard = {
// (let ((goblin-wizard (clone goblin))) (set-field! goblin-wizard (quote health) 20) (set-field! goblin-wizard (quote armor) 0) goblin-wizard)
{let goblin_minus_wizard = {
// (clone goblin)
Scm::func(clone).invoke(&[goblin.clone()])};{{
// (set-field! goblin-wizard (quote health) 20)
Scm::func(set_minus_field_i).invoke(&[goblin_minus_wizard.clone(),Scm::symbol("health"),Scm::from(20)])};{
// (set-field! goblin-wizard (quote armor) 0)
Scm::func(set_minus_field_i).invoke(&[goblin_minus_wizard.clone(),Scm::symbol("armor"),Scm::from(0)])};goblin_minus_wizard.clone()}}};
// (let ((krog (call-method goblin (quote spawn)))) (let ((kold (call-method goblin (quote spawn)))) (let ((vard (call-method goblin (quote spawn)))) (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?)))))))))))))
let krog = {
// (call-method goblin (quote spawn))
Scm::func(call_minus_method).invoke(&[goblin.clone(),Scm::symbol("spawn")])};
// (let ((kold (call-method goblin (quote spawn)))) (let ((vard (call-method goblin (quote spawn)))) (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?))))))))))))
let kold = {
// (call-method goblin (quote spawn))
Scm::func(call_minus_method).invoke(&[goblin.clone(),Scm::symbol("spawn")])};
// (let ((vard (call-method goblin (quote spawn)))) (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?)))))))))))
let vard = {
// (call-method goblin (quote spawn))
Scm::func(call_minus_method).invoke(&[goblin.clone(),Scm::symbol("spawn")])};
// (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?))))))))))
let dega = {
// (call-method goblin-wizard (quote spawn))
Scm::func(call_minus_method).invoke(&[goblin_minus_wizard.clone(),Scm::symbol("spawn")])};{{
// (call-method krog (quote take-damage!) 15)
Scm::func(call_minus_method).invoke(&[krog.clone(),Scm::symbol("take-damage!"),Scm::from(15)])};{
// (call-method kold (quote take-damage!) 30)
Scm::func(call_minus_method).invoke(&[kold.clone(),Scm::symbol("take-damage!"),Scm::from(30)])};{
// (call-method vard (quote take-damage!) 45)
Scm::func(call_minus_method).invoke(&[vard.clone(),Scm::symbol("take-damage!"),Scm::from(45)])};{
// (call-method dega (quote take-damage!) 20)
Scm::func(call_minus_method).invoke(&[dega.clone(),Scm::symbol("take-damage!"),Scm::from(20)])};assert!({
// (call-method krog (quote alive?))
Scm::func(call_minus_method).invoke(&[krog.clone(),Scm::symbol("alive?")])}.is_true());
;assert!({
// (call-method kold (quote alive?))
Scm::func(call_minus_method).invoke(&[kold.clone(),Scm::symbol("alive?")])}.is_true());
;assert!({
// (not (call-method vard (quote alive?)))
imports::not.with(|value| value.get()).invoke(&[{
// (call-method vard (quote alive?))
Scm::func(call_minus_method).invoke(&[vard.clone(),Scm::symbol("alive?")])}])}.is_true());
;assert!({
// (not (call-method dega (quote alive?)))
imports::not.with(|value| value.get()).invoke(&[{
// (call-method dega (quote alive?))
Scm::func(call_minus_method).invoke(&[dega.clone(),Scm::symbol("alive?")])}])}.is_true());
}}}}}
#[test]
fn ancestor_of_untable() {
super::initialize();
{
// (let* ((t0 (make-table)) (obj (quote not-a-table))) (begin (assert (not (ancestor? obj t0)))))
{
// (let ((t0 (make-table))) (let ((obj (quote not-a-table))) (begin (begin (assert (not (ancestor? obj t0)))))))
{let t0 = {
// (make-table)
Scm::func(make_minus_table).invoke(&[])};
// (let ((obj (quote not-a-table))) (begin (begin (assert (not (ancestor? obj t0))))))
let obj = Scm::symbol("not-a-table");assert!({
// (not (ancestor? obj t0))
imports::not.with(|value| value.get()).invoke(&[{
// (ancestor? obj t0)
Scm::func(ancestor_p).invoke(&[obj.clone(),t0.clone()])}])}.is_true());
}}}}
#[test]
fn ancestor_of_unrelated_table() {
super::initialize();
{
// (let* ((t0 (make-table)) (t1 (make-table))) (begin (assert (not (ancestor? t1 t0)))))
{
// (let ((t0 (make-table))) (let ((t1 (make-table))) (begin (begin (assert (not (ancestor? t1 t0)))))))
{let t0 = {
// (make-table)
Scm::func(make_minus_table).invoke(&[])};
// (let ((t1 (make-table))) (begin (begin (assert (not (ancestor? t1 t0))))))
let t1 = {
// (make-table)
Scm::func(make_minus_table).invoke(&[])};assert!({
// (not (ancestor? t1 t0))
imports::not.with(|value| value.get()).invoke(&[{
// (ancestor? t1 t0)
Scm::func(ancestor_p).invoke(&[t1.clone(),t0.clone()])}])}.is_true());
}}}}
#[test]
fn ancestor_of_cloned_table() {
super::initialize();
{
// (let* ((t0 (make-table)) (t1 (clone t0))) (begin (assert (ancestor? t1 t0)) (assert (not (ancestor? t0 t1)))))
{
// (let ((t0 (make-table))) (let ((t1 (clone t0))) (begin (begin (assert (ancestor? t1 t0)) (assert (not (ancestor? t0 t1)))))))
{let t0 = {
// (make-table)
Scm::func(make_minus_table).invoke(&[])};
// (let ((t1 (clone t0))) (begin (begin (assert (ancestor? t1 t0)) (assert (not (ancestor? t0 t1))))))
let t1 = {
// (clone t0)
Scm::func(clone).invoke(&[t0.clone()])};{assert!({
// (ancestor? t1 t0)
Scm::func(ancestor_p).invoke(&[t1.clone(),t0.clone()])}.is_true());
;assert!({
// (not (ancestor? t0 t1))
imports::not.with(|value| value.get()).invoke(&[{
// (ancestor? t0 t1)
Scm::func(ancestor_p).invoke(&[t0.clone(),t1.clone()])}])}.is_true());
}}}}}
#[test]
fn ancestor_across_generations() {
super::initialize();
{
// (let* ((t0 (make-table)) (t1 (clone t0)) (t2 (clone t1)) (t3 (clone t2))) (begin (assert (ancestor? t3 t0))))
{
// (let ((t0 (make-table))) (let ((t1 (clone t0))) (let ((t2 (clone t1))) (let ((t3 (clone t2))) (begin (begin (assert (ancestor? t3 t0))))))))
{let t0 = {
// (make-table)
Scm::func(make_minus_table).invoke(&[])};
// (let ((t1 (clone t0))) (let ((t2 (clone t1))) (let ((t3 (clone t2))) (begin (begin (assert (ancestor? t3 t0)))))))
let t1 = {
// (clone t0)
Scm::func(clone).invoke(&[t0.clone()])};
// (let ((t2 (clone t1))) (let ((t3 (clone t2))) (begin (begin (assert (ancestor? t3 t0))))))
let t2 = {
// (clone t1)
Scm::func(clone).invoke(&[t1.clone()])};
// (let ((t3 (clone t2))) (begin (begin (assert (ancestor? t3 t0)))))
let t3 = {
// (clone t2)
Scm::func(clone).invoke(&[t2.clone()])};assert!({
// (ancestor? t3 t0)
Scm::func(ancestor_p).invoke(&[t3.clone(),t0.clone()])}.is_true());
}}}}
}
}.into()
}
pub fn set_minus_field_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let table = args[0].clone();
        let key = args[1].clone();
        let value = args[2].clone();
        {
            // (let ((entry (assq key (fields table)))) (if entry (set-cdr! entry value) (set-fields! table (cons (cons key value) (fields table)))))
            {
                let entry = {
                    // (assq key (fields table))
                    imports::assq
                        .with(|value| value.get())
                        .invoke(&[key.clone(), {
                            // (fields table)
                            Scm::func(fields).invoke(&[table.clone()])
                        }])
                };
                if (entry.clone()).is_true() {
                    {
                        // (set-cdr! entry value)
                        imports::set_minus_cdr_i
                            .with(|value| value.get())
                            .invoke(&[entry.clone(), value.clone()])
                    }
                } else {
                    {
                        // (set-fields! table (cons (cons key value) (fields table)))
                        Scm::func(set_minus_fields_i).invoke(&[table.clone(), {
                            // (cons (cons key value) (fields table))
                            imports::cons.with(|value| value.get()).invoke(&[
                                {
                                    // (cons key value)
                                    imports::cons
                                        .with(|value| value.get())
                                        .invoke(&[key.clone(), value.clone()])
                                },
                                {
                                    // (fields table)
                                    Scm::func(fields).invoke(&[table.clone()])
                                },
                            ])
                        }])
                    }
                }
            }
        }
    }
    .into()
}
pub fn set_minus_fields_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let table = args[0].clone();
        let fields = args[1].clone();
        {
            // (set-cdr! (cdr table) fields)
            imports::set_minus_cdr_i.with(|value| value.get()).invoke(&[
                {
                    // (cdr table)
                    imports::cdr
                        .with(|value| value.get())
                        .invoke(&[table.clone()])
                },
                fields.clone(),
            ])
        }
    }
    .into()
}
pub fn set_minus_parent_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let table = args[0].clone();
        let parent = args[1].clone();
        {
            // (set-car! (cdr table) parent)
            imports::set_minus_car_i.with(|value| value.get()).invoke(&[
                {
                    // (cdr table)
                    imports::cdr
                        .with(|value| value.get())
                        .invoke(&[table.clone()])
                },
                parent.clone(),
            ])
        }
    }
    .into()
}
pub fn table_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj = args[0].clone();
        {
            // (and (pair? obj) (eq? (car obj) TABLE-ID))
            if ({
                // (pair? obj)
                imports::pair_p
                    .with(|value| value.get())
                    .invoke(&[obj.clone()])
            })
            .is_true()
            {
                {
                    // (eq? (car obj) TABLE-ID)
                    imports::eq_p.with(|value| value.get()).invoke(&[
                        {
                            // (car obj)
                            imports::car
                                .with(|value| value.get())
                                .invoke(&[obj.clone()])
                        },
                        TABLE_minus_ID.with(|value| value.get()),
                    ])
                }
            } else {
                Scm::False
            }
        }
    }
    .into()
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    {
        {
            (/*NOP*/);
            {
                // (define TABLE-ID (cons (quote <table>) (quote ())))
                TABLE_minus_ID.with(|value| {
                    value.set({
                        // (cons (quote <table>) (quote ()))
                        imports::cons
                            .with(|value| value.get())
                            .invoke(&[Scm::symbol("<table>"), Scm::Nil])
                    })
                })
            };
            {
                // (define (table? obj) ...)
                (/*NOP*/)
            };
            {
                // (define (make-table) ...)
                (/*NOP*/)
            };
            {
                // (define (clone table) ...)
                (/*NOP*/)
            };
            {
                // (define (parent table) ...)
                (/*NOP*/)
            };
            {
                // (define (set-parent! table parent) ...)
                (/*NOP*/)
            };
            {
                // (define (fields table) ...)
                (/*NOP*/)
            };
            {
                // (define (set-fields! table fields) ...)
                (/*NOP*/)
            };
            {
                // (define (get-field table key) ...)
                (/*NOP*/)
            };
            {
                // (define (set-field! table key value) ...)
                (/*NOP*/)
            };
            {
                // (define (call-method table key . args) ...)
                (/*NOP*/)
            };
            {
                // (define (ancestor? obj ancestor) ...)
                (/*NOP*/)
            };
            {
                // (define (replace-table! table source) ...)
                (/*NOP*/)
            }
        };
        {
            // (define (run-tests) ...)
            (/*NOP*/)
        }
    };
}
