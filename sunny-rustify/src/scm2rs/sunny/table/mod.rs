#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
}

pub mod exports {
    pub use super::globals::clone;
    pub use super::globals::fields;
    pub use super::globals::get_minus_field;
    pub use super::globals::make_minus_table;
    pub use super::globals::parent;
    pub use super::globals::run_minus_tests;
    pub use super::globals::set_minus_field_i;
    pub use super::globals::table_p;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static run_minus_tests: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL run-tests"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static call_minus_method: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL call-method"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static set_minus_field_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL set-field!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static get_minus_field: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL get-field"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static set_minus_fields_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL set-fields!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static fields: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL fields"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static parent: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL parent"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static clone: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL clone"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_table: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-table"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static table_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL table?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static TABLE_minus_ID: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL TABLE-ID"))}
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
            // (define TABLE-ID (cons (quote <table>) (quote ())))
            globals::TABLE_minus_ID.with(|value| {
                value.set(
                    // (cons (quote <table>) (quote ()))
                    imports::cons
                        .with(|value| value.get())
                        .invoke(&[Scm::symbol("<table>"), Scm::Nil]),
                )
            });
            // (define (table? obj) (and (pair? obj) (eq? (car obj) TABLE-ID)))
            globals::table_p.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let obj = args[0].clone();
                        // (letrec () (and (pair? obj) (eq? (car obj) TABLE-ID)))
                        {
                            // (and (pair? obj) (eq? (car obj) TABLE-ID))
                            if (
                                // (pair? obj)
                                imports::pair_p
                                    .with(|value| value.get())
                                    .invoke(&[obj.clone()])
                            )
                            .is_true()
                            {
                                // (eq? (car obj) TABLE-ID)
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    // (car obj)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[obj.clone()]),
                                    globals::TABLE_minus_ID.with(|value| value.get()),
                                ])
                            } else {
                                Scm::False
                            }
                        }
                    })
                })
            });
            // (define (make-table) (list TABLE-ID #f))
            globals::make_minus_table.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 0 {
                            panic!("invalid arity")
                        }
                        // (letrec () (list TABLE-ID #f))
                        {
                            // (list TABLE-ID #f)
                            imports::list.with(|value| value.get()).invoke(&[
                                globals::TABLE_minus_ID.with(|value| value.get()),
                                Scm::False,
                            ])
                        }
                    })
                })
            });
            // (define (clone table) (list TABLE-ID table))
            globals::clone.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let table = args[0].clone();
                        // (letrec () (list TABLE-ID table))
                        {
                            // (list TABLE-ID table)
                            imports::list.with(|value| value.get()).invoke(&[
                                globals::TABLE_minus_ID.with(|value| value.get()),
                                table.clone(),
                            ])
                        }
                    })
                })
            });
            // (define (parent table) (cadr table))
            globals::parent.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let table = args[0].clone();
                        // (letrec () (cadr table))
                        {
                            // (cadr table)
                            imports::cadr
                                .with(|value| value.get())
                                .invoke(&[table.clone()])
                        }
                    })
                })
            });
            // (define (fields table) (cddr table))
            globals::fields.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let table = args[0].clone();
                        // (letrec () (cddr table))
                        {
                            // (cddr table)
                            imports::cddr
                                .with(|value| value.get())
                                .invoke(&[table.clone()])
                        }
                    })
                })
            });
            // (define (set-fields! table fields) (set-cdr! (cdr table) fields))
            globals::set_minus_fields_i.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let table = args[0].clone();
                        let fields = args[1].clone();
                        // (letrec () (set-cdr! (cdr table) fields))
                        {
                            // (set-cdr! (cdr table) fields)
                            imports::set_minus_cdr_i.with(|value| value.get()).invoke(&[
                                // (cdr table)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[table.clone()]),
                                fields.clone(),
                            ])
                        }
                    })
                })
            });
            // (define (get-field table key) (let ((value (assq key (fields table)))) (cond (value (cdr value)) ((parent table) (get-field (parent table) key)) (else #f))))
            globals::get_minus_field.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let table = args[0].clone();
                        let key = args[1].clone();
                        // (letrec () (let ((value (assq key (fields table)))) (cond (value (cdr value)) ((parent table) (get-field (parent table) key)) (else #f))))
                        {
                            // (let ((value (assq key (fields table)))) (cond (value (cdr value)) ((parent table) (get-field (parent table) key)) (else #f)))
                            {
                                let [value] = [
                                    // (assq key (fields table))
                                    imports::assq.with(|value| value.get()).invoke(&[
                                        key.clone(),
                                        // (fields table)
                                        globals::fields
                                            .with(|value| value.get())
                                            .invoke(&[table.clone()]),
                                    ]),
                                ];
                                // (cond (value (cdr value)) ((parent table) (get-field (parent table) key)) (else #f))
                                if (value.clone()).is_true() {
                                    // (cdr value)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[value.clone()])
                                } else {
                                    if (
                                        // (parent table)
                                        globals::parent
                                            .with(|value| value.get())
                                            .invoke(&[table.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (get-field (parent table) key)
                                        globals::get_minus_field.with(|value| value.get()).invoke(
                                            &[
                                                // (parent table)
                                                globals::parent
                                                    .with(|value| value.get())
                                                    .invoke(&[table.clone()]),
                                                key.clone(),
                                            ],
                                        )
                                    } else {
                                        Scm::False
                                    }
                                }
                            }
                        }
                    })
                })
            });
            // (define (set-field! table key value) (set-fields! table (cons (cons key value) (fields table))))
            globals::set_minus_field_i.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 3 {
                            panic!("invalid arity")
                        }
                        let table = args[0].clone();
                        let key = args[1].clone();
                        let value = args[2].clone();
                        // (letrec () (set-fields! table (cons (cons key value) (fields table))))
                        {
                            // (set-fields! table (cons (cons key value) (fields table)))
                            globals::set_minus_fields_i
                                .with(|value| value.get())
                                .invoke(&[
                                    table.clone(),
                                    // (cons (cons key value) (fields table))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        // (cons key value)
                                        imports::cons
                                            .with(|value| value.get())
                                            .invoke(&[key.clone(), value.clone()]),
                                        // (fields table)
                                        globals::fields
                                            .with(|value| value.get())
                                            .invoke(&[table.clone()]),
                                    ]),
                                ])
                        }
                    })
                })
            });
            // (define (call-method table key . args) (apply (get-field table key) table args))
            globals::call_minus_method.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() < 2 {
                            panic!("not enough args")
                        }
                        let table = args[0].clone();
                        let key = args[1].clone();
                        let args_ = Scm::list(&args[2..]);
                        // (letrec () (apply (get-field table key) table args))
                        {
                            // (apply (get-field table key) table args)
                            imports::apply.with(|value| value.get()).invoke(&[
                                // (get-field table key)
                                globals::get_minus_field
                                    .with(|value| value.get())
                                    .invoke(&[table.clone(), key.clone()]),
                                table.clone(),
                                args_.clone(),
                            ])
                        }
                    })
                })
            })
        };
        // (define (run-tests) (testsuite "table tests" (testcase "new table" (given (t <- (make-table))) (then (table? t))) (testcase "can't fake tables" (given (t <- (list (cons (quote <table>) (quote ()))))) (then (not (table? t)))) (testcase "empty table has no parent" (given (t <- (make-table))) (then (not (parent t)))) (testcase "cloned table has parent" (given (t <- (make-table))) (when (s <- (clone t))) (then (eq? (parent s) t))) (testcase "empty table has no fields" (given (t <- (make-table))) (when (f <- (fields t))) (then (null? f))) (testcase "access missing field" (given (t <- (make-table))) (when (value <- (get-field t (quote x)))) (then (not value))) (testcase "insert and retrieve field" (given (t <- (make-table))) (when (set-field! t (quote x) 1)) (then (= (get-field t (quote x)) 1))) (testcase "inherit field from parent" (given (t <- (make-table))) (when (set-field! t (quote x) 1) (s <- (clone t))) (then (= (get-field s (quote x)) 1))) (testcase "setting child field does not affect parent" (given (t <- (make-table))) (when (set-field! t (quote x) 1) (s <- (clone t)) (set-field! s (quote x) 2)) (then (= (get-field t (quote x)) 1))) (testcase "call unary method" (given (t <- (let ((t (make-table))) (set-field! t (quote count) 0) (set-field! t (quote inc) (lambda (self) (set-field! self (quote count) (+ 1 (get-field self (quote count)))))) t))) (when (call-method t (quote inc))) (then (= (get-field t (quote count)) 1))) (testcase "call binary method" (given (t <- (let ((t (make-table))) (set-field! t (quote value) 1) (set-field! t (quote add) (lambda (self other) (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value)))))) t))) (when (call-method t (quote add) t)) (then (= (get-field t (quote value)) 2)))))
        globals::run_minus_tests.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 0 {
                        panic!("invalid arity")
                    }
                    // (letrec () (testsuite "table tests" (testcase "new table" (given (t <- (make-table))) (then (table? t))) (testcase "can't fake tables" (given (t <- (list (cons (quote <table>) (quote ()))))) (then (not (table? t)))) (testcase "empty table has no parent" (given (t <- (make-table))) (then (not (parent t)))) (testcase "cloned table has parent" (given (t <- (make-table))) (when (s <- (clone t))) (then (eq? (parent s) t))) (testcase "empty table has no fields" (given (t <- (make-table))) (when (f <- (fields t))) (then (null? f))) (testcase "access missing field" (given (t <- (make-table))) (when (value <- (get-field t (quote x)))) (then (not value))) (testcase "insert and retrieve field" (given (t <- (make-table))) (when (set-field! t (quote x) 1)) (then (= (get-field t (quote x)) 1))) (testcase "inherit field from parent" (given (t <- (make-table))) (when (set-field! t (quote x) 1) (s <- (clone t))) (then (= (get-field s (quote x)) 1))) (testcase "setting child field does not affect parent" (given (t <- (make-table))) (when (set-field! t (quote x) 1) (s <- (clone t)) (set-field! s (quote x) 2)) (then (= (get-field t (quote x)) 1))) (testcase "call unary method" (given (t <- (let ((t (make-table))) (set-field! t (quote count) 0) (set-field! t (quote inc) (lambda (self) (set-field! self (quote count) (+ 1 (get-field self (quote count)))))) t))) (when (call-method t (quote inc))) (then (= (get-field t (quote count)) 1))) (testcase "call binary method" (given (t <- (let ((t (make-table))) (set-field! t (quote value) 1) (set-field! t (quote add) (lambda (self other) (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value)))))) t))) (when (call-method t (quote add) t)) (then (= (get-field t (quote value)) 2)))))
                    {
                        Scm::symbol("*UNSPECIFIED*")
                    }
                })
            })
        })
    };
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn new_table() {
        super::initialize();

        // (let* ((t (make-table))) (begin (assert (table? t))))
        {
            let [t] = [
                // (make-table)
                globals::make_minus_table
                    .with(|value| value.get())
                    .invoke(&[]),
            ];
            // (let* () (begin (assert (table? t))))
            assert!(
                // (table? t)
                globals::table_p
                    .with(|value| value.get())
                    .invoke(&[t.clone(),])
                    .is_true()
            );
        }
    }
    #[test]
    fn cant_fake_tables() {
        super::initialize();

        // (let* ((t (list (cons (quote <table>) (quote ()))))) (begin (assert (not (table? t)))))
        {
            let [t] = [
                // (list (cons (quote <table>) (quote ())))
                imports::list.with(|value| value.get()).invoke(&[
                    // (cons (quote <table>) (quote ()))
                    imports::cons
                        .with(|value| value.get())
                        .invoke(&[Scm::symbol("<table>"), Scm::Nil]),
                ]),
            ];
            // (let* () (begin (assert (not (table? t)))))
            assert!(
                // (not (table? t))
                imports::not
                    .with(|value| value.get())
                    .invoke(&[
                        // (table? t)
                        globals::table_p
                            .with(|value| value.get())
                            .invoke(&[t.clone(),]),
                    ])
                    .is_true()
            );
        }
    }
    #[test]
    fn empty_table_has_no_parent() {
        super::initialize();

        // (let* ((t (make-table))) (begin (assert (not (parent t)))))
        {
            let [t] = [
                // (make-table)
                globals::make_minus_table
                    .with(|value| value.get())
                    .invoke(&[]),
            ];
            // (let* () (begin (assert (not (parent t)))))
            assert!(
                // (not (parent t))
                imports::not
                    .with(|value| value.get())
                    .invoke(&[
                        // (parent t)
                        globals::parent
                            .with(|value| value.get())
                            .invoke(&[t.clone(),]),
                    ])
                    .is_true()
            );
        }
    }
    #[test]
    fn cloned_table_has_parent() {
        super::initialize();

        // (let* ((t (make-table))) (let ((s (clone t))) (begin (assert (eq? (parent s) t)))))
        {
            let [t] = [
                // (make-table)
                globals::make_minus_table
                    .with(|value| value.get())
                    .invoke(&[]),
            ];
            // (let* () (let ((s (clone t))) (begin (assert (eq? (parent s) t)))))

            // (let ((s (clone t))) (begin (assert (eq? (parent s) t))))
            {
                let [s] = [
                    // (clone t)
                    globals::clone
                        .with(|value| value.get())
                        .invoke(&[t.clone()]),
                ];
                assert!(
                    // (eq? (parent s) t)
                    imports::eq_p
                        .with(|value| value.get())
                        .invoke(&[
                            // (parent s)
                            globals::parent
                                .with(|value| value.get())
                                .invoke(&[s.clone(),]),
                            t.clone(),
                        ])
                        .is_true()
                );
            }
        }
    }
    #[test]
    fn empty_table_has_no_fields() {
        super::initialize();

        // (let* ((t (make-table))) (let ((f (fields t))) (begin (assert (null? f)))))
        {
            let [t] = [
                // (make-table)
                globals::make_minus_table
                    .with(|value| value.get())
                    .invoke(&[]),
            ];
            // (let* () (let ((f (fields t))) (begin (assert (null? f)))))

            // (let ((f (fields t))) (begin (assert (null? f))))
            {
                let [f] = [
                    // (fields t)
                    globals::fields
                        .with(|value| value.get())
                        .invoke(&[t.clone()]),
                ];
                assert!(
                    // (null? f)
                    imports::null_p
                        .with(|value| value.get())
                        .invoke(&[f.clone(),])
                        .is_true()
                );
            }
        }
    }
    #[test]
    fn access_missing_field() {
        super::initialize();

        // (let* ((t (make-table))) (let ((value (get-field t (quote x)))) (begin (assert (not value)))))
        {
            let [t] = [
                // (make-table)
                globals::make_minus_table
                    .with(|value| value.get())
                    .invoke(&[]),
            ];
            // (let* () (let ((value (get-field t (quote x)))) (begin (assert (not value)))))

            // (let ((value (get-field t (quote x)))) (begin (assert (not value))))
            {
                let [value] = [
                    // (get-field t (quote x))
                    globals::get_minus_field
                        .with(|value| value.get())
                        .invoke(&[t.clone(), Scm::symbol("x")]),
                ];
                assert!(
                    // (not value)
                    imports::not
                        .with(|value| value.get())
                        .invoke(&[value.clone(),])
                        .is_true()
                );
            }
        }
    }
    #[test]
    fn insert_and_retrieve_field() {
        super::initialize();

        // (let* ((t (make-table))) (begin (set-field! t (quote x) 1) (begin (assert (= (get-field t (quote x)) 1)))))
        {
            let [t] = [
                // (make-table)
                globals::make_minus_table
                    .with(|value| value.get())
                    .invoke(&[]),
            ];
            // (let* () (begin (set-field! t (quote x) 1) (begin (assert (= (get-field t (quote x)) 1)))))
            {
                // (set-field! t (quote x) 1)
                globals::set_minus_field_i
                    .with(|value| value.get())
                    .invoke(&[t.clone(), Scm::symbol("x"), Scm::from(1)]);
                assert!(
                    // (= (get-field t (quote x)) 1)
                    imports::_e_
                        .with(|value| value.get())
                        .invoke(&[
                            // (get-field t (quote x))
                            globals::get_minus_field
                                .with(|value| value.get())
                                .invoke(&[t.clone(), Scm::symbol("x"),]),
                            Scm::from(1),
                        ])
                        .is_true()
                );
            }
        }
    }
    #[test]
    fn inherit_field_from_parent() {
        super::initialize();

        // (let* ((t (make-table))) (begin (set-field! t (quote x) 1) (let ((s (clone t))) (begin (assert (= (get-field s (quote x)) 1))))))
        {
            let [t] = [
                // (make-table)
                globals::make_minus_table
                    .with(|value| value.get())
                    .invoke(&[]),
            ];
            // (let* () (begin (set-field! t (quote x) 1) (let ((s (clone t))) (begin (assert (= (get-field s (quote x)) 1))))))
            {
                // (set-field! t (quote x) 1)
                globals::set_minus_field_i
                    .with(|value| value.get())
                    .invoke(&[t.clone(), Scm::symbol("x"), Scm::from(1)]);
                // (let ((s (clone t))) (begin (assert (= (get-field s (quote x)) 1))))
                {
                    let [s] = [
                        // (clone t)
                        globals::clone
                            .with(|value| value.get())
                            .invoke(&[t.clone()]),
                    ];
                    assert!(
                        // (= (get-field s (quote x)) 1)
                        imports::_e_
                            .with(|value| value.get())
                            .invoke(&[
                                // (get-field s (quote x))
                                globals::get_minus_field
                                    .with(|value| value.get())
                                    .invoke(&[s.clone(), Scm::symbol("x"),]),
                                Scm::from(1),
                            ])
                            .is_true()
                    );
                }
            }
        }
    }
    #[test]
    fn setting_child_field_does_not_affect_parent() {
        super::initialize();

        // (let* ((t (make-table))) (begin (set-field! t (quote x) 1) (let ((s (clone t))) (begin (set-field! s (quote x) 2) (begin (assert (= (get-field t (quote x)) 1)))))))
        {
            let [t] = [
                // (make-table)
                globals::make_minus_table
                    .with(|value| value.get())
                    .invoke(&[]),
            ];
            // (let* () (begin (set-field! t (quote x) 1) (let ((s (clone t))) (begin (set-field! s (quote x) 2) (begin (assert (= (get-field t (quote x)) 1)))))))
            {
                // (set-field! t (quote x) 1)
                globals::set_minus_field_i
                    .with(|value| value.get())
                    .invoke(&[t.clone(), Scm::symbol("x"), Scm::from(1)]);
                // (let ((s (clone t))) (begin (set-field! s (quote x) 2) (begin (assert (= (get-field t (quote x)) 1)))))
                {
                    let [s] = [
                        // (clone t)
                        globals::clone
                            .with(|value| value.get())
                            .invoke(&[t.clone()]),
                    ];
                    {
                        // (set-field! s (quote x) 2)
                        globals::set_minus_field_i
                            .with(|value| value.get())
                            .invoke(&[s.clone(), Scm::symbol("x"), Scm::from(2)]);
                        assert!(
                            // (= (get-field t (quote x)) 1)
                            imports::_e_
                                .with(|value| value.get())
                                .invoke(&[
                                    // (get-field t (quote x))
                                    globals::get_minus_field
                                        .with(|value| value.get())
                                        .invoke(&[t.clone(), Scm::symbol("x"),]),
                                    Scm::from(1),
                                ])
                                .is_true()
                        );
                    }
                }
            }
        }
    }
    #[test]
    fn call_unary_method() {
        super::initialize();

        // (let* ((t (let ((t (make-table))) (set-field! t (quote count) 0) (set-field! t (quote inc) (lambda (self) (set-field! self (quote count) (+ 1 (get-field self (quote count)))))) t))) (begin (call-method t (quote inc)) (begin (assert (= (get-field t (quote count)) 1)))))
        {
            let [t] = [
                // (let ((t (make-table))) (set-field! t (quote count) 0) (set-field! t (quote inc) (lambda (self) (set-field! self (quote count) (+ 1 (get-field self (quote count)))))) t)
                {
                    let [t] = [
                        // (make-table)
                        globals::make_minus_table
                            .with(|value| value.get())
                            .invoke(&[]),
                    ];
                    {
                        // (set-field! t (quote count) 0)
                        globals::set_minus_field_i
                            .with(|value| value.get())
                            .invoke(&[t.clone(), Scm::symbol("count"), Scm::from(0)]);
                        // (set-field! t (quote inc) (lambda (self) (set-field! self (quote count) (+ 1 (get-field self (quote count))))))
                        globals::set_minus_field_i
                            .with(|value| value.get())
                            .invoke(&[t.clone(), Scm::symbol("inc"), {
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 1 {
                                        panic!("invalid arity")
                                    }
                                    let self_ = args[0].clone();
                                    // (letrec () (set-field! self (quote count) (+ 1 (get-field self (quote count)))))
                                    {
                                        // (set-field! self (quote count) (+ 1 (get-field self (quote count))))
                                        globals::set_minus_field_i.with(|value| value.get()).invoke(
                                            &[
                                                self_.clone(),
                                                Scm::symbol("count"),
                                                // (+ 1 (get-field self (quote count)))
                                                imports::_plus_.with(|value| value.get()).invoke(
                                                    &[
                                                        Scm::from(1),
                                                        // (get-field self (quote count))
                                                        globals::get_minus_field
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                self_.clone(),
                                                                Scm::symbol("count"),
                                                            ]),
                                                    ],
                                                ),
                                            ],
                                        )
                                    }
                                })
                            }]);
                        t.clone()
                    }
                },
            ];
            // (let* () (begin (call-method t (quote inc)) (begin (assert (= (get-field t (quote count)) 1)))))
            {
                // (call-method t (quote inc))
                globals::call_minus_method
                    .with(|value| value.get())
                    .invoke(&[t.clone(), Scm::symbol("inc")]);
                assert!(
                    // (= (get-field t (quote count)) 1)
                    imports::_e_
                        .with(|value| value.get())
                        .invoke(&[
                            // (get-field t (quote count))
                            globals::get_minus_field
                                .with(|value| value.get())
                                .invoke(&[t.clone(), Scm::symbol("count"),]),
                            Scm::from(1),
                        ])
                        .is_true()
                );
            }
        }
    }
    #[test]
    fn call_binary_method() {
        super::initialize();

        // (let* ((t (let ((t (make-table))) (set-field! t (quote value) 1) (set-field! t (quote add) (lambda (self other) (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value)))))) t))) (begin (call-method t (quote add) t) (begin (assert (= (get-field t (quote value)) 2)))))
        {
            let [t] = [
                // (let ((t (make-table))) (set-field! t (quote value) 1) (set-field! t (quote add) (lambda (self other) (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value)))))) t)
                {
                    let [t] = [
                        // (make-table)
                        globals::make_minus_table
                            .with(|value| value.get())
                            .invoke(&[]),
                    ];
                    {
                        // (set-field! t (quote value) 1)
                        globals::set_minus_field_i
                            .with(|value| value.get())
                            .invoke(&[t.clone(), Scm::symbol("value"), Scm::from(1)]);
                        // (set-field! t (quote add) (lambda (self other) (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value))))))
                        globals::set_minus_field_i
                            .with(|value| value.get())
                            .invoke(&[t.clone(), Scm::symbol("add"), {
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 2 {
                                        panic!("invalid arity")
                                    }
                                    let self_ = args[0].clone();
                                    let other = args[1].clone();
                                    // (letrec () (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value)))))
                                    {
                                        // (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value))))
                                        globals::set_minus_field_i.with(|value| value.get()).invoke(
                                            &[
                                                self_.clone(),
                                                Scm::symbol("value"),
                                                // (+ (get-field self (quote value)) (get-field other (quote value)))
                                                imports::_plus_.with(|value| value.get()).invoke(
                                                    &[
                                                        // (get-field self (quote value))
                                                        globals::get_minus_field
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                self_.clone(),
                                                                Scm::symbol("value"),
                                                            ]),
                                                        // (get-field other (quote value))
                                                        globals::get_minus_field
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                other.clone(),
                                                                Scm::symbol("value"),
                                                            ]),
                                                    ],
                                                ),
                                            ],
                                        )
                                    }
                                })
                            }]);
                        t.clone()
                    }
                },
            ];
            // (let* () (begin (call-method t (quote add) t) (begin (assert (= (get-field t (quote value)) 2)))))
            {
                // (call-method t (quote add) t)
                globals::call_minus_method
                    .with(|value| value.get())
                    .invoke(&[t.clone(), Scm::symbol("add"), t.clone()]);
                assert!(
                    // (= (get-field t (quote value)) 2)
                    imports::_e_
                        .with(|value| value.get())
                        .invoke(&[
                            // (get-field t (quote value))
                            globals::get_minus_field
                                .with(|value| value.get())
                                .invoke(&[t.clone(), Scm::symbol("value"),]),
                            Scm::from(2),
                        ])
                        .is_true()
                );
            }
        }
    }
}
