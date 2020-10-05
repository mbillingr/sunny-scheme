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
                table_p(&[obj.clone()])
            })
            .is_true()
            {
                if ({
                    // (eq? (parent obj) ancestor)
                    imports::eq_p(&[
                        {
                            // (parent obj)
                            parent(&[obj.clone()])
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
                                parent(&[obj.clone()])
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
            imports::apply(&[
                {
                    // (get-field table key)
                    get_minus_field(&[table.clone(), key.clone()])
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
            imports::list(&[TABLE_minus_ID.with(|value| value.get()), table.clone()])
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
            imports::cddr(&[table.clone()])
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
                    imports::assq(&[key.clone(), {
                        // (fields table)
                        fields(&[table.clone()])
                    }])
                };
                {
                    // (cond ...)
                    if (entry.clone()).is_true() {
                        {
                            // (cdr entry)
                            imports::cdr(&[entry.clone()])
                        }
                    } else if ({
                        // (parent table)
                        parent(&[table.clone()])
                    })
                    .is_true()
                    {
                        {
                            // (get-field (parent table) key)
                            Scm::func(get_minus_field).invoke(&[
                                {
                                    // (parent table)
                                    parent(&[table.clone()])
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
            imports::list(&[TABLE_minus_ID.with(|value| value.get()), Scm::False])
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
            imports::cadr(&[table.clone()])
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
                set_minus_parent_i(&[table.clone(), {
                    // (parent source)
                    parent(&[source.clone()])
                }])
            };
            {
                // (set-fields! table (fields source))
                set_minus_fields_i(&[table.clone(), {
                    // (fields source)
                    fields(&[source.clone()])
                }])
            }
        }
    }
    .into()
}
pub fn run_minus_tests(args: &[Scm]) -> Scm {
    {
        if args.len() != 0 {
            panic!("invalid arity")
        }
        Scm::symbol("*UNSPECIFIED*")
    }
    .into()
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
                    imports::assq(&[key.clone(), {
                        // (fields table)
                        fields(&[table.clone()])
                    }])
                };
                if (entry.clone()).is_true() {
                    {
                        // (set-cdr! entry value)
                        imports::set_minus_cdr_i(&[entry.clone(), value.clone()])
                    }
                } else {
                    {
                        // (set-fields! table (cons (cons key value) (fields table)))
                        set_minus_fields_i(&[table.clone(), {
                            // (cons (cons key value) (fields table))
                            imports::cons(&[
                                {
                                    // (cons key value)
                                    imports::cons(&[key.clone(), value.clone()])
                                },
                                {
                                    // (fields table)
                                    fields(&[table.clone()])
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
            imports::set_minus_cdr_i(&[
                {
                    // (cdr table)
                    imports::cdr(&[table.clone()])
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
            imports::set_minus_car_i(&[
                {
                    // (cdr table)
                    imports::cdr(&[table.clone()])
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
                imports::pair_p(&[obj.clone()])
            })
            .is_true()
            {
                {
                    // (eq? (car obj) TABLE-ID)
                    imports::eq_p(&[
                        {
                            // (car obj)
                            imports::car(&[obj.clone()])
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
                        imports::cons(&[Scm::symbol("<table>"), Scm::Nil])
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
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn new_table() {
        super::initialize();
        {
            // (let* ((t (make-table))) (begin (assert (table? t))))
            {
                // (let ((t (make-table))) (begin (begin (assert (table? t)))))
                {
                    let t = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    assert!({
                        // (table? t)
                        table_p(&[t.clone()])
                    }
                    .is_true());
                }
            }
        }
    }
    #[test]
    fn cant_fake_tables() {
        super::initialize();
        {
            // (let* ((t (list (cons (quote <table>) (quote ()))))) (begin (assert (not (table? t)))))
            {
                // (let ((t (list (cons (quote <table>) (quote ()))))) (begin (begin (assert (not (table? t))))))
                {
                    let t = {
                        // (list (cons (quote <table>) (quote ())))
                        imports::list(&[{
                            // (cons (quote <table>) (quote ()))
                            imports::cons(&[Scm::symbol("<table>"), Scm::Nil])
                        }])
                    };
                    assert!({
                        // (not (table? t))
                        imports::not(&[{
                            // (table? t)
                            table_p(&[t.clone()])
                        }])
                    }
                    .is_true());
                }
            }
        }
    }
    #[test]
    fn empty_table_has_no_parent() {
        super::initialize();
        {
            // (let* ((t (make-table))) (begin (assert (not (parent t)))))
            {
                // (let ((t (make-table))) (begin (begin (assert (not (parent t))))))
                {
                    let t = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    assert!({
                        // (not (parent t))
                        imports::not(&[{
                            // (parent t)
                            parent(&[t.clone()])
                        }])
                    }
                    .is_true());
                }
            }
        }
    }
    #[test]
    fn cloned_table_has_parent() {
        super::initialize();
        {
            // (let* ((t (make-table))) (let ((s (clone t))) (begin (assert (eq? (parent s) t)))))
            {
                // (let ((t (make-table))) (begin (let ((s (clone t))) (begin (assert (eq? (parent s) t))))))
                {
                    let t = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    // (let ((s (clone t))) (begin (assert (eq? (parent s) t))))
                    let s = {
                        // (clone t)
                        clone(&[t.clone()])
                    };
                    assert!({
                        // (eq? (parent s) t)
                        imports::eq_p(&[
                            {
                                // (parent s)
                                parent(&[s.clone()])
                            },
                            t.clone(),
                        ])
                    }
                    .is_true());
                }
            }
        }
    }
    #[test]
    fn empty_table_has_no_fields() {
        super::initialize();
        {
            // (let* ((t (make-table))) (let ((f (fields t))) (begin (assert (null? f)))))
            {
                // (let ((t (make-table))) (begin (let ((f (fields t))) (begin (assert (null? f))))))
                {
                    let t = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    // (let ((f (fields t))) (begin (assert (null? f))))
                    let f = {
                        // (fields t)
                        fields(&[t.clone()])
                    };
                    assert!({
                        // (null? f)
                        imports::null_p(&[f.clone()])
                    }
                    .is_true());
                }
            }
        }
    }
    #[test]
    fn access_missing_field() {
        super::initialize();
        {
            // (let* ((t (make-table))) (let ((value (get-field t (quote x)))) (begin (assert (not value)))))
            {
                // (let ((t (make-table))) (begin (let ((value (get-field t (quote x)))) (begin (assert (not value))))))
                {
                    let t = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    // (let ((value (get-field t (quote x)))) (begin (assert (not value))))
                    let value = {
                        // (get-field t (quote x))
                        get_minus_field(&[t.clone(), Scm::symbol("x")])
                    };
                    assert!({
                        // (not value)
                        imports::not(&[value.clone()])
                    }
                    .is_true());
                }
            }
        }
    }
    #[test]
    fn insert_and_retrieve_field() {
        super::initialize();
        {
            // (let* ((t (make-table))) (begin (set-field! t (quote x) 1) (begin (assert (= (get-field t (quote x)) 1)))))
            {
                // (let ((t (make-table))) (begin (begin (set-field! t (quote x) 1) (begin (assert (= (get-field t (quote x)) 1))))))
                {
                    let t = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    {
                        {
                            // (set-field! t (quote x) 1)
                            set_minus_field_i(&[t.clone(), Scm::symbol("x"), Scm::from(1)])
                        };
                        assert!({
                            // (= (get-field t (quote x)) 1)
                            imports::_e_(&[
                                {
                                    // (get-field t (quote x))
                                    get_minus_field(&[t.clone(), Scm::symbol("x")])
                                },
                                Scm::from(1),
                            ])
                        }
                        .is_true());
                    }
                }
            }
        }
    }
    #[test]
    fn inherit_field_from_parent() {
        super::initialize();
        {
            // (let* ((t (make-table))) (begin (set-field! t (quote x) 1) (let ((s (clone t))) (begin (assert (= (get-field s (quote x)) 1))))))
            {
                // (let ((t (make-table))) (begin (begin (set-field! t (quote x) 1) (let ((s (clone t))) (begin (assert (= (get-field s (quote x)) 1)))))))
                {
                    let t = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    {
                        {
                            // (set-field! t (quote x) 1)
                            set_minus_field_i(&[t.clone(), Scm::symbol("x"), Scm::from(1)])
                        };
                        {
                            // (let ((s (clone t))) (begin (assert (= (get-field s (quote x)) 1))))
                            {
                                let s = {
                                    // (clone t)
                                    clone(&[t.clone()])
                                };
                                assert!({
                                    // (= (get-field s (quote x)) 1)
                                    imports::_e_(&[
                                        {
                                            // (get-field s (quote x))
                                            get_minus_field(&[s.clone(), Scm::symbol("x")])
                                        },
                                        Scm::from(1),
                                    ])
                                }
                                .is_true());
                            }
                        }
                    }
                }
            }
        }
    }
    #[test]
    fn setting_child_field_does_not_affect_parent() {
        super::initialize();
        {
            // (let* ((t (make-table))) (begin (set-field! t (quote x) 1) (let ((s (clone t))) (begin (set-field! s (quote x) 2) (begin (assert (= (get-field t (quote x)) 1)))))))
            {
                // (let ((t (make-table))) (begin (begin (set-field! t (quote x) 1) (let ((s (clone t))) (begin (set-field! s (quote x) 2) (begin (assert (= (get-field t (quote x)) 1))))))))
                {
                    let t = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    {
                        {
                            // (set-field! t (quote x) 1)
                            set_minus_field_i(&[t.clone(), Scm::symbol("x"), Scm::from(1)])
                        };
                        {
                            // (let ((s (clone t))) (begin (set-field! s (quote x) 2) (begin (assert (= (get-field t (quote x)) 1)))))
                            {
                                let s = {
                                    // (clone t)
                                    clone(&[t.clone()])
                                };
                                {
                                    {
                                        // (set-field! s (quote x) 2)
                                        set_minus_field_i(&[
                                            s.clone(),
                                            Scm::symbol("x"),
                                            Scm::from(2),
                                        ])
                                    };
                                    assert!({
                                        // (= (get-field t (quote x)) 1)
                                        imports::_e_(&[
                                            {
                                                // (get-field t (quote x))
                                                get_minus_field(&[t.clone(), Scm::symbol("x")])
                                            },
                                            Scm::from(1),
                                        ])
                                    }
                                    .is_true());
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    #[test]
    fn call_unary_method() {
        super::initialize();
        {
            // (let* ((t (let ((t (make-table))) (set-field! t (quote count) 0) (set-field! t (quote inc) (lambda (self) (set-field! self (quote count) (+ 1 (get-field self (quote count)))))) t))) (begin (call-method t (quote inc)) (begin (assert (= (get-field t (quote count)) 1)))))
            {
                // (let ((t (let ((t (make-table))) (set-field! t (quote count) 0) (set-field! t (quote inc) (lambda (self) (set-field! self (quote count) (+ 1 (get-field self (quote count)))))) t))) (begin (begin (call-method t (quote inc)) (begin (assert (= (get-field t (quote count)) 1))))))
                {
                    let t = {
                        // (let ((t (make-table))) (set-field! t (quote count) 0) (set-field! t (quote inc) (lambda (self) (set-field! self (quote count) (+ 1 (get-field self (quote count)))))) t)
                        {
                            let t = {
                                // (make-table)
                                make_minus_table(&[])
                            };
                            {
                                {
                                    // (set-field! t (quote count) 0)
                                    set_minus_field_i(&[
                                        t.clone(),
                                        Scm::symbol("count"),
                                        Scm::from(0),
                                    ])
                                };
                                {
                                    // (set-field! t (quote inc) (lambda (self) (set-field! self (quote count) (+ 1 (get-field self (quote count))))))
                                    set_minus_field_i(&[t.clone(), Scm::symbol("inc"), {
                                        // Closure
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let self_ = args[0].clone();
                                            {
                                                // (set-field! self (quote count) (+ 1 (get-field self (quote count))))
                                                set_minus_field_i(&[
                                                    self_.clone(),
                                                    Scm::symbol("count"),
                                                    {
                                                        // (+ 1 (get-field self (quote count)))
                                                        imports::_plus_(&[Scm::from(1), {
                                                            // (get-field self (quote count))
                                                            get_minus_field(&[
                                                                self_.clone(),
                                                                Scm::symbol("count"),
                                                            ])
                                                        }])
                                                    },
                                                ])
                                            }
                                        })
                                    }])
                                };
                                t.clone()
                            }
                        }
                    };
                    {
                        {
                            // (call-method t (quote inc))
                            call_minus_method(&[t.clone(), Scm::symbol("inc")])
                        };
                        assert!({
                            // (= (get-field t (quote count)) 1)
                            imports::_e_(&[
                                {
                                    // (get-field t (quote count))
                                    get_minus_field(&[t.clone(), Scm::symbol("count")])
                                },
                                Scm::from(1),
                            ])
                        }
                        .is_true());
                    }
                }
            }
        }
    }
    #[test]
    fn call_binary_method() {
        super::initialize();
        {
            // (let* ((t (let ((t (make-table))) (set-field! t (quote value) 1) (set-field! t (quote add) (lambda (self other) (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value)))))) t))) (begin (call-method t (quote add) t) (begin (assert (= (get-field t (quote value)) 2)))))
            {
                // (let ((t (let ((t (make-table))) (set-field! t (quote value) 1) (set-field! t (quote add) (lambda (self other) (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value)))))) t))) (begin (begin (call-method t (quote add) t) (begin (assert (= (get-field t (quote value)) 2))))))
                {
                    let t = {
                        // (let ((t (make-table))) (set-field! t (quote value) 1) (set-field! t (quote add) (lambda (self other) (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value)))))) t)
                        {
                            let t = {
                                // (make-table)
                                make_minus_table(&[])
                            };
                            {
                                {
                                    // (set-field! t (quote value) 1)
                                    set_minus_field_i(&[
                                        t.clone(),
                                        Scm::symbol("value"),
                                        Scm::from(1),
                                    ])
                                };
                                {
                                    // (set-field! t (quote add) (lambda (self other) (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value))))))
                                    set_minus_field_i(&[t.clone(), Scm::symbol("add"), {
                                        // Closure
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 2 {
                                                panic!("invalid arity")
                                            }
                                            let self_ = args[0].clone();
                                            let other = args[1].clone();
                                            {
                                                // (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value))))
                                                set_minus_field_i(&[
                                                    self_.clone(),
                                                    Scm::symbol("value"),
                                                    {
                                                        // (+ (get-field self (quote value)) (get-field other (quote value)))
                                                        imports::_plus_(&[
                                                            {
                                                                // (get-field self (quote value))
                                                                get_minus_field(&[
                                                                    self_.clone(),
                                                                    Scm::symbol("value"),
                                                                ])
                                                            },
                                                            {
                                                                // (get-field other (quote value))
                                                                get_minus_field(&[
                                                                    other.clone(),
                                                                    Scm::symbol("value"),
                                                                ])
                                                            },
                                                        ])
                                                    },
                                                ])
                                            }
                                        })
                                    }])
                                };
                                t.clone()
                            }
                        }
                    };
                    {
                        {
                            // (call-method t (quote add) t)
                            call_minus_method(&[t.clone(), Scm::symbol("add"), t.clone()])
                        };
                        assert!({
                            // (= (get-field t (quote value)) 2)
                            imports::_e_(&[
                                {
                                    // (get-field t (quote value))
                                    get_minus_field(&[t.clone(), Scm::symbol("value")])
                                },
                                Scm::from(2),
                            ])
                        }
                        .is_true());
                    }
                }
            }
        }
    }
    #[test]
    fn big_example() {
        super::initialize();
        {
            // (let* ((goblin (let ((goblin (make-table))) (set-field! goblin (quote health) 30) (set-field! goblin (quote armor) 10) (set-field! goblin (quote alive?) (lambda (self) (> (get-field self (quote health)) 0))) (set-field! goblin (quote take-damage!) (lambda (self amount) (if (> amount (get-field self (quote armor))) (set-field! self (quote health) (- (get-field self (quote health)) (- amount (get-field self (quote armor)))))))) (set-field! goblin (quote spawn) (lambda (self) (clone self))) goblin)) (goblin-wizard (let ((goblin-wizard (clone goblin))) (set-field! goblin-wizard (quote health) 20) (set-field! goblin-wizard (quote armor) 0) goblin-wizard))) (let ((krog (call-method goblin (quote spawn)))) (let ((kold (call-method goblin (quote spawn)))) (let ((vard (call-method goblin (quote spawn)))) (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?))))))))))))))
            {
                // (let ((goblin (let ((goblin (make-table))) (set-field! goblin (quote health) 30) (set-field! goblin (quote armor) 10) (set-field! goblin (quote alive?) (lambda (self) (> (get-field self (quote health)) 0))) (set-field! goblin (quote take-damage!) (lambda (self amount) (if (> amount (get-field self (quote armor))) (set-field! self (quote health) (- (get-field self (quote health)) (- amount (get-field self (quote armor)))))))) (set-field! goblin (quote spawn) (lambda (self) (clone self))) goblin))) (let ((goblin-wizard (let ((goblin-wizard (clone goblin))) (set-field! goblin-wizard (quote health) 20) (set-field! goblin-wizard (quote armor) 0) goblin-wizard))) (begin (let ((krog (call-method goblin (quote spawn)))) (let ((kold (call-method goblin (quote spawn)))) (let ((vard (call-method goblin (quote spawn)))) (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?))))))))))))))))
                {
                    let goblin = {
                        // (let ((goblin (make-table))) (set-field! goblin (quote health) 30) (set-field! goblin (quote armor) 10) (set-field! goblin (quote alive?) (lambda (self) (> (get-field self (quote health)) 0))) (set-field! goblin (quote take-damage!) (lambda (self amount) (if (> amount (get-field self (quote armor))) (set-field! self (quote health) (- (get-field self (quote health)) (- amount (get-field self (quote armor)))))))) (set-field! goblin (quote spawn) (lambda (self) (clone self))) goblin)
                        {
                            let goblin = {
                                // (make-table)
                                make_minus_table(&[])
                            };
                            {
                                {
                                    // (set-field! goblin (quote health) 30)
                                    set_minus_field_i(&[
                                        goblin.clone(),
                                        Scm::symbol("health"),
                                        Scm::from(30),
                                    ])
                                };
                                {
                                    // (set-field! goblin (quote armor) 10)
                                    set_minus_field_i(&[
                                        goblin.clone(),
                                        Scm::symbol("armor"),
                                        Scm::from(10),
                                    ])
                                };
                                {
                                    // (set-field! goblin (quote alive?) (lambda (self) (> (get-field self (quote health)) 0)))
                                    set_minus_field_i(&[goblin.clone(), Scm::symbol("alive?"), {
                                        // Closure
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let self_ = args[0].clone();
                                            {
                                                // (> (get-field self (quote health)) 0)
                                                imports::_g_(&[
                                                    {
                                                        // (get-field self (quote health))
                                                        get_minus_field(&[
                                                            self_.clone(),
                                                            Scm::symbol("health"),
                                                        ])
                                                    },
                                                    Scm::from(0),
                                                ])
                                            }
                                        })
                                    }])
                                };
                                {
                                    // (set-field! goblin (quote take-damage!) (lambda (self amount) (if (> amount (get-field self (quote armor))) (set-field! self (quote health) (- (get-field self (quote health)) (- amount (get-field self (quote armor))))))))
                                    set_minus_field_i(&[
                                        goblin.clone(),
                                        Scm::symbol("take-damage!"),
                                        {
                                            // Closure
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 2 {
                                                    panic!("invalid arity")
                                                }
                                                let self_ = args[0].clone();
                                                let amount = args[1].clone();
                                                if ({
                                                    // (> amount (get-field self (quote armor)))
                                                    imports::_g_(&[amount.clone(), {
                                                        // (get-field self (quote armor))
                                                        get_minus_field(&[
                                                            self_.clone(),
                                                            Scm::symbol("armor"),
                                                        ])
                                                    }])
                                                })
                                                .is_true()
                                                {
                                                    {
                                                        // (set-field! self (quote health) (- (get-field self (quote health)) (- amount (get-field self (quote armor)))))
                                                        set_minus_field_i(&[
                                                            self_.clone(),
                                                            Scm::symbol("health"),
                                                            {
                                                                // (- (get-field self (quote health)) (- amount (get-field self (quote armor))))
                                                                imports::_minus_(&[
                                                                    {
                                                                        // (get-field self (quote health))
                                                                        get_minus_field(&[
                                                                            self_.clone(),
                                                                            Scm::symbol("health"),
                                                                        ])
                                                                    },
                                                                    {
                                                                        // (- amount (get-field self (quote armor)))
                                                                        imports::_minus_(&[
                                                                            amount.clone(),
                                                                            {
                                                                                // (get-field self (quote armor))
                                                                                get_minus_field(&[
                                                                                    self_.clone(),
                                                                                    Scm::symbol(
                                                                                        "armor",
                                                                                    ),
                                                                                ])
                                                                            },
                                                                        ])
                                                                    },
                                                                ])
                                                            },
                                                        ])
                                                    }
                                                } else {
                                                    Scm::symbol("*UNSPECIFIED*")
                                                }
                                            })
                                        },
                                    ])
                                };
                                {
                                    // (set-field! goblin (quote spawn) (lambda (self) (clone self)))
                                    set_minus_field_i(&[goblin.clone(), Scm::symbol("spawn"), {
                                        // Closure
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let self_ = args[0].clone();
                                            {
                                                // (clone self)
                                                clone(&[self_.clone()])
                                            }
                                        })
                                    }])
                                };
                                goblin.clone()
                            }
                        }
                    };
                    // (let ((goblin-wizard (let ((goblin-wizard (clone goblin))) (set-field! goblin-wizard (quote health) 20) (set-field! goblin-wizard (quote armor) 0) goblin-wizard))) (begin (let ((krog (call-method goblin (quote spawn)))) (let ((kold (call-method goblin (quote spawn)))) (let ((vard (call-method goblin (quote spawn)))) (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?)))))))))))))))
                    let goblin_minus_wizard = {
                        // (let ((goblin-wizard (clone goblin))) (set-field! goblin-wizard (quote health) 20) (set-field! goblin-wizard (quote armor) 0) goblin-wizard)
                        {
                            let goblin_minus_wizard = {
                                // (clone goblin)
                                clone(&[goblin.clone()])
                            };
                            {
                                {
                                    // (set-field! goblin-wizard (quote health) 20)
                                    set_minus_field_i(&[
                                        goblin_minus_wizard.clone(),
                                        Scm::symbol("health"),
                                        Scm::from(20),
                                    ])
                                };
                                {
                                    // (set-field! goblin-wizard (quote armor) 0)
                                    set_minus_field_i(&[
                                        goblin_minus_wizard.clone(),
                                        Scm::symbol("armor"),
                                        Scm::from(0),
                                    ])
                                };
                                goblin_minus_wizard.clone()
                            }
                        }
                    };
                    // (let ((krog (call-method goblin (quote spawn)))) (let ((kold (call-method goblin (quote spawn)))) (let ((vard (call-method goblin (quote spawn)))) (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?)))))))))))))
                    let krog = {
                        // (call-method goblin (quote spawn))
                        call_minus_method(&[goblin.clone(), Scm::symbol("spawn")])
                    };
                    // (let ((kold (call-method goblin (quote spawn)))) (let ((vard (call-method goblin (quote spawn)))) (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?))))))))))))
                    let kold = {
                        // (call-method goblin (quote spawn))
                        call_minus_method(&[goblin.clone(), Scm::symbol("spawn")])
                    };
                    // (let ((vard (call-method goblin (quote spawn)))) (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?)))))))))))
                    let vard = {
                        // (call-method goblin (quote spawn))
                        call_minus_method(&[goblin.clone(), Scm::symbol("spawn")])
                    };
                    // (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?))))))))))
                    let dega = {
                        // (call-method goblin-wizard (quote spawn))
                        call_minus_method(&[goblin_minus_wizard.clone(), Scm::symbol("spawn")])
                    };
                    {
                        {
                            // (call-method krog (quote take-damage!) 15)
                            call_minus_method(&[
                                krog.clone(),
                                Scm::symbol("take-damage!"),
                                Scm::from(15),
                            ])
                        };
                        {
                            // (call-method kold (quote take-damage!) 30)
                            call_minus_method(&[
                                kold.clone(),
                                Scm::symbol("take-damage!"),
                                Scm::from(30),
                            ])
                        };
                        {
                            // (call-method vard (quote take-damage!) 45)
                            call_minus_method(&[
                                vard.clone(),
                                Scm::symbol("take-damage!"),
                                Scm::from(45),
                            ])
                        };
                        {
                            // (call-method dega (quote take-damage!) 20)
                            call_minus_method(&[
                                dega.clone(),
                                Scm::symbol("take-damage!"),
                                Scm::from(20),
                            ])
                        };
                        assert!({
                            // (call-method krog (quote alive?))
                            call_minus_method(&[krog.clone(), Scm::symbol("alive?")])
                        }
                        .is_true());
                        assert!({
                            // (call-method kold (quote alive?))
                            call_minus_method(&[kold.clone(), Scm::symbol("alive?")])
                        }
                        .is_true());
                        assert!({
                            // (not (call-method vard (quote alive?)))
                            imports::not(&[{
                                // (call-method vard (quote alive?))
                                call_minus_method(&[vard.clone(), Scm::symbol("alive?")])
                            }])
                        }
                        .is_true());
                        assert!({
                            // (not (call-method dega (quote alive?)))
                            imports::not(&[{
                                // (call-method dega (quote alive?))
                                call_minus_method(&[dega.clone(), Scm::symbol("alive?")])
                            }])
                        }
                        .is_true());
                    }
                }
            }
        }
    }
    #[test]
    fn ancestor_of_untable() {
        super::initialize();
        {
            // (let* ((t0 (make-table)) (obj (quote not-a-table))) (begin (assert (not (ancestor? obj t0)))))
            {
                // (let ((t0 (make-table))) (let ((obj (quote not-a-table))) (begin (begin (assert (not (ancestor? obj t0)))))))
                {
                    let t0 = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    // (let ((obj (quote not-a-table))) (begin (begin (assert (not (ancestor? obj t0))))))
                    let obj = Scm::symbol("not-a-table");
                    assert!({
                        // (not (ancestor? obj t0))
                        imports::not(&[{
                            // (ancestor? obj t0)
                            ancestor_p(&[obj.clone(), t0.clone()])
                        }])
                    }
                    .is_true());
                }
            }
        }
    }
    #[test]
    fn ancestor_of_unrelated_table() {
        super::initialize();
        {
            // (let* ((t0 (make-table)) (t1 (make-table))) (begin (assert (not (ancestor? t1 t0)))))
            {
                // (let ((t0 (make-table))) (let ((t1 (make-table))) (begin (begin (assert (not (ancestor? t1 t0)))))))
                {
                    let t0 = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    // (let ((t1 (make-table))) (begin (begin (assert (not (ancestor? t1 t0))))))
                    let t1 = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    assert!({
                        // (not (ancestor? t1 t0))
                        imports::not(&[{
                            // (ancestor? t1 t0)
                            ancestor_p(&[t1.clone(), t0.clone()])
                        }])
                    }
                    .is_true());
                }
            }
        }
    }
    #[test]
    fn ancestor_of_cloned_table() {
        super::initialize();
        {
            // (let* ((t0 (make-table)) (t1 (clone t0))) (begin (assert (ancestor? t1 t0)) (assert (not (ancestor? t0 t1)))))
            {
                // (let ((t0 (make-table))) (let ((t1 (clone t0))) (begin (begin (assert (ancestor? t1 t0)) (assert (not (ancestor? t0 t1)))))))
                {
                    let t0 = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    // (let ((t1 (clone t0))) (begin (begin (assert (ancestor? t1 t0)) (assert (not (ancestor? t0 t1))))))
                    let t1 = {
                        // (clone t0)
                        clone(&[t0.clone()])
                    };
                    {
                        assert!({
                            // (ancestor? t1 t0)
                            ancestor_p(&[t1.clone(), t0.clone()])
                        }
                        .is_true());
                        assert!({
                            // (not (ancestor? t0 t1))
                            imports::not(&[{
                                // (ancestor? t0 t1)
                                ancestor_p(&[t0.clone(), t1.clone()])
                            }])
                        }
                        .is_true());
                    }
                }
            }
        }
    }
    #[test]
    fn ancestor_across_generations() {
        super::initialize();
        {
            // (let* ((t0 (make-table)) (t1 (clone t0)) (t2 (clone t1)) (t3 (clone t2))) (begin (assert (ancestor? t3 t0))))
            {
                // (let ((t0 (make-table))) (let ((t1 (clone t0))) (let ((t2 (clone t1))) (let ((t3 (clone t2))) (begin (begin (assert (ancestor? t3 t0))))))))
                {
                    let t0 = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    // (let ((t1 (clone t0))) (let ((t2 (clone t1))) (let ((t3 (clone t2))) (begin (begin (assert (ancestor? t3 t0)))))))
                    let t1 = {
                        // (clone t0)
                        clone(&[t0.clone()])
                    };
                    // (let ((t2 (clone t1))) (let ((t3 (clone t2))) (begin (begin (assert (ancestor? t3 t0))))))
                    let t2 = {
                        // (clone t1)
                        clone(&[t1.clone()])
                    };
                    // (let ((t3 (clone t2))) (begin (begin (assert (ancestor? t3 t0)))))
                    let t3 = {
                        // (clone t2)
                        clone(&[t2.clone()])
                    };
                    assert!({
                        // (ancestor? t3 t0)
                        ancestor_p(&[t3.clone(), t0.clone()])
                    }
                    .is_true());
                }
            }
        }
    }
}
