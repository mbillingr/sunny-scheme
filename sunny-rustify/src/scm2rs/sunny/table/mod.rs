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
        let obj__111 = args[0].clone();
        let ancestor__112 = args[1].clone();
        {
            // (and (table? obj) (if (eq? (parent obj) ancestor) #t (ancestor? (parent obj) ancestor)))
            if ({
                // (table? obj)
                table_p(&[obj__111.clone()])
            })
            .is_true()
            {
                if ({
                    // (eq? (parent obj) ancestor)
                    imports::eq_p(&[
                        {
                            // (parent obj)
                            parent(&[obj__111.clone()])
                        },
                        ancestor__112.clone(),
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
                                parent(&[obj__111.clone()])
                            },
                            ancestor__112.clone(),
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
        let table__108 = args[0].clone();
        let key__109 = args[1].clone();
        let args__110 = Scm::list(&args[2..]);
        {
            // (apply (get-field table key) table args)
            imports::apply(&[
                {
                    // (get-field table key)
                    get_minus_field(&[table__108.clone(), key__109.clone()])
                },
                table__108.clone(),
                args__110.clone(),
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
        let table__94 = args[0].clone();
        {
            // (list TABLE-ID table)
            imports::list(&[TABLE_minus_ID.with(|value| value.get()), table__94.clone()])
        }
    }
    .into()
}
pub fn fields(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let table__98 = args[0].clone();
        {
            // (cddr table)
            imports::cddr(&[table__98.clone()])
        }
    }
    .into()
}
pub fn get_minus_field(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let table__102 = args[0].clone();
        let key__101 = args[1].clone();
        {
            // (let ((entry (assq key (fields table)))) (cond (entry (cdr entry)) ((parent table) (get-field (parent table) key)) (else #f)))
            {
                let entry__103 = {
                    // (assq key (fields table))
                    imports::assq(&[key__101.clone(), {
                        // (fields table)
                        fields(&[table__102.clone()])
                    }])
                };
                {
                    // (cond ...)
                    if (entry__103.clone()).is_true() {
                        {
                            // (cdr entry)
                            imports::cdr(&[entry__103.clone()])
                        }
                    } else if ({
                        // (parent table)
                        parent(&[table__102.clone()])
                    })
                    .is_true()
                    {
                        {
                            // (get-field (parent table) key)
                            Scm::func(get_minus_field).invoke(&[
                                {
                                    // (parent table)
                                    parent(&[table__102.clone()])
                                },
                                key__101.clone(),
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
        let table__95 = args[0].clone();
        {
            // (cadr table)
            imports::cadr(&[table__95.clone()])
        }
    }
    .into()
}
pub fn replace_minus_table_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let table__113 = args[0].clone();
        let source__114 = args[1].clone();
        {
            {
                // (set-parent! table (parent source))
                set_minus_parent_i(&[table__113.clone(), {
                    // (parent source)
                    parent(&[source__114.clone()])
                }])
            };
            {
                // (set-fields! table (fields source))
                set_minus_fields_i(&[table__113.clone(), {
                    // (fields source)
                    fields(&[source__114.clone()])
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
        let table__105 = args[0].clone();
        let key__104 = args[1].clone();
        let value__107 = args[2].clone();
        {
            // (let ((entry (assq key (fields table)))) (if entry (set-cdr! entry value) (set-fields! table (cons (cons key value) (fields table)))))
            {
                let entry__106 = {
                    // (assq key (fields table))
                    imports::assq(&[key__104.clone(), {
                        // (fields table)
                        fields(&[table__105.clone()])
                    }])
                };
                if (entry__106.clone()).is_true() {
                    {
                        // (set-cdr! entry value)
                        imports::set_minus_cdr_i(&[entry__106.clone(), value__107.clone()])
                    }
                } else {
                    {
                        // (set-fields! table (cons (cons key value) (fields table)))
                        set_minus_fields_i(&[table__105.clone(), {
                            // (cons (cons key value) (fields table))
                            imports::cons(&[
                                {
                                    // (cons key value)
                                    imports::cons(&[key__104.clone(), value__107.clone()])
                                },
                                {
                                    // (fields table)
                                    fields(&[table__105.clone()])
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
        let table__99 = args[0].clone();
        let fields__100 = args[1].clone();
        {
            // (set-cdr! (cdr table) fields)
            imports::set_minus_cdr_i(&[
                {
                    // (cdr table)
                    imports::cdr(&[table__99.clone()])
                },
                fields__100.clone(),
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
        let table__96 = args[0].clone();
        let parent__97 = args[1].clone();
        {
            // (set-car! (cdr table) parent)
            imports::set_minus_car_i(&[
                {
                    // (cdr table)
                    imports::cdr(&[table__96.clone()])
                },
                parent__97.clone(),
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
        let obj__93 = args[0].clone();
        {
            // (and (pair? obj) (eq? (car obj) TABLE-ID))
            if ({
                // (pair? obj)
                imports::pair_p(&[obj__93.clone()])
            })
            .is_true()
            {
                {
                    // (eq? (car obj) TABLE-ID)
                    imports::eq_p(&[
                        {
                            // (car obj)
                            imports::car(&[obj__93.clone()])
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
                });
                Scm::anything()
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
                    let t__115 = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    assert!({
                        // (table? t)
                        table_p(&[t__115.clone()])
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
                    let t__116 = {
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
                            table_p(&[t__116.clone()])
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
                    let t__117 = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    assert!({
                        // (not (parent t))
                        imports::not(&[{
                            // (parent t)
                            parent(&[t__117.clone()])
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
                    let t__118 = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    // (let ((s (clone t))) (begin (assert (eq? (parent s) t))))
                    let s__119 = {
                        // (clone t)
                        clone(&[t__118.clone()])
                    };
                    assert!({
                        // (eq? (parent s) t)
                        imports::eq_p(&[
                            {
                                // (parent s)
                                parent(&[s__119.clone()])
                            },
                            t__118.clone(),
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
                    let t__120 = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    // (let ((f (fields t))) (begin (assert (null? f))))
                    let f__121 = {
                        // (fields t)
                        fields(&[t__120.clone()])
                    };
                    assert!({
                        // (null? f)
                        imports::null_p(&[f__121.clone()])
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
                    let t__122 = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    // (let ((value (get-field t (quote x)))) (begin (assert (not value))))
                    let value__123 = {
                        // (get-field t (quote x))
                        get_minus_field(&[t__122.clone(), Scm::symbol("x")])
                    };
                    assert!({
                        // (not value)
                        imports::not(&[value__123.clone()])
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
                    let t__124 = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    {
                        {
                            // (set-field! t (quote x) 1)
                            set_minus_field_i(&[t__124.clone(), Scm::symbol("x"), Scm::from(1)])
                        };
                        assert!({
                            // (= (get-field t (quote x)) 1)
                            imports::_e_(&[
                                {
                                    // (get-field t (quote x))
                                    get_minus_field(&[t__124.clone(), Scm::symbol("x")])
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
                    let t__125 = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    {
                        {
                            // (set-field! t (quote x) 1)
                            set_minus_field_i(&[t__125.clone(), Scm::symbol("x"), Scm::from(1)])
                        };
                        {
                            // (let ((s (clone t))) (begin (assert (= (get-field s (quote x)) 1))))
                            {
                                let s__126 = {
                                    // (clone t)
                                    clone(&[t__125.clone()])
                                };
                                assert!({
                                    // (= (get-field s (quote x)) 1)
                                    imports::_e_(&[
                                        {
                                            // (get-field s (quote x))
                                            get_minus_field(&[s__126.clone(), Scm::symbol("x")])
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
                    let t__127 = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    {
                        {
                            // (set-field! t (quote x) 1)
                            set_minus_field_i(&[t__127.clone(), Scm::symbol("x"), Scm::from(1)])
                        };
                        {
                            // (let ((s (clone t))) (begin (set-field! s (quote x) 2) (begin (assert (= (get-field t (quote x)) 1)))))
                            {
                                let s__128 = {
                                    // (clone t)
                                    clone(&[t__127.clone()])
                                };
                                {
                                    {
                                        // (set-field! s (quote x) 2)
                                        set_minus_field_i(&[
                                            s__128.clone(),
                                            Scm::symbol("x"),
                                            Scm::from(2),
                                        ])
                                    };
                                    assert!({
                                        // (= (get-field t (quote x)) 1)
                                        imports::_e_(&[
                                            {
                                                // (get-field t (quote x))
                                                get_minus_field(&[t__127.clone(), Scm::symbol("x")])
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
                    let t__131 = {
                        // (let ((t (make-table))) (set-field! t (quote count) 0) (set-field! t (quote inc) (lambda (self) (set-field! self (quote count) (+ 1 (get-field self (quote count)))))) t)
                        {
                            let t__129 = {
                                // (make-table)
                                make_minus_table(&[])
                            };
                            {
                                {
                                    // (set-field! t (quote count) 0)
                                    set_minus_field_i(&[
                                        t__129.clone(),
                                        Scm::symbol("count"),
                                        Scm::from(0),
                                    ])
                                };
                                {
                                    // (set-field! t (quote inc) (lambda (self) (set-field! self (quote count) (+ 1 (get-field self (quote count))))))
                                    set_minus_field_i(&[t__129.clone(), Scm::symbol("inc"), {
                                        // Closure
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let self__130 = args[0].clone();
                                            {
                                                // (set-field! self (quote count) (+ 1 (get-field self (quote count))))
                                                set_minus_field_i(&[
                                                    self__130.clone(),
                                                    Scm::symbol("count"),
                                                    {
                                                        // (+ 1 (get-field self (quote count)))
                                                        imports::_plus_(&[Scm::from(1), {
                                                            // (get-field self (quote count))
                                                            get_minus_field(&[
                                                                self__130.clone(),
                                                                Scm::symbol("count"),
                                                            ])
                                                        }])
                                                    },
                                                ])
                                            }
                                        })
                                    }])
                                };
                                t__129.clone()
                            }
                        }
                    };
                    {
                        {
                            // (call-method t (quote inc))
                            call_minus_method(&[t__131.clone(), Scm::symbol("inc")])
                        };
                        assert!({
                            // (= (get-field t (quote count)) 1)
                            imports::_e_(&[
                                {
                                    // (get-field t (quote count))
                                    get_minus_field(&[t__131.clone(), Scm::symbol("count")])
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
                    let t__135 = {
                        // (let ((t (make-table))) (set-field! t (quote value) 1) (set-field! t (quote add) (lambda (self other) (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value)))))) t)
                        {
                            let t__132 = {
                                // (make-table)
                                make_minus_table(&[])
                            };
                            {
                                {
                                    // (set-field! t (quote value) 1)
                                    set_minus_field_i(&[
                                        t__132.clone(),
                                        Scm::symbol("value"),
                                        Scm::from(1),
                                    ])
                                };
                                {
                                    // (set-field! t (quote add) (lambda (self other) (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value))))))
                                    set_minus_field_i(&[t__132.clone(), Scm::symbol("add"), {
                                        // Closure
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 2 {
                                                panic!("invalid arity")
                                            }
                                            let self__133 = args[0].clone();
                                            let other__134 = args[1].clone();
                                            {
                                                // (set-field! self (quote value) (+ (get-field self (quote value)) (get-field other (quote value))))
                                                set_minus_field_i(&[
                                                    self__133.clone(),
                                                    Scm::symbol("value"),
                                                    {
                                                        // (+ (get-field self (quote value)) (get-field other (quote value)))
                                                        imports::_plus_(&[
                                                            {
                                                                // (get-field self (quote value))
                                                                get_minus_field(&[
                                                                    self__133.clone(),
                                                                    Scm::symbol("value"),
                                                                ])
                                                            },
                                                            {
                                                                // (get-field other (quote value))
                                                                get_minus_field(&[
                                                                    other__134.clone(),
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
                                t__132.clone()
                            }
                        }
                    };
                    {
                        {
                            // (call-method t (quote add) t)
                            call_minus_method(&[t__135.clone(), Scm::symbol("add"), t__135.clone()])
                        };
                        assert!({
                            // (= (get-field t (quote value)) 2)
                            imports::_e_(&[
                                {
                                    // (get-field t (quote value))
                                    get_minus_field(&[t__135.clone(), Scm::symbol("value")])
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
                    let goblin__141 = {
                        // (let ((goblin (make-table))) (set-field! goblin (quote health) 30) (set-field! goblin (quote armor) 10) (set-field! goblin (quote alive?) (lambda (self) (> (get-field self (quote health)) 0))) (set-field! goblin (quote take-damage!) (lambda (self amount) (if (> amount (get-field self (quote armor))) (set-field! self (quote health) (- (get-field self (quote health)) (- amount (get-field self (quote armor)))))))) (set-field! goblin (quote spawn) (lambda (self) (clone self))) goblin)
                        {
                            let goblin__136 = {
                                // (make-table)
                                make_minus_table(&[])
                            };
                            {
                                {
                                    // (set-field! goblin (quote health) 30)
                                    set_minus_field_i(&[
                                        goblin__136.clone(),
                                        Scm::symbol("health"),
                                        Scm::from(30),
                                    ])
                                };
                                {
                                    // (set-field! goblin (quote armor) 10)
                                    set_minus_field_i(&[
                                        goblin__136.clone(),
                                        Scm::symbol("armor"),
                                        Scm::from(10),
                                    ])
                                };
                                {
                                    // (set-field! goblin (quote alive?) (lambda (self) (> (get-field self (quote health)) 0)))
                                    set_minus_field_i(&[
                                        goblin__136.clone(),
                                        Scm::symbol("alive?"),
                                        {
                                            // Closure
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 1 {
                                                    panic!("invalid arity")
                                                }
                                                let self__137 = args[0].clone();
                                                {
                                                    // (> (get-field self (quote health)) 0)
                                                    imports::_g_(&[
                                                        {
                                                            // (get-field self (quote health))
                                                            get_minus_field(&[
                                                                self__137.clone(),
                                                                Scm::symbol("health"),
                                                            ])
                                                        },
                                                        Scm::from(0),
                                                    ])
                                                }
                                            })
                                        },
                                    ])
                                };
                                {
                                    // (set-field! goblin (quote take-damage!) (lambda (self amount) (if (> amount (get-field self (quote armor))) (set-field! self (quote health) (- (get-field self (quote health)) (- amount (get-field self (quote armor))))))))
                                    set_minus_field_i(&[
                                        goblin__136.clone(),
                                        Scm::symbol("take-damage!"),
                                        {
                                            // Closure
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 2 {
                                                    panic!("invalid arity")
                                                }
                                                let self__139 = args[0].clone();
                                                let amount__138 = args[1].clone();
                                                if ({
                                                    // (> amount (get-field self (quote armor)))
                                                    imports::_g_(&[amount__138.clone(), {
                                                        // (get-field self (quote armor))
                                                        get_minus_field(&[
                                                            self__139.clone(),
                                                            Scm::symbol("armor"),
                                                        ])
                                                    }])
                                                })
                                                .is_true()
                                                {
                                                    {
                                                        // (set-field! self (quote health) (- (get-field self (quote health)) (- amount (get-field self (quote armor)))))
                                                        set_minus_field_i(&[
                                                            self__139.clone(),
                                                            Scm::symbol("health"),
                                                            {
                                                                // (- (get-field self (quote health)) (- amount (get-field self (quote armor))))
                                                                imports::_minus_(&[
                                                                    {
                                                                        // (get-field self (quote health))
                                                                        get_minus_field(&[
                                                                            self__139.clone(),
                                                                            Scm::symbol("health"),
                                                                        ])
                                                                    },
                                                                    {
                                                                        // (- amount (get-field self (quote armor)))
                                                                        imports::_minus_(&[
                                                                            amount__138.clone(),
                                                                            {
                                                                                // (get-field self (quote armor))
                                                                                get_minus_field(&[
                                                                                    self__139
                                                                                        .clone(),
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
                                    set_minus_field_i(&[
                                        goblin__136.clone(),
                                        Scm::symbol("spawn"),
                                        {
                                            // Closure
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 1 {
                                                    panic!("invalid arity")
                                                }
                                                let self__140 = args[0].clone();
                                                {
                                                    // (clone self)
                                                    clone(&[self__140.clone()])
                                                }
                                            })
                                        },
                                    ])
                                };
                                goblin__136.clone()
                            }
                        }
                    };
                    // (let ((goblin-wizard (let ((goblin-wizard (clone goblin))) (set-field! goblin-wizard (quote health) 20) (set-field! goblin-wizard (quote armor) 0) goblin-wizard))) (begin (let ((krog (call-method goblin (quote spawn)))) (let ((kold (call-method goblin (quote spawn)))) (let ((vard (call-method goblin (quote spawn)))) (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?)))))))))))))))
                    let goblin_minus_wizard__143 = {
                        // (let ((goblin-wizard (clone goblin))) (set-field! goblin-wizard (quote health) 20) (set-field! goblin-wizard (quote armor) 0) goblin-wizard)
                        {
                            let goblin_minus_wizard__142 = {
                                // (clone goblin)
                                clone(&[goblin__141.clone()])
                            };
                            {
                                {
                                    // (set-field! goblin-wizard (quote health) 20)
                                    set_minus_field_i(&[
                                        goblin_minus_wizard__142.clone(),
                                        Scm::symbol("health"),
                                        Scm::from(20),
                                    ])
                                };
                                {
                                    // (set-field! goblin-wizard (quote armor) 0)
                                    set_minus_field_i(&[
                                        goblin_minus_wizard__142.clone(),
                                        Scm::symbol("armor"),
                                        Scm::from(0),
                                    ])
                                };
                                goblin_minus_wizard__142.clone()
                            }
                        }
                    };
                    // (let ((krog (call-method goblin (quote spawn)))) (let ((kold (call-method goblin (quote spawn)))) (let ((vard (call-method goblin (quote spawn)))) (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?)))))))))))))
                    let krog__144 = {
                        // (call-method goblin (quote spawn))
                        call_minus_method(&[goblin__141.clone(), Scm::symbol("spawn")])
                    };
                    // (let ((kold (call-method goblin (quote spawn)))) (let ((vard (call-method goblin (quote spawn)))) (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?))))))))))))
                    let kold__145 = {
                        // (call-method goblin (quote spawn))
                        call_minus_method(&[goblin__141.clone(), Scm::symbol("spawn")])
                    };
                    // (let ((vard (call-method goblin (quote spawn)))) (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?)))))))))))
                    let vard__146 = {
                        // (call-method goblin (quote spawn))
                        call_minus_method(&[goblin__141.clone(), Scm::symbol("spawn")])
                    };
                    // (let ((dega (call-method goblin-wizard (quote spawn)))) (begin (call-method krog (quote take-damage!) 15) (begin (call-method kold (quote take-damage!) 30) (begin (call-method vard (quote take-damage!) 45) (begin (call-method dega (quote take-damage!) 20) (begin (assert (call-method krog (quote alive?))) (assert (call-method kold (quote alive?))) (assert (not (call-method vard (quote alive?)))) (assert (not (call-method dega (quote alive?))))))))))
                    let dega__147 = {
                        // (call-method goblin-wizard (quote spawn))
                        call_minus_method(&[goblin_minus_wizard__143.clone(), Scm::symbol("spawn")])
                    };
                    {
                        {
                            // (call-method krog (quote take-damage!) 15)
                            call_minus_method(&[
                                krog__144.clone(),
                                Scm::symbol("take-damage!"),
                                Scm::from(15),
                            ])
                        };
                        {
                            // (call-method kold (quote take-damage!) 30)
                            call_minus_method(&[
                                kold__145.clone(),
                                Scm::symbol("take-damage!"),
                                Scm::from(30),
                            ])
                        };
                        {
                            // (call-method vard (quote take-damage!) 45)
                            call_minus_method(&[
                                vard__146.clone(),
                                Scm::symbol("take-damage!"),
                                Scm::from(45),
                            ])
                        };
                        {
                            // (call-method dega (quote take-damage!) 20)
                            call_minus_method(&[
                                dega__147.clone(),
                                Scm::symbol("take-damage!"),
                                Scm::from(20),
                            ])
                        };
                        assert!({
                            // (call-method krog (quote alive?))
                            call_minus_method(&[krog__144.clone(), Scm::symbol("alive?")])
                        }
                        .is_true());
                        assert!({
                            // (call-method kold (quote alive?))
                            call_minus_method(&[kold__145.clone(), Scm::symbol("alive?")])
                        }
                        .is_true());
                        assert!({
                            // (not (call-method vard (quote alive?)))
                            imports::not(&[{
                                // (call-method vard (quote alive?))
                                call_minus_method(&[vard__146.clone(), Scm::symbol("alive?")])
                            }])
                        }
                        .is_true());
                        assert!({
                            // (not (call-method dega (quote alive?)))
                            imports::not(&[{
                                // (call-method dega (quote alive?))
                                call_minus_method(&[dega__147.clone(), Scm::symbol("alive?")])
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
                    let t0__149 = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    // (let ((obj (quote not-a-table))) (begin (begin (assert (not (ancestor? obj t0))))))
                    let obj__148 = Scm::symbol("not-a-table");
                    assert!({
                        // (not (ancestor? obj t0))
                        imports::not(&[{
                            // (ancestor? obj t0)
                            ancestor_p(&[obj__148.clone(), t0__149.clone()])
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
                    let t0__151 = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    // (let ((t1 (make-table))) (begin (begin (assert (not (ancestor? t1 t0))))))
                    let t1__150 = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    assert!({
                        // (not (ancestor? t1 t0))
                        imports::not(&[{
                            // (ancestor? t1 t0)
                            ancestor_p(&[t1__150.clone(), t0__151.clone()])
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
                    let t0__152 = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    // (let ((t1 (clone t0))) (begin (begin (assert (ancestor? t1 t0)) (assert (not (ancestor? t0 t1))))))
                    let t1__153 = {
                        // (clone t0)
                        clone(&[t0__152.clone()])
                    };
                    {
                        assert!({
                            // (ancestor? t1 t0)
                            ancestor_p(&[t1__153.clone(), t0__152.clone()])
                        }
                        .is_true());
                        assert!({
                            // (not (ancestor? t0 t1))
                            imports::not(&[{
                                // (ancestor? t0 t1)
                                ancestor_p(&[t0__152.clone(), t1__153.clone()])
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
                    let t0__154 = {
                        // (make-table)
                        make_minus_table(&[])
                    };
                    // (let ((t1 (clone t0))) (let ((t2 (clone t1))) (let ((t3 (clone t2))) (begin (begin (assert (ancestor? t3 t0)))))))
                    let t1__155 = {
                        // (clone t0)
                        clone(&[t0__154.clone()])
                    };
                    // (let ((t2 (clone t1))) (let ((t3 (clone t2))) (begin (begin (assert (ancestor? t3 t0))))))
                    let t2__156 = {
                        // (clone t1)
                        clone(&[t1__155.clone()])
                    };
                    // (let ((t3 (clone t2))) (begin (begin (assert (ancestor? t3 t0)))))
                    let t3__157 = {
                        // (clone t2)
                        clone(&[t2__156.clone()])
                    };
                    assert!({
                        // (ancestor? t3 t0)
                        ancestor_p(&[t3__157.clone(), t0__154.clone()])
                    }
                    .is_true());
                }
            }
        }
    }
}
