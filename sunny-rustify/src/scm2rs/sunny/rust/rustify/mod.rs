#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::sunny::utils::exports::*;
}

pub mod exports {
    pub use super::rustify_minus_identifier;
    pub use super::rustify_minus_libname;
    pub use super::rustify_minus_testname;
}

pub fn rustify_minus_identifier(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name_20 = args[0].clone();
        {
            // (letrec ((char-map (lambda (ch) (cond ((eq? ch #\_) "__") ((eq? ch #\?) "_p") ((eq? ch #\!) "_i") ((eq? ch #\<) "_l_") ((eq? ch #\>) "_g_") ((eq? ch #\=) "_e_") ((eq? ch #\-) "_minus_") ((eq? ch #\+) "_plus_") ((eq? ch #\*) "_star_") ((eq? ch #\/) "_slash_") (else (list->string (list ch)))))) (append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs))))))) (cond ((same-name? name (quote args)) "args_") ((same-name? name (quote fn)) "fn_") ((same-name? name (quote loop)) "loop_") ((same-name? name (quote let)) "let_") ((same-name? name (quote mut)) "mut_") ((same-name? name (quote ref)) "ref_") ((same-name? name (quote self)) "self_") (else (append-all (map char-map (string->list (if (symbol? name) (symbol->string name) name)))))))
            {
                // (let ((char-map (quote *uninitialized*)) (append-all (quote *uninitialized*))) (begin (set! char-map (lambda (ch) (cond ((eq? ch #\_) "__") ((eq? ch #\?) "_p") ((eq? ch #\!) "_i") ((eq? ch #\<) "_l_") ((eq? ch #\>) "_g_") ((eq? ch #\=) "_e_") ((eq? ch #\-) "_minus_") ((eq? ch #\+) "_plus_") ((eq? ch #\*) "_star_") ((eq? ch #\/) "_slash_") (else (list->string (list ch)))))) (set! append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs)))))) (cond ((same-name? name (quote args)) "args_") ((same-name? name (quote fn)) "fn_") ((same-name? name (quote loop)) "loop_") ((same-name? name (quote let)) "let_") ((same-name? name (quote mut)) "mut_") ((same-name? name (quote ref)) "ref_") ((same-name? name (quote self)) "self_") (else (append-all (map char-map (string->list (if (symbol? name) (symbol->string name) name))))))))
                {
                    let [char_minus_map_0, append_minus_all_0] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let append_minus_all_0 = append_minus_all_0.into_boxed();
                        {
                            let char_minus_map_0 = char_minus_map_0.into_boxed();
                            {
                                char_minus_map_0.set({
                                    // Closure
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let ch_0 = args[0].clone();
                                        {
                                            // (cond ...)
                                            if ({
                                                // (eq? ch #\_)
                                                imports::eq_p(&[ch_0.clone(), Scm::char('_')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("__")
                                            } else if ({
                                                // (eq? ch #\?)
                                                imports::eq_p(&[ch_0.clone(), Scm::char('?')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_p")
                                            } else if ({
                                                // (eq? ch #\!)
                                                imports::eq_p(&[ch_0.clone(), Scm::char('!')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_i")
                                            } else if ({
                                                // (eq? ch #\<)
                                                imports::eq_p(&[ch_0.clone(), Scm::char('<')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_l_")
                                            } else if ({
                                                // (eq? ch #\>)
                                                imports::eq_p(&[ch_0.clone(), Scm::char('>')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_g_")
                                            } else if ({
                                                // (eq? ch #\=)
                                                imports::eq_p(&[ch_0.clone(), Scm::char('=')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_e_")
                                            } else if ({
                                                // (eq? ch #\-)
                                                imports::eq_p(&[ch_0.clone(), Scm::char('-')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_minus_")
                                            } else if ({
                                                // (eq? ch #\+)
                                                imports::eq_p(&[ch_0.clone(), Scm::char('+')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_plus_")
                                            } else if ({
                                                // (eq? ch #\*)
                                                imports::eq_p(&[ch_0.clone(), Scm::char('*')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_star_")
                                            } else if ({
                                                // (eq? ch #\/)
                                                imports::eq_p(&[ch_0.clone(), Scm::char('/')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_slash_")
                                            } else {
                                                {
                                                    // (list->string (list ch))
                                                    imports::list_minus__g_string(&[{
                                                        // (list ch)
                                                        imports::list(&[ch_0.clone()])
                                                    }])
                                                }
                                            }
                                        }
                                    })
                                });
                                Scm::anything();
                                append_minus_all_0.set({
                                    // Closure
                                    let append_minus_all_0 = append_minus_all_0.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let strs_0 = args[0].clone();
                                        if ({
                                            // (null? strs)
                                            imports::null_p(&[strs_0.clone()])
                                        })
                                        .is_true()
                                        {
                                            Scm::from("")
                                        } else {
                                            {
                                                // (string-append (car strs) (append-all (cdr strs)))
                                                imports::string_minus_append(&[
                                                    {
                                                        // (car strs)
                                                        imports::car(&[strs_0.clone()])
                                                    },
                                                    {
                                                        // (append-all (cdr strs))
                                                        append_minus_all_0.get().invoke(&[{
                                                            // (cdr strs)
                                                            imports::cdr(&[strs_0.clone()])
                                                        }])
                                                    },
                                                ])
                                            }
                                        }
                                    })
                                });
                                Scm::anything();
                                {
                                    // (cond ...)
                                    if ({
                                        // (same-name? name (quote args))
                                        imports::same_minus_name_p(&[
                                            name_20.clone(),
                                            Scm::symbol("args"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("args_")
                                    } else if ({
                                        // (same-name? name (quote fn))
                                        imports::same_minus_name_p(&[
                                            name_20.clone(),
                                            Scm::symbol("fn"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("fn_")
                                    } else if ({
                                        // (same-name? name (quote loop))
                                        imports::same_minus_name_p(&[
                                            name_20.clone(),
                                            Scm::symbol("loop"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("loop_")
                                    } else if ({
                                        // (same-name? name (quote let))
                                        imports::same_minus_name_p(&[
                                            name_20.clone(),
                                            Scm::symbol("let"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("let_")
                                    } else if ({
                                        // (same-name? name (quote mut))
                                        imports::same_minus_name_p(&[
                                            name_20.clone(),
                                            Scm::symbol("mut"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("mut_")
                                    } else if ({
                                        // (same-name? name (quote ref))
                                        imports::same_minus_name_p(&[
                                            name_20.clone(),
                                            Scm::symbol("ref"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("ref_")
                                    } else if ({
                                        // (same-name? name (quote self))
                                        imports::same_minus_name_p(&[
                                            name_20.clone(),
                                            Scm::symbol("self"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("self_")
                                    } else {
                                        {
                                            // (append-all (map char-map (string->list (if (symbol? name) (symbol->string name) name))))
                                            append_minus_all_0.get().invoke(&[{
                                                // (map char-map (string->list (if (symbol? name) (symbol->string name) name)))
                                                imports::map(&[char_minus_map_0.get(), {
                                                    // (string->list (if (symbol? name) (symbol->string name) name))
                                                    imports::string_minus__g_list(&[
                                                        if ({
                                                            // (symbol? name)
                                                            imports::symbol_p(&[name_20.clone()])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (symbol->string name)
                                                                imports::symbol_minus__g_string(&[
                                                                    name_20.clone(),
                                                                ])
                                                            }
                                                        } else {
                                                            name_20.clone()
                                                        },
                                                    ])
                                                }])
                                            }])
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    .into()
}
pub fn rustify_minus_libname(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name_21 = args[0].clone();
        {
            // (letrec ((char-map (lambda (ch) (cond ((eq? ch #\_) "__") ((eq? ch #\-) "_") (else (list->string (list ch)))))) (append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs))))))) (let ((name (if (symbol? name) (symbol->string name) name))) (cond ((eq? name (quote fn)) "fn_") (else (append-all (map char-map (string->list name)))))))
            {
                // (let ((char-map (quote *uninitialized*)) (append-all (quote *uninitialized*))) (begin (set! char-map (lambda (ch) (cond ((eq? ch #\_) "__") ((eq? ch #\-) "_") (else (list->string (list ch)))))) (set! append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs)))))) (let ((name (if (symbol? name) (symbol->string name) name))) (cond ((eq? name (quote fn)) "fn_") (else (append-all (map char-map (string->list name))))))))
                {
                    let [char_minus_map_1, append_minus_all_1] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let append_minus_all_1 = append_minus_all_1.into_boxed();
                        {
                            let char_minus_map_1 = char_minus_map_1.into_boxed();
                            {
                                char_minus_map_1.set({
                                    // Closure
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let ch_1 = args[0].clone();
                                        {
                                            // (cond ...)
                                            if ({
                                                // (eq? ch #\_)
                                                imports::eq_p(&[ch_1.clone(), Scm::char('_')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("__")
                                            } else if ({
                                                // (eq? ch #\-)
                                                imports::eq_p(&[ch_1.clone(), Scm::char('-')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_")
                                            } else {
                                                {
                                                    // (list->string (list ch))
                                                    imports::list_minus__g_string(&[{
                                                        // (list ch)
                                                        imports::list(&[ch_1.clone()])
                                                    }])
                                                }
                                            }
                                        }
                                    })
                                });
                                Scm::anything();
                                append_minus_all_1.set({
                                    // Closure
                                    let append_minus_all_1 = append_minus_all_1.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let strs_1 = args[0].clone();
                                        if ({
                                            // (null? strs)
                                            imports::null_p(&[strs_1.clone()])
                                        })
                                        .is_true()
                                        {
                                            Scm::from("")
                                        } else {
                                            {
                                                // (string-append (car strs) (append-all (cdr strs)))
                                                imports::string_minus_append(&[
                                                    {
                                                        // (car strs)
                                                        imports::car(&[strs_1.clone()])
                                                    },
                                                    {
                                                        // (append-all (cdr strs))
                                                        append_minus_all_1.get().invoke(&[{
                                                            // (cdr strs)
                                                            imports::cdr(&[strs_1.clone()])
                                                        }])
                                                    },
                                                ])
                                            }
                                        }
                                    })
                                });
                                Scm::anything();
                                {
                                    // (let ((name (if (symbol? name) (symbol->string name) name))) (cond ((eq? name (quote fn)) "fn_") (else (append-all (map char-map (string->list name))))))
                                    {
                                        let name_22 = if ({
                                            // (symbol? name)
                                            imports::symbol_p(&[name_21.clone()])
                                        })
                                        .is_true()
                                        {
                                            {
                                                // (symbol->string name)
                                                imports::symbol_minus__g_string(&[name_21.clone()])
                                            }
                                        } else {
                                            name_21.clone()
                                        };
                                        {
                                            // (cond ...)
                                            if ({
                                                // (eq? name (quote fn))
                                                imports::eq_p(&[name_22.clone(), Scm::symbol("fn")])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("fn_")
                                            } else {
                                                {
                                                    // (append-all (map char-map (string->list name)))
                                                    append_minus_all_1.get().invoke(&[{
                                                        // (map char-map (string->list name))
                                                        imports::map(&[char_minus_map_1.get(), {
                                                            // (string->list name)
                                                            imports::string_minus__g_list(&[
                                                                name_22.clone(),
                                                            ])
                                                        }])
                                                    }])
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    .into()
}
pub fn rustify_minus_testname(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name_23 = args[0].clone();
        {
            // (letrec ((char-map (lambda (ch) (cond ((eq? ch #\ ) "_") ((eq? ch #\') #f) (else (list->string (list ch)))))) (append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs))))))) (append-all (filter (lambda (x) x) (map char-map (string->list name)))))
            {
                // (let ((char-map (quote *uninitialized*)) (append-all (quote *uninitialized*))) (begin (set! char-map (lambda (ch) (cond ((eq? ch #\ ) "_") ((eq? ch #\') #f) (else (list->string (list ch)))))) (set! append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs)))))) (append-all (filter (lambda (x) x) (map char-map (string->list name))))))
                {
                    let [char_minus_map_2, append_minus_all_2] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let append_minus_all_2 = append_minus_all_2.into_boxed();
                        {
                            let char_minus_map_2 = char_minus_map_2.into_boxed();
                            {
                                char_minus_map_2.set({
                                    // Closure
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let ch_2 = args[0].clone();
                                        {
                                            // (cond ...)
                                            if ({
                                                // (eq? ch #\ )
                                                imports::eq_p(&[ch_2.clone(), Scm::char(' ')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_")
                                            } else if ({
                                                // (eq? ch #\')
                                                imports::eq_p(&[
                                                    ch_2.clone(),
                                                    Scm::char_apostrophe(),
                                                ])
                                            })
                                            .is_true()
                                            {
                                                Scm::False
                                            } else {
                                                {
                                                    // (list->string (list ch))
                                                    imports::list_minus__g_string(&[{
                                                        // (list ch)
                                                        imports::list(&[ch_2.clone()])
                                                    }])
                                                }
                                            }
                                        }
                                    })
                                });
                                Scm::anything();
                                append_minus_all_2.set({
                                    // Closure
                                    let append_minus_all_2 = append_minus_all_2.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let strs_2 = args[0].clone();
                                        if ({
                                            // (null? strs)
                                            imports::null_p(&[strs_2.clone()])
                                        })
                                        .is_true()
                                        {
                                            Scm::from("")
                                        } else {
                                            {
                                                // (string-append (car strs) (append-all (cdr strs)))
                                                imports::string_minus_append(&[
                                                    {
                                                        // (car strs)
                                                        imports::car(&[strs_2.clone()])
                                                    },
                                                    {
                                                        // (append-all (cdr strs))
                                                        append_minus_all_2.get().invoke(&[{
                                                            // (cdr strs)
                                                            imports::cdr(&[strs_2.clone()])
                                                        }])
                                                    },
                                                ])
                                            }
                                        }
                                    })
                                });
                                Scm::anything();
                                {
                                    // (append-all (filter (lambda (x) x) (map char-map (string->list name))))
                                    append_minus_all_2.get().invoke(&[{
                                        // (filter (lambda (x) x) (map char-map (string->list name)))
                                        imports::filter(&[
                                            {
                                                // Closure
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let x_31 = args[0].clone();
                                                    x_31.clone()
                                                })
                                            },
                                            {
                                                // (map char-map (string->list name))
                                                imports::map(&[char_minus_map_2.get(), {
                                                    // (string->list name)
                                                    imports::string_minus__g_list(
                                                        &[name_23.clone()],
                                                    )
                                                }])
                                            },
                                        ])
                                    }])
                                }
                            }
                        }
                    }
                }
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
    crate::sunny::utils::initialize();
    {
        (/*NOP*/);
        {
            // (define (rustify-identifier name) ...)
            (/*NOP*/)
        };
        {
            // (define (rustify-libname name) ...)
            (/*NOP*/)
        };
        {
            // (define (rustify-testname name) ...)
            (/*NOP*/)
        }
    };
}
