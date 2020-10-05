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
        let name = args[0].clone();
        {
            // (letrec ((char-map (lambda (ch) (cond ((eq? ch #\_) "__") ((eq? ch #\?) "_p") ((eq? ch #\!) "_i") ((eq? ch #\<) "_l_") ((eq? ch #\>) "_g_") ((eq? ch #\=) "_e_") ((eq? ch #\-) "_minus_") ((eq? ch #\+) "_plus_") ((eq? ch #\*) "_star_") ((eq? ch #\/) "_slash_") (else (list->string (list ch)))))) (append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs))))))) (cond ((eq? name (quote args)) "args_") ((eq? name (quote fn)) "fn_") ((eq? name (quote loop)) "loop_") ((eq? name (quote let)) "let_") ((eq? name (quote mut)) "mut_") ((eq? name (quote ref)) "ref_") ((eq? name (quote self)) "self_") (else (append-all (map char-map (string->list (symbol->string name)))))))
            {
                // (let ((char-map (quote *uninitialized*)) (append-all (quote *uninitialized*))) (begin (set! char-map (lambda (ch) (cond ((eq? ch #\_) "__") ((eq? ch #\?) "_p") ((eq? ch #\!) "_i") ((eq? ch #\<) "_l_") ((eq? ch #\>) "_g_") ((eq? ch #\=) "_e_") ((eq? ch #\-) "_minus_") ((eq? ch #\+) "_plus_") ((eq? ch #\*) "_star_") ((eq? ch #\/) "_slash_") (else (list->string (list ch)))))) (set! append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs)))))) (cond ((eq? name (quote args)) "args_") ((eq? name (quote fn)) "fn_") ((eq? name (quote loop)) "loop_") ((eq? name (quote let)) "let_") ((eq? name (quote mut)) "mut_") ((eq? name (quote ref)) "ref_") ((eq? name (quote self)) "self_") (else (append-all (map char-map (string->list (symbol->string name))))))))
                {
                    let [char_minus_map, append_minus_all] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let append_minus_all = append_minus_all.into_boxed();
                        {
                            let char_minus_map = char_minus_map.into_boxed();
                            {
                                char_minus_map.set({
                                    // Closure
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let ch = args[0].clone();
                                        {
                                            // (cond ...)
                                            if ({
                                                // (eq? ch #\_)
                                                Scm::func(imports::eq_p)
                                                    .invoke(&[ch.clone(), Scm::char('_')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("__")
                                            } else if ({
                                                // (eq? ch #\?)
                                                Scm::func(imports::eq_p)
                                                    .invoke(&[ch.clone(), Scm::char('?')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_p")
                                            } else if ({
                                                // (eq? ch #\!)
                                                Scm::func(imports::eq_p)
                                                    .invoke(&[ch.clone(), Scm::char('!')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_i")
                                            } else if ({
                                                // (eq? ch #\<)
                                                Scm::func(imports::eq_p)
                                                    .invoke(&[ch.clone(), Scm::char('<')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_l_")
                                            } else if ({
                                                // (eq? ch #\>)
                                                Scm::func(imports::eq_p)
                                                    .invoke(&[ch.clone(), Scm::char('>')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_g_")
                                            } else if ({
                                                // (eq? ch #\=)
                                                Scm::func(imports::eq_p)
                                                    .invoke(&[ch.clone(), Scm::char('=')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_e_")
                                            } else if ({
                                                // (eq? ch #\-)
                                                Scm::func(imports::eq_p)
                                                    .invoke(&[ch.clone(), Scm::char('-')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_minus_")
                                            } else if ({
                                                // (eq? ch #\+)
                                                Scm::func(imports::eq_p)
                                                    .invoke(&[ch.clone(), Scm::char('+')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_plus_")
                                            } else if ({
                                                // (eq? ch #\*)
                                                Scm::func(imports::eq_p)
                                                    .invoke(&[ch.clone(), Scm::char('*')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_star_")
                                            } else if ({
                                                // (eq? ch #\/)
                                                Scm::func(imports::eq_p)
                                                    .invoke(&[ch.clone(), Scm::char('/')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_slash_")
                                            } else {
                                                {
                                                    // (list->string (list ch))
                                                    Scm::func(imports::list_minus__g_string).invoke(
                                                        &[{
                                                            // (list ch)
                                                            Scm::func(imports::list)
                                                                .invoke(&[ch.clone()])
                                                        }],
                                                    )
                                                }
                                            }
                                        }
                                    })
                                });
                                append_minus_all.set({
                                    // Closure
                                    let append_minus_all = append_minus_all.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let strs = args[0].clone();
                                        if ({
                                            // (null? strs)
                                            Scm::func(imports::null_p).invoke(&[strs.clone()])
                                        })
                                        .is_true()
                                        {
                                            Scm::from("")
                                        } else {
                                            {
                                                // (string-append (car strs) (append-all (cdr strs)))
                                                Scm::func(imports::string_minus_append).invoke(&[
                                                    {
                                                        // (car strs)
                                                        Scm::func(imports::car)
                                                            .invoke(&[strs.clone()])
                                                    },
                                                    {
                                                        // (append-all (cdr strs))
                                                        append_minus_all.get().invoke(&[{
                                                            // (cdr strs)
                                                            Scm::func(imports::cdr)
                                                                .invoke(&[strs.clone()])
                                                        }])
                                                    },
                                                ])
                                            }
                                        }
                                    })
                                });
                                {
                                    // (cond ...)
                                    if ({
                                        // (eq? name (quote args))
                                        Scm::func(imports::eq_p)
                                            .invoke(&[name.clone(), Scm::symbol("args")])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("args_")
                                    } else if ({
                                        // (eq? name (quote fn))
                                        Scm::func(imports::eq_p)
                                            .invoke(&[name.clone(), Scm::symbol("fn")])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("fn_")
                                    } else if ({
                                        // (eq? name (quote loop))
                                        Scm::func(imports::eq_p)
                                            .invoke(&[name.clone(), Scm::symbol("loop")])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("loop_")
                                    } else if ({
                                        // (eq? name (quote let))
                                        Scm::func(imports::eq_p)
                                            .invoke(&[name.clone(), Scm::symbol("let")])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("let_")
                                    } else if ({
                                        // (eq? name (quote mut))
                                        Scm::func(imports::eq_p)
                                            .invoke(&[name.clone(), Scm::symbol("mut")])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("mut_")
                                    } else if ({
                                        // (eq? name (quote ref))
                                        Scm::func(imports::eq_p)
                                            .invoke(&[name.clone(), Scm::symbol("ref")])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("ref_")
                                    } else if ({
                                        // (eq? name (quote self))
                                        Scm::func(imports::eq_p)
                                            .invoke(&[name.clone(), Scm::symbol("self")])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("self_")
                                    } else {
                                        {
                                            // (append-all (map char-map (string->list (symbol->string name))))
                                            append_minus_all.get().invoke(&[{
                                                // (map char-map (string->list (symbol->string name)))
                                                Scm::func(imports::map).invoke(&[
                                                    char_minus_map.get(),
                                                    {
                                                        // (string->list (symbol->string name))
                                                        Scm::func(imports::string_minus__g_list)
                                                            .invoke(&[{
                                                                // (symbol->string name)
                                                                Scm::func(
                                                                    imports::symbol_minus__g_string,
                                                                )
                                                                .invoke(&[name.clone()])
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
        }
    }
    .into()
}
pub fn rustify_minus_libname(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name = args[0].clone();
        {
            // (letrec ((char-map (lambda (ch) (cond ((eq? ch #\_) "__") ((eq? ch #\-) "_") (else (list->string (list ch)))))) (append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs))))))) (let ((name (if (symbol? name) (symbol->string name) name))) (cond ((eq? name (quote fn)) "fn_") (else (append-all (map char-map (string->list name)))))))
            {
                // (let ((char-map (quote *uninitialized*)) (append-all (quote *uninitialized*))) (begin (set! char-map (lambda (ch) (cond ((eq? ch #\_) "__") ((eq? ch #\-) "_") (else (list->string (list ch)))))) (set! append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs)))))) (let ((name (if (symbol? name) (symbol->string name) name))) (cond ((eq? name (quote fn)) "fn_") (else (append-all (map char-map (string->list name))))))))
                {
                    let [char_minus_map, append_minus_all] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let append_minus_all = append_minus_all.into_boxed();
                        {
                            let char_minus_map = char_minus_map.into_boxed();
                            {
                                char_minus_map.set({
                                    // Closure
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let ch = args[0].clone();
                                        {
                                            // (cond ...)
                                            if ({
                                                // (eq? ch #\_)
                                                Scm::func(imports::eq_p)
                                                    .invoke(&[ch.clone(), Scm::char('_')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("__")
                                            } else if ({
                                                // (eq? ch #\-)
                                                Scm::func(imports::eq_p)
                                                    .invoke(&[ch.clone(), Scm::char('-')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_")
                                            } else {
                                                {
                                                    // (list->string (list ch))
                                                    Scm::func(imports::list_minus__g_string).invoke(
                                                        &[{
                                                            // (list ch)
                                                            Scm::func(imports::list)
                                                                .invoke(&[ch.clone()])
                                                        }],
                                                    )
                                                }
                                            }
                                        }
                                    })
                                });
                                append_minus_all.set({
                                    // Closure
                                    let append_minus_all = append_minus_all.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let strs = args[0].clone();
                                        if ({
                                            // (null? strs)
                                            Scm::func(imports::null_p).invoke(&[strs.clone()])
                                        })
                                        .is_true()
                                        {
                                            Scm::from("")
                                        } else {
                                            {
                                                // (string-append (car strs) (append-all (cdr strs)))
                                                Scm::func(imports::string_minus_append).invoke(&[
                                                    {
                                                        // (car strs)
                                                        Scm::func(imports::car)
                                                            .invoke(&[strs.clone()])
                                                    },
                                                    {
                                                        // (append-all (cdr strs))
                                                        append_minus_all.get().invoke(&[{
                                                            // (cdr strs)
                                                            Scm::func(imports::cdr)
                                                                .invoke(&[strs.clone()])
                                                        }])
                                                    },
                                                ])
                                            }
                                        }
                                    })
                                });
                                {
                                    // (let ((name (if (symbol? name) (symbol->string name) name))) (cond ((eq? name (quote fn)) "fn_") (else (append-all (map char-map (string->list name))))))
                                    {
                                        let name = if ({
                                            // (symbol? name)
                                            Scm::func(imports::symbol_p).invoke(&[name.clone()])
                                        })
                                        .is_true()
                                        {
                                            {
                                                // (symbol->string name)
                                                Scm::func(imports::symbol_minus__g_string)
                                                    .invoke(&[name.clone()])
                                            }
                                        } else {
                                            name.clone()
                                        };
                                        {
                                            // (cond ...)
                                            if ({
                                                // (eq? name (quote fn))
                                                Scm::func(imports::eq_p)
                                                    .invoke(&[name.clone(), Scm::symbol("fn")])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("fn_")
                                            } else {
                                                {
                                                    // (append-all (map char-map (string->list name)))
                                                    append_minus_all.get().invoke(&[{
                                                        // (map char-map (string->list name))
                                                        Scm::func(imports::map).invoke(&[
                                                            char_minus_map.get(),
                                                            {
                                                                // (string->list name)
                                                                Scm::func(
                                                                    imports::string_minus__g_list,
                                                                )
                                                                .invoke(&[name.clone()])
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
        let name = args[0].clone();
        {
            // (letrec ((char-map (lambda (ch) (cond ((eq? ch #\ ) "_") ((eq? ch #\') #f) (else (list->string (list ch)))))) (append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs))))))) (append-all (filter (lambda (x) x) (map char-map (string->list name)))))
            {
                // (let ((char-map (quote *uninitialized*)) (append-all (quote *uninitialized*))) (begin (set! char-map (lambda (ch) (cond ((eq? ch #\ ) "_") ((eq? ch #\') #f) (else (list->string (list ch)))))) (set! append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs)))))) (append-all (filter (lambda (x) x) (map char-map (string->list name))))))
                {
                    let [char_minus_map, append_minus_all] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let append_minus_all = append_minus_all.into_boxed();
                        {
                            let char_minus_map = char_minus_map.into_boxed();
                            {
                                char_minus_map.set({
                                    // Closure
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let ch = args[0].clone();
                                        {
                                            // (cond ...)
                                            if ({
                                                // (eq? ch #\ )
                                                Scm::func(imports::eq_p)
                                                    .invoke(&[ch.clone(), Scm::char(' ')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_")
                                            } else if ({
                                                // (eq? ch #\')
                                                Scm::func(imports::eq_p)
                                                    .invoke(&[ch.clone(), Scm::char_apostrophe()])
                                            })
                                            .is_true()
                                            {
                                                Scm::False
                                            } else {
                                                {
                                                    // (list->string (list ch))
                                                    Scm::func(imports::list_minus__g_string).invoke(
                                                        &[{
                                                            // (list ch)
                                                            Scm::func(imports::list)
                                                                .invoke(&[ch.clone()])
                                                        }],
                                                    )
                                                }
                                            }
                                        }
                                    })
                                });
                                append_minus_all.set({
                                    // Closure
                                    let append_minus_all = append_minus_all.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let strs = args[0].clone();
                                        if ({
                                            // (null? strs)
                                            Scm::func(imports::null_p).invoke(&[strs.clone()])
                                        })
                                        .is_true()
                                        {
                                            Scm::from("")
                                        } else {
                                            {
                                                // (string-append (car strs) (append-all (cdr strs)))
                                                Scm::func(imports::string_minus_append).invoke(&[
                                                    {
                                                        // (car strs)
                                                        Scm::func(imports::car)
                                                            .invoke(&[strs.clone()])
                                                    },
                                                    {
                                                        // (append-all (cdr strs))
                                                        append_minus_all.get().invoke(&[{
                                                            // (cdr strs)
                                                            Scm::func(imports::cdr)
                                                                .invoke(&[strs.clone()])
                                                        }])
                                                    },
                                                ])
                                            }
                                        }
                                    })
                                });
                                {
                                    // (append-all (filter (lambda (x) x) (map char-map (string->list name))))
                                    append_minus_all.get().invoke(&[{
                                        // (filter (lambda (x) x) (map char-map (string->list name)))
                                        Scm::func(imports::filter).invoke(&[
                                            {
                                                // Closure
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let x = args[0].clone();
                                                    x.clone()
                                                })
                                            },
                                            {
                                                // (map char-map (string->list name))
                                                Scm::func(imports::map).invoke(&[
                                                    char_minus_map.get(),
                                                    {
                                                        // (string->list name)
                                                        Scm::func(imports::string_minus__g_list)
                                                            .invoke(&[name.clone()])
                                                    },
                                                ])
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
