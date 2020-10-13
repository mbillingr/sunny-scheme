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
        let name__20 = args[0].clone();
        {
            // (letrec ((char-map (lambda (ch) (cond ((eq? ch #\_) "__") ((eq? ch #\?) "_p") ((eq? ch #\!) "_i") ((eq? ch #\<) "_l_") ((eq? ch #\>) "_g_") ((eq? ch #\=) "_e_") ((eq? ch #\-) "_minus_") ((eq? ch #\+) "_plus_") ((eq? ch #\*) "_star_") ((eq? ch #\/) "_slash_") (else (list->string (list ch)))))) (append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs))))))) (cond ((same-name? name (quote args)) "args_") ((same-name? name (quote fn)) "fn_") ((same-name? name (quote loop)) "loop_") ((same-name? name (quote let)) "let_") ((same-name? name (quote mut)) "mut_") ((same-name? name (quote ref)) "ref_") ((same-name? name (quote self)) "self_") (else (append-all (map char-map (string->list (if (symbol? name) (symbol->string name) name)))))))
            {
                // (let ((char-map (quote *uninitialized*)) (append-all (quote *uninitialized*))) (begin (set! char-map (lambda (ch) (cond ((eq? ch #\_) "__") ((eq? ch #\?) "_p") ((eq? ch #\!) "_i") ((eq? ch #\<) "_l_") ((eq? ch #\>) "_g_") ((eq? ch #\=) "_e_") ((eq? ch #\-) "_minus_") ((eq? ch #\+) "_plus_") ((eq? ch #\*) "_star_") ((eq? ch #\/) "_slash_") (else (list->string (list ch)))))) (set! append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs)))))) (cond ((same-name? name (quote args)) "args_") ((same-name? name (quote fn)) "fn_") ((same-name? name (quote loop)) "loop_") ((same-name? name (quote let)) "let_") ((same-name? name (quote mut)) "mut_") ((same-name? name (quote ref)) "ref_") ((same-name? name (quote self)) "self_") (else (append-all (map char-map (string->list (if (symbol? name) (symbol->string name) name))))))))
                {
                    let [char_minus_map__0, append_minus_all__0] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let append_minus_all__0 = append_minus_all__0.into_boxed();
                        {
                            let char_minus_map__0 = char_minus_map__0.into_boxed();
                            {
                                char_minus_map__0.set({
                                    // Closure
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let ch__0 = args[0].clone();
                                        {
                                            // (cond ...)
                                            if ({
                                                // (eq? ch #\_)
                                                imports::eq_p(&[ch__0.clone(), Scm::char('_')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("__")
                                            } else if ({
                                                // (eq? ch #\?)
                                                imports::eq_p(&[ch__0.clone(), Scm::char('?')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_p")
                                            } else if ({
                                                // (eq? ch #\!)
                                                imports::eq_p(&[ch__0.clone(), Scm::char('!')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_i")
                                            } else if ({
                                                // (eq? ch #\<)
                                                imports::eq_p(&[ch__0.clone(), Scm::char('<')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_l_")
                                            } else if ({
                                                // (eq? ch #\>)
                                                imports::eq_p(&[ch__0.clone(), Scm::char('>')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_g_")
                                            } else if ({
                                                // (eq? ch #\=)
                                                imports::eq_p(&[ch__0.clone(), Scm::char('=')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_e_")
                                            } else if ({
                                                // (eq? ch #\-)
                                                imports::eq_p(&[ch__0.clone(), Scm::char('-')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_minus_")
                                            } else if ({
                                                // (eq? ch #\+)
                                                imports::eq_p(&[ch__0.clone(), Scm::char('+')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_plus_")
                                            } else if ({
                                                // (eq? ch #\*)
                                                imports::eq_p(&[ch__0.clone(), Scm::char('*')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_star_")
                                            } else if ({
                                                // (eq? ch #\/)
                                                imports::eq_p(&[ch__0.clone(), Scm::char('/')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_slash_")
                                            } else {
                                                {
                                                    // (list->string (list ch))
                                                    imports::list_minus__g_string(&[{
                                                        // (list ch)
                                                        imports::list(&[ch__0.clone()])
                                                    }])
                                                }
                                            }
                                        }
                                    })
                                });
                                Scm::anything();
                                append_minus_all__0.set({
                                    // Closure
                                    let append_minus_all__0 = append_minus_all__0.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let strs__0 = args[0].clone();
                                        if ({
                                            // (null? strs)
                                            imports::null_p(&[strs__0.clone()])
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
                                                        imports::car(&[strs__0.clone()])
                                                    },
                                                    {
                                                        // (append-all (cdr strs))
                                                        append_minus_all__0.get().invoke(&[{
                                                            // (cdr strs)
                                                            imports::cdr(&[strs__0.clone()])
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
                                            name__20.clone(),
                                            Scm::symbol("args"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("args_")
                                    } else if ({
                                        // (same-name? name (quote fn))
                                        imports::same_minus_name_p(&[
                                            name__20.clone(),
                                            Scm::symbol("fn"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("fn_")
                                    } else if ({
                                        // (same-name? name (quote loop))
                                        imports::same_minus_name_p(&[
                                            name__20.clone(),
                                            Scm::symbol("loop"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("loop_")
                                    } else if ({
                                        // (same-name? name (quote let))
                                        imports::same_minus_name_p(&[
                                            name__20.clone(),
                                            Scm::symbol("let"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("let_")
                                    } else if ({
                                        // (same-name? name (quote mut))
                                        imports::same_minus_name_p(&[
                                            name__20.clone(),
                                            Scm::symbol("mut"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("mut_")
                                    } else if ({
                                        // (same-name? name (quote ref))
                                        imports::same_minus_name_p(&[
                                            name__20.clone(),
                                            Scm::symbol("ref"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("ref_")
                                    } else if ({
                                        // (same-name? name (quote self))
                                        imports::same_minus_name_p(&[
                                            name__20.clone(),
                                            Scm::symbol("self"),
                                        ])
                                    })
                                    .is_true()
                                    {
                                        Scm::from("self_")
                                    } else {
                                        {
                                            // (append-all (map char-map (string->list (if (symbol? name) (symbol->string name) name))))
                                            append_minus_all__0.get().invoke(&[{
                                                // (map char-map (string->list (if (symbol? name) (symbol->string name) name)))
                                                imports::map(&[char_minus_map__0.get(), {
                                                    // (string->list (if (symbol? name) (symbol->string name) name))
                                                    imports::string_minus__g_list(&[
                                                        if ({
                                                            // (symbol? name)
                                                            imports::symbol_p(&[name__20.clone()])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (symbol->string name)
                                                                imports::symbol_minus__g_string(&[
                                                                    name__20.clone(),
                                                                ])
                                                            }
                                                        } else {
                                                            name__20.clone()
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
        let name__21 = args[0].clone();
        {
            // (letrec ((char-map (lambda (ch) (cond ((eq? ch #\_) "__") ((eq? ch #\-) "_") (else (list->string (list ch)))))) (append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs))))))) (let ((name (if (symbol? name) (symbol->string name) name))) (cond ((eq? name (quote fn)) "fn_") (else (append-all (map char-map (string->list name)))))))
            {
                // (let ((char-map (quote *uninitialized*)) (append-all (quote *uninitialized*))) (begin (set! char-map (lambda (ch) (cond ((eq? ch #\_) "__") ((eq? ch #\-) "_") (else (list->string (list ch)))))) (set! append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs)))))) (let ((name (if (symbol? name) (symbol->string name) name))) (cond ((eq? name (quote fn)) "fn_") (else (append-all (map char-map (string->list name))))))))
                {
                    let [char_minus_map__1, append_minus_all__1] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let append_minus_all__1 = append_minus_all__1.into_boxed();
                        {
                            let char_minus_map__1 = char_minus_map__1.into_boxed();
                            {
                                char_minus_map__1.set({
                                    // Closure
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let ch__1 = args[0].clone();
                                        {
                                            // (cond ...)
                                            if ({
                                                // (eq? ch #\_)
                                                imports::eq_p(&[ch__1.clone(), Scm::char('_')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("__")
                                            } else if ({
                                                // (eq? ch #\-)
                                                imports::eq_p(&[ch__1.clone(), Scm::char('-')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_")
                                            } else {
                                                {
                                                    // (list->string (list ch))
                                                    imports::list_minus__g_string(&[{
                                                        // (list ch)
                                                        imports::list(&[ch__1.clone()])
                                                    }])
                                                }
                                            }
                                        }
                                    })
                                });
                                Scm::anything();
                                append_minus_all__1.set({
                                    // Closure
                                    let append_minus_all__1 = append_minus_all__1.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let strs__1 = args[0].clone();
                                        if ({
                                            // (null? strs)
                                            imports::null_p(&[strs__1.clone()])
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
                                                        imports::car(&[strs__1.clone()])
                                                    },
                                                    {
                                                        // (append-all (cdr strs))
                                                        append_minus_all__1.get().invoke(&[{
                                                            // (cdr strs)
                                                            imports::cdr(&[strs__1.clone()])
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
                                        let name__22 = if ({
                                            // (symbol? name)
                                            imports::symbol_p(&[name__21.clone()])
                                        })
                                        .is_true()
                                        {
                                            {
                                                // (symbol->string name)
                                                imports::symbol_minus__g_string(&[name__21.clone()])
                                            }
                                        } else {
                                            name__21.clone()
                                        };
                                        {
                                            // (cond ...)
                                            if ({
                                                // (eq? name (quote fn))
                                                imports::eq_p(&[
                                                    name__22.clone(),
                                                    Scm::symbol("fn"),
                                                ])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("fn_")
                                            } else {
                                                {
                                                    // (append-all (map char-map (string->list name)))
                                                    append_minus_all__1.get().invoke(&[{
                                                        // (map char-map (string->list name))
                                                        imports::map(&[char_minus_map__1.get(), {
                                                            // (string->list name)
                                                            imports::string_minus__g_list(&[
                                                                name__22.clone(),
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
        let name__23 = args[0].clone();
        {
            // (letrec ((char-map (lambda (ch) (cond ((eq? ch #\ ) "_") ((eq? ch #\') #f) (else (list->string (list ch)))))) (append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs))))))) (append-all (filter (lambda (x) x) (map char-map (string->list name)))))
            {
                // (let ((char-map (quote *uninitialized*)) (append-all (quote *uninitialized*))) (begin (set! char-map (lambda (ch) (cond ((eq? ch #\ ) "_") ((eq? ch #\') #f) (else (list->string (list ch)))))) (set! append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs)))))) (append-all (filter (lambda (x) x) (map char-map (string->list name))))))
                {
                    let [char_minus_map__2, append_minus_all__2] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let append_minus_all__2 = append_minus_all__2.into_boxed();
                        {
                            let char_minus_map__2 = char_minus_map__2.into_boxed();
                            {
                                char_minus_map__2.set({
                                    // Closure
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let ch__2 = args[0].clone();
                                        {
                                            // (cond ...)
                                            if ({
                                                // (eq? ch #\ )
                                                imports::eq_p(&[ch__2.clone(), Scm::char(' ')])
                                            })
                                            .is_true()
                                            {
                                                Scm::from("_")
                                            } else if ({
                                                // (eq? ch #\')
                                                imports::eq_p(&[
                                                    ch__2.clone(),
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
                                                        imports::list(&[ch__2.clone()])
                                                    }])
                                                }
                                            }
                                        }
                                    })
                                });
                                Scm::anything();
                                append_minus_all__2.set({
                                    // Closure
                                    let append_minus_all__2 = append_minus_all__2.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let strs__2 = args[0].clone();
                                        if ({
                                            // (null? strs)
                                            imports::null_p(&[strs__2.clone()])
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
                                                        imports::car(&[strs__2.clone()])
                                                    },
                                                    {
                                                        // (append-all (cdr strs))
                                                        append_minus_all__2.get().invoke(&[{
                                                            // (cdr strs)
                                                            imports::cdr(&[strs__2.clone()])
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
                                    append_minus_all__2.get().invoke(&[{
                                        // (filter (lambda (x) x) (map char-map (string->list name)))
                                        imports::filter(&[
                                            {
                                                // Closure
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let x__31 = args[0].clone();
                                                    x__31.clone()
                                                })
                                            },
                                            {
                                                // (map char-map (string->list name))
                                                imports::map(&[char_minus_map__2.get(), {
                                                    // (string->list name)
                                                    imports::string_minus__g_list(&[
                                                        name__23.clone()
                                                    ])
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
