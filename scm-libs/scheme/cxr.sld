(define-library (scheme cxr)
  (export
    caaar caadr cadar caddr
    cdaar cdadr cddar cdddr

    caaaar caaadr caadar caaddr
    cadaar cadadr caddar cadddr

    cdaaar cdaadr cdadar cdaddr
    cddaar cddadr cdddar cddddr)

  (import (only (scheme base) car cdr caar cadr cdar cddr))

  (begin
    (define (caaar x) (car (caar x)))
    (define (caadr x) (car (cadr x)))
    (define (cadar x) (car (cdar x)))
    (define (caddr x) (car (cddr x)))

    (define (cdaar x) (cdr (caar x)))
    (define (cdadr x) (cdr (cadr x)))
    (define (cddar x) (cdr (cdar x)))
    (define (cdddr x) (cdr (cddr x)))

    (define (caaaar x) (caar (caar x)))
    (define (caaadr x) (caar (cadr x)))
    (define (caadar x) (caar (cdar x)))
    (define (caaddr x) (caar (cddr x)))

    (define (cadaar x) (cadr (caar x)))
    (define (cadadr x) (cadr (cadr x)))
    (define (caddar x) (cadr (cdar x)))
    (define (cadddr x) (cadr (cddr x)))

    (define (cdaaar x) (cdar (caar x)))
    (define (cdaadr x) (cdar (cadr x)))
    (define (cdadar x) (cdar (cdar x)))
    (define (cdaddr x) (cdar (cddr x)))

    (define (cddaar x) (cddr (caar x)))
    (define (cddadr x) (cddr (cadr x)))
    (define (cdddar x) (cddr (cdar x)))
    (define (cddddr x) (cddr (cddr x)))))
