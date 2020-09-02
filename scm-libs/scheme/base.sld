(define-library (scheme base)
  (export
    = > < - +
    assq
    car caar cadr
    cdr cdar cddr
    char? close-port cons
    eof-object?
    eq? equal?
    error
    length
    list
    list?
    list->string
    list-copy
    memq
    not
    null?
    pair?
    reverse
    set-car! set-cdr!
    string->list string=? string-append string<?
    symbol? symbol->string symbol=?)

  (import (native base))

  (begin

    ; ====== logic =======================

    (define (not x)
      (if x #f #t))

    ; ====== lists =======================

    (define (list . x) x)

    (define (list? seq)
      (cond ((null? seq) #t)
            ((pair? seq) (list? (cdr seq)))
            (else #f)))

    (define (length seq)
      (if (pair? seq)
          (+ 1 (length (cdr seq)))
          0))

    (define (reverse seq)
      (reverse-iter seq '()))

    (define (reverse-iter seq out)
      (if (null? seq)
          out
          (reverse-iter (cdr seq)
                        (cons (car seq) out))))

    (define (list-copy obj)
      (if (pair? obj)
          (cons (car obj)
                (list-copy (cdr obj)))
          obj))

    (define (assq obj seq)
      (if (pair? seq)
          (if (eq? obj (caar seq))
              (car seq)
              (assq obj (cdr seq)))
          #f))

    (define (memq obj seq)
      (if (pair? seq)
          (if (eq? obj (car seq))
              seq
              (memq obj (cdr seq)))
          #f))

    ; ====== symbols =======================

    (define (symbol=? s1 s2 . args)
      (_symbol=? s1 (cons s2 args)))

    (define (_symbol=? s1 s*)
      (cond ((null? s*) #t)
            ((eq? s1 (car s)) (_symbol=? s1 (cdr s*)))
            (else #f)))

    ; ====== symbols =======================

    (define (string=? s1 s2 . args)
      (_string=? s1 (cons s2 args)))

    (define (_string=? s1 s*)
      (cond ((null? s*) #t)
            ((equal? s1 (car s*)) (_string=? s1 (cdr s*)))
            (else #f)))))
