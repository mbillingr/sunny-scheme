(define-library (sunny sets)
  (export make-set
          set-add
          set-add*
          set-do*
          set-remove
          set-remove*
          set-union)

  (import (scheme base))

  (begin
    ; quick and dirty implementation of sets as a unordered list

    (define (make-set)
      '())

    (define (set-add set item)
      (cond ((null? set)
             (cons item '()))
            ((equal? (car set) item)
             set)
            (else (cons (car set)
                        (set-add (cdr set) item)))))

    (define (set-remove set item)
      (cond ((null? set)
             '())
            ((equal? (car set) item)
             (cdr set))
            (else (cons (car set)
                        (set-remove (cdr set) item)))))

    (define (set-add* set item*)
      (set-do* set-add set item*))

    (define (set-remove* set item*)
      (set-do* set-remove set item*))

    (define (set-do* func set item*)
      (if (null? item*)
          set
          (set-do* func
                   (func set (car item*))
                   (cdr item*))))

    (define (set-union set1 set2)
      (cond ((null? set1) set2)
            ((null? set2) set1)
            (else (set-add* set1 set2))))))
