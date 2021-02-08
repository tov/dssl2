#lang racket/base

(provide lambda/cases
         define/cases
         def-conj)

(require syntax/parse/define
         (for-syntax racket/base
                     (only-in racket/sequence in-syntax)))

(define-for-syntax (generate-temporaries/2 stx)
  (for/list ([params (in-syntax stx)])
    (generate-temporaries params)))

(define-syntax-parser lambda/cases
  [(_ [(formal:id ...) body:expr ...+])
   #'(λ (formal ...) body ...)]
  [(_ [(formal:id ...) body:expr ...+] ...+)
   #'(case-lambda [(formal ...) body ...] ...)])

(define-syntax-parser define/cases
  [(_ name:id [(formal:id ...) body:expr ...+] ...+)
   (with-syntax
       ([((actual ...) ...)
         (generate-temporaries/2 #'((formal ...) ...))])
     #'(begin
         (define proc-version
           (procedure-rename
            (lambda/cases [(formal ...) (name formal ...)] ...)
            'name))
         (define-syntax-parser name
           [(_ actual ...) #'(let ([formal actual] ...) body ...)]
           ...
           [_:id           #'proc-version])))])

(define-syntax-parser def-conj
  [(_ name:id pred?:expr)
   #'(define name (procedure-rename pred? 'name))]
  [(_ name:id pred?:expr ...+)
   #'(define (name x) (and (pred? x) ...))])

