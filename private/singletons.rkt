#lang racket/base

(provide dssl-True
         dssl-False
         dssl-None
         falsy?
         truthy?
         falsy->false
         truthy-cond)

(require (for-syntax racket/base)
         syntax/parse/define)

(define dssl-True #t)
(define dssl-False #f)
(define dssl-None (void))

(define (falsy? v)
  (or (eq? v dssl-False) (eq? v dssl-None)))

(define (truthy? v)
  (not (falsy? v)))

(define (falsy->false v)
  (and (truthy? v) v))

(begin-for-syntax
  (define-syntax-class truthy-clause
    #:literals (else)
    (pattern [els:else body:expr ...+]
             #:attr translation #'[els body ...])
    (pattern [test:expr bodyish:expr ...]
             #:attr translation #'[(falsy->false test) bodyish ...])))

(define-syntax-parser truthy-cond
  [(_ clause:truthy-clause ...)
   #'(cond clause.translation ...)])

