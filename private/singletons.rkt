#lang racket/base

(provide dssl-True
         dssl-False
         dssl-None
         falsy?
         truthy?)

(define dssl-True #t)
(define dssl-False #f)
(define dssl-None (void))

(define (falsy? v)
  (or (eq? v dssl-False) (eq? v dssl-None)))

(define (truthy? v)
  (not (falsy? v)))


