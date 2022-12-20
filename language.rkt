#lang racket/base

(provide #%datum
         #%require
         (rename-out [top-undefined #%top])
         (all-from-out "private/operators.rkt"
                       "private/prims.rkt"
                       "private/syntax.rkt"))

(require "private/operators.rkt"
         "private/prims.rkt"
         "private/syntax.rkt"
         (for-syntax racket/base))

(define-syntax (top-undefined stx)
  (syntax-case stx ()
    [(_ . x)
     (raise-syntax-error 'dssl2
                         (format "variable ~s is undefined" (syntax-e #'x))
                         #'x)]))
