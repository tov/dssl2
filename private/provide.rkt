#lang racket/base

(provide dssl-provide)
(require (for-syntax racket/base
                     syntax/parse
                     "names.rkt"))

(define-syntax (dssl-provide stx)
  (define (each-spec spec)
    (syntax-parse spec #:literals (for-syntax)
      [name:id #:when (public-method-name? #'name)
        #'(provide name)]
      [(for-syntax name:id) #:when (public-method-name? #'name)
        #'(provide (for-syntax name))]
      [_
        #'(begin)]))
  (if (eq? 'module (syntax-local-context))
    (syntax-parse stx
      [(_ spec ...)
       #`(begin
           #,@(map each-spec (syntax->list #'(spec ...))))])
    #'(begin)))

