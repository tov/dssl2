#lang racket/base

(provide dssl-provide)
(require (for-syntax racket/base
                     syntax/parse
                     "names.rkt"))

(define-syntax (provide1 stx)
  (syntax-parse stx #:literals (for-syntax)
    [(_ name:id) #:when (public-method-name? #'name)
      #'(provide name)]
    [(_ (for-syntax name:id)) #:when (public-method-name? #'name)
      #'(provide (for-syntax name))]
    [_
      #'(begin)]))

(define-syntax (dssl-provide stx)
  (if (eq? 'module (syntax-local-context))
    (syntax-parse stx
      [(_ spec ...)
       #'(begin
           (provide1 spec)
           ...)])
    #'(begin)))

