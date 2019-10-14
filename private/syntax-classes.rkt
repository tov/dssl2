#lang racket/base

(provide var)
(require syntax/parse)

(define-syntax-class var
  #:attributes (id)
  (pattern raw:id
           #:attr id (if (eq? '_ (syntax-e #'raw))
                       #`#,(gensym '_)
                       #'raw)))
