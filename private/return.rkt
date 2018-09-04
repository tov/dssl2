#lang racket/base

(provide dssl-return with-return)
(require (for-syntax racket/base
                     "errors.rkt")
         racket/stxparam
         syntax/parse/define)

; We define return (for lambda) as a syntax parameter, and then
; syntax-parameterize it inside dssl-lambda.
(define-syntax-parameter
  dssl-return
  (lambda (stx)
    (syntax-error stx "use of return keyword not in a function")))

(define-simple-macro (with-return expr:expr)
  (let/ec return-f
    (syntax-parameterize
      ([dssl-return (syntax-rules ()
                      [(_)        (return-f (void))]
                      [(_ result) (return-f result)])])
      expr)))

