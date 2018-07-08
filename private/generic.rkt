#lang racket/base

(provide generic-base
         generic-base?
         generic-base-name
         generic-base-instantiate
         generic-class-apply
         square-bracket-lambda)

(require syntax/parse/define)
(require (only-in racket/contract/base
                  contract-name))
(require (only-in racket/contract/combinator
                  prop:contract
                  build-contract-property))
(require (for-syntax racket/base))

; This is for generic things that can be instantiated
; with square brackets.
(struct generic-base (name instantiate))

; Generic classes are functions with optional square-bracket
; arguments. That is, they can be instantiated or directly
; applied.
(struct generic-class generic-base (apply)
  #:property prop:procedure
  (struct-field-index apply))

; Generic contracts can be instantiated or used as contracts
; directly.
(struct generic-contract generic-base (first-order late-neg-projection)
  #:property prop:contract
  (build-contract-property
    #:name generic-base-name
    #:first-order (λ (v) (generic-contract-first-order v))
    #:late-neg-projection (λ (v) (generic-contract-late-neg-projection v))))

(define (format-generic f xs)
  (define port (open-output-string))
  (fprintf port "~a[~a" f (contract-name (car xs)))
  (for ([xi (in-list (cdr xs))])
    (fprintf port ", ~a" (contract-name xi)))
  (fprintf port "]")
  (string->symbol (get-output-string port)))

(define-syntax (square-bracket-lambda stx)
  (syntax-parse stx
    [(_ name:id
        ()
        (formal:id ...)
        body:expr ...+)
     #'(λ (formal ...) body ...)]
    [(_ name:id
        ([opt-formal:id default:expr] ...+)
        (formal:id ...)
        body:expr ...+)
     #'(let ([instantiate
               (λ (opt-formal ...)
                  (procedure-rename
                    (λ (formal ...)
                       body ...)
                    (format-generic 'name (list opt-formal ...))))])
         (generic-class 'name
                        instantiate
                        (instantiate default ...)))]))
