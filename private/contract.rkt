#lang racket/base

(provide AnyC
         ensure-contract)
(require (only-in racket/contract
                  any/c
                  contract?
                  flat-named-contract)
         "errors.rkt")

(define AnyC (flat-named-contract 'AnyC any/c))

(define (ensure-contract/fn srclocs who contract)
  (if (contract? contract)
    contract
    (raise-runtime-error #:context srclocs
                         "%s: expected a contract\n got: %p"
                         who contract)))

(define-syntax-rule (ensure-contract who contract)
  (ensure-contract/fn (capture-context contract) who contract))

