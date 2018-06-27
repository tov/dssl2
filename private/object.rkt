#lang racket/base

(provide object-base object-base?
         object-base-object-info object-base-contract-params
         make-object-info
         object-info-name object-info-interface object-info-method-infos
         make-method-info method-info?
         method-info-name method-info-getter
         get-method-vector)

(define-struct method-info (name getter))
(define-struct object-info (name interface method-infos))

(define-struct object-base (object-info contract-params)
               #:transparent)

(define (get-method-vector obj)
  (for/vector ([method-info (object-info-method-infos
                              (object-base-object-info obj))])
    (symbol->string (method-info-name method-info))))

