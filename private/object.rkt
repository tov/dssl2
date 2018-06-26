#lang racket/base

(provide object-base object-base? object-base-object-info
         make-object-info object-info-name object-info-method-infos
         make-method-info method-info?
         method-info-name method-info-getter
         get-method-vector)

(define-struct method-info (name getter))
(define-struct object-info (name method-infos))

(define-struct object-base (object-info)
               #:transparent)

(define (get-method-vector obj)
  (for/vector ([method-info (object-info-method-infos
                              (object-base-object-info obj))])
    (symbol->string (method-info-name method-info))))

