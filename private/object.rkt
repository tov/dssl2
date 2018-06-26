#lang racket/base

(provide object-base object-base? object-base-object-info
         make-object-info object-info-name object-info-method-infos
         make-method-info method-info?
         method-info-name method-info-getter)

(define-struct method-info (name getter))
(define-struct object-info (name method-infos))

(define-struct object-base (object-info)
               #:transparent)
