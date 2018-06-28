#lang racket/base

(provide struct-base struct-base? struct-base-struct-info
         make-struct-info struct-info-name struct-info-field-infos
         make-field-info field-info?
         field-info-name field-info-getter field-info-setter
         get-field-info)

(define-struct field-info (name getter setter))
(define-struct struct-info (name field-infos))

(define-struct struct-base (struct-info)
               #:transparent)

(define (get-field-info struct sym)
  (let/ec return
    (define info-vector (struct-info-field-infos
                          (struct-base-struct-info struct)))
    (for ([info (in-vector info-vector)])
      (when (eq? sym (field-info-name info))
        (return info)))
    #false))
