#lang racket/base

(provide struct-base struct-base? struct-base-struct-info
         make-struct-info struct-info-name struct-info-field-infos
         make-field-info field-info?
         field-info-name field-info-getter field-info-setter)

(define-struct field-info (name getter setter))
(define-struct struct-info (name field-infos))

(define-struct struct-base (struct-info)
               #:transparent)

