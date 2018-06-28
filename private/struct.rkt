#lang racket/base

(provide struct-base
         struct-base?
         struct-base-struct-info
         make-struct-info
         struct-info-name
         struct-info-field-infos
         make-field-info
         field-info?
         field-info-name
         field-info-getter
         field-info-setter
         get-field-info
         write-struct)

(define-struct field-info (name getter setter))
(define-struct struct-info (name field-infos))

(define-struct struct-base (struct-info)
               #:transparent
               #:methods gen:custom-write
               [(define write-proc
                  (λ (obj port mode) (write-struct obj port)))])

(define (write-struct value port [recur (λ (v) (fprintf port "~e" v))])
  (define info (struct-base-struct-info value))
  (define first #true)
  (fprintf port "~a {" (struct-info-name info))
  (for ([field-info (in-vector (struct-info-field-infos info))])
    (if first
      (set! first #f)
      (display ", " port))
    (fprintf port "~a: " (field-info-name field-info))
    (recur ((field-info-getter field-info) value)))
  (display "}" port))

(define (get-field-info struct sym)
  (let/ec return
    (define info-vector (struct-info-field-infos
                          (struct-base-struct-info struct)))
    (for ([info (in-vector info-vector)])
      (when (eq? sym (field-info-name info))
        (return info)))
    #false))
