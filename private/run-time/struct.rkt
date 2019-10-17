#lang racket/base

(provide struct-has-field?
         struct-ref
         struct-set!
         struct-name
         struct-fields
         write-struct
         ;
         struct-info
         struct-base
         struct-base?
         struct-base-name
         struct-base-fields
         struct-base-data)

(struct struct-info [name fields ctcs])
(struct struct-base [info data] #:transparent)

(define (struct-has-field? obj field)
  (hash-has-key? (struct-base-data obj) field))

(define (struct-ref obj field on-error)
  (hash-ref (struct-base-data obj) field on-error))

(define (struct-set! obj field value on-error)
  (hash-update! (struct-base-data obj)
                field
                (λ (_) value)
                on-error))

(define (struct-name obj)
  (struct-info-name (struct-base-info obj)))

(define (struct-fields obj)
  (struct-info-fields (struct-base-info obj)))

(define (write-struct obj port recur)
  (define first #true)
  (fprintf port "~a {" (struct-name obj))
  (for ([field (in-vector (struct-fields obj))])
    (if first
      (set! first #f)
      (display ", " port))
    (fprintf port "~a: " field)
    (recur (struct-ref obj field)))
  (display "}" port))

