#lang racket

(provide dssl-write-struct
         struct-base struct-base? struct-base-struct-info
         make-struct-info struct-info-field-infos
         make-field-info field-info?
         field-info-name field-info-getter field-info-setter)

(define-struct field-info (name getter setter))
(define-struct struct-info (name field-infos))

(define-struct struct-base (struct-info)
               #:transparent)

(define (dssl-write-struct struct port mode)
  (define info (struct-base-struct-info struct))
  (define first #t)
  (fprintf port "~a {" (struct-info-name info))
  (for ([field-vec (in-vector (struct-info-field-infos info))])
    (if first
      (set! first #f)
      (display ", " port))
    (fprintf port "~a: " (field-info-name field-vec))
    (define field-value ((field-info-getter field-vec) struct))
    (cond
      [(eq? #t mode)     (write field-value port)]
      [(eq? #f mode)     (display field-value port)]
      [(procedure? mode) (mode field-value port)]
      [else              (print field-value port mode)]))
  (display "}" port))

