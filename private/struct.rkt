#lang racket

(provide dssl-write-struct
         make-field-info field-info?
         field-info-name field-info-getter field-info-setter)

(define-struct field-info (name getter setter))

(define (dssl-write-struct name info struct port mode)
  (define first #t)
  (fprintf port "~a {" name)
  (for ([field-vec (in-vector info)])
    (if first
      (set! first #f)
      (display ", " port))
    (fprintf port "~a: " (field-info-name field-vec))
    (define field-value ((field-info-getter field-vec) struct))
    (case mode
      [(#t) (write field-value port)]
      [(#f) (display field-value port)]
      [else (print field-value port mode)]))
  (display "}" port))

