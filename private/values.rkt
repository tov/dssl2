#lang racket

(provide dssl-write-struct
         make-field-info field-info?
         field-info-name field-info-getter field-info-setter
         make-vec vec? unvec)

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

(define (write-vec vec port mode)
  (let ([recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (λ (p port) (print p port mode))])])
    (display "[" port)
    (define first #t)
    (for ([element (in-vector (vec-contents vec))])
      (if first
        (set! first #f)
        (display ", " port))
      (recur element port))
    (display "]" port)))

(define-struct vec [contents]
               #:transparent
               #:methods gen:custom-write
               [(define write-proc write-vec)])

(define (unvec v)
  (if (vec? v)
    (vec-contents v)
    (error (format "Runtime error: expected vector, got ~s instead." v))))

