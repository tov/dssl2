#lang racket

(provide dssl-print)
(require (only-in dssl2/language
                  dssl-struct? dssl-struct-name dssl-struct-fields
                  dssl-field-name dssl-field-value))

(define-struct seen (table [count #:mutable]))

(define (dssl-print v)
  (unless (void? v)
    (define seen (make-seen (make-hasheq) 0))
    (scan v (seen-table seen))
    (print-value v seen)
    (newline)))

(define (scan v hash)
  (cond
    [(hash-has-key? hash v)     (hash-set! hash v #t)]
    [(vector? v)                (hash-set! hash v #f)
                                (for ([elt v]) (scan elt hash))]
    [(dssl-struct? v)           (hash-set! hash v #f)
                                (for ([f (dssl-struct-fields v)])
                                  (scan (dssl-field-value f) hash))]
    [else                       (void)]))

(define (print-value v seen)
  (cond
    [(and (hash-has-key? (seen-table seen) v)
          (number? (hash-ref (seen-table seen) v)))
                           (printf "#~a#" (hash-ref (seen-table seen) v))]
    [(number? v)           (print-number v)]
    [(string? v)           (write v)]
    [(vector? v)           (print-ref v seen)
                           (print-vector v seen)]
    [(dssl-struct? v)      (print-ref v seen)
                           (print-struct v seen)]
    [(procedure? v)        (write v)]
    [else                  (print v)]))

(define (print-ref v seen)
  (when (and (hash-has-key? (seen-table seen) v)
             (hash-ref (seen-table seen) v))
    (define count (seen-count seen))
    (set-seen-count! seen (add1 count))
    (hash-set! (seen-table seen) v count)
    (printf "#~a=" count)))

(define (print-number number)
  (cond
    [(= number +inf.0)  (display "inf")]
    [(= number -inf.0)  (display "-inf")]
    [(nan? number)      (display "nan")]
    [else               (display number)]))

(define (print-vector vec seen)
  (display "[")
  (print-commas (λ (v) (print-value v seen)) vec)
  (display "]"))

(define (print-struct str seen)
  (display (dssl-struct-name str))
  (display "{")
  (print-commas
    (λ (field)
       (display (dssl-field-name field))
       (display ": ")
       (print-value (dssl-field-value field) seen))
    (dssl-struct-fields str))
  (display "}"))

(define (print-commas each sequence)
  (define first #t)
  (for ([value sequence])
    (if first
      (set! first #f)
      (display ", "))
    (each value)))
