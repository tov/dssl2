#lang racket

(provide dssl-print)
(require (only-in dssl2/language
                  dssl-struct? dssl-struct-name dssl-struct-fields
                  dssl-field-name dssl-field-value))

(define-struct seen (table [count #:mutable]))

(define (dssl-print v)
  (unless (void? v)
    (print-value v (seen (make-hasheq) 0))
    (newline)))

(define (print-value v seen)
  (cond
    [(hash-has-key? (seen-table seen) v)
     (print-cycle (hash-ref (seen-table seen) v) v seen)]
    [(number? v)           (print-number v)]
    [(string? v)           (write v)]
    [(vector? v)           (hash-set! (seen-table seen) v #false)
                           (print-vector v seen)]
    [(dssl-struct? v)      (hash-set! (seen-table seen) v #false)
                           (print-struct v seen)]
    [(procedure? v)        (write v)]
    [else                  (print v)]))

(define (print-cycle ref v seen)
  (cond
    [ref        (display "#")
                (display ref)
                (display "#")]
    [else       (define ref (seen-count seen))
                (hash-set! (seen-table seen) v ref)
                (set-seen-count! seen (add1 ref))
                (print-cycle ref v seen)]))

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
