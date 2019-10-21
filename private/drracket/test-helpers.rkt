#lang racket/base

(provide line
         table
         define-text%-check
         (all-from-out rackunit))

(require (only-in racket/class new send)
         (only-in racket/format ~a)
         (only-in racket/gui text%)
         (only-in rackunit define-check fail-check)
         syntax/parse/define)

(define (line n [indent 0])
  (~a (make-string indent #\space)
      (make-string (- n indent 1) #\o)
      #\newline))
  
(define-simple-macro (table (same:expr ...+) [diff:expr ...] ...)
  (begin (same ... diff ...) ...))


(define-simple-macro
  (define-text%-check (check:id text:id args:id ...) actual-e:expr)
  (define-check (check text args ... expected)
    (text-check/proc (λ (text) actual-e) text expected)))

(define (text-check/proc actual-proc contents expected)
  (define text (new text%))
  (send text insert contents)
  (define actual (actual-proc text))
  (unless (equal? actual expected)
    (fail-check
     (format "got ~s where ~s expected" actual expected))))