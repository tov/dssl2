#lang racket/base

(provide %
         **
         ==
         !=
         ===
         !==
         !
         &
         \|
         ^
         ~
         -              ; from Racket
         *              ; from Racket
         /
         +
         <
         >
         <=
         >=
         >>
         <<
         ; syntax
         and            ; from Racket
         or)            ; from Racket

(require "prims.rkt"
         "errors.rkt"
         "equal.rkt")
(require (prefix-in racket: racket/base))
(require (only-in racket/contract/region
                  define/contract)
         (only-in racket/contract/base
                  ->))

(define (% a b)
  (modulo a b))

(define (** a b)
  (expt a b))

(define (! a)
  (not a))

(define (& a b)
  (bitwise-and a b))

(define (\| a b)
  (bitwise-ior a b))

(define (^ a b)
  (bitwise-xor a b))

(define (~ a)
  (bitwise-not a))

(define/contract (/ a b)
  (-> num? num? num?)
  (cond
    [(and (int? a) (int? b))
     (quotient a b)]
    [else
     (racket:/ a b)]))

(define +
  (case-lambda
    [(a)
     (cond
       [(number? a)
        a]
       [else
         (runtime-error
           "unary + expects a number, but given ‘~e’"
           a)])]
    [(a b)
     (cond
       [(and (number? a) (number? b))
        (racket:+ a b)]
       [(string? a)
        (cond
          [(string? b) (string-append a b)]
          [else        (format "~a~e" a b)])]
       [(string? b)
        (format "~e~a" a b)]
       [else
         (runtime-error
           "+ expects 2 numbers or at least 1 string, but given ~e and ~e"
           a b)])]))

(define (== a b)
  (dssl-equal? a b))

(define (!= a b)
  (not (== a b)))

(define (=== a b)
  (eq? a b))

(define (!== a b)
  (not (=== a b)))

(define-syntax-rule (make-comparison name string-cmp number-cmp)
  (define (name a b)
    (cond
      [(and (string? a) (string? b))
       (string-cmp a b)]
      [(and (number? a) (number? b))
       (number-cmp a b)]
      [else
        (runtime-error
          "Comparator ~a only applies to 2 strings or 2 numbers"
          'number-cmp)])))

(make-comparison < string<? racket:<)
(make-comparison > string>? racket:>)
(make-comparison <= string<=? racket:<=)
(make-comparison >= string>=? racket:>=)

(define (<< n m)
  (arithmetic-shift n m))

(define (>> n m)
  (arithmetic-shift n (- m)))

