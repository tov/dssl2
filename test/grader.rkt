#lang racket/base

(require dssl2/grader)
(require rackunit
         syntax/parse/define)

(define (check-grades subject expected-successes expected-total)
  (define-values (actual-successes actual-total)
    (grade-file subject))
  (check-equal? actual-successes expected-successes
                (format "~a: successes" subject))
  (check-equal? actual-total expected-total
                (format "~a: total" subject)))

(check-grades "grader/1_of_1.rkt"
              1 1)
(check-grades "grader/2_of_2.rkt"
              2 2)
(check-grades "grader/2_of_3.rkt"
              2 3)