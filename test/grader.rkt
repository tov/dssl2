#lang racket/base

(require dssl2/grader)
(require rackunit
         (only-in racket/runtime-path define-runtime-path)
         (only-in racket/port open-output-nowhere)
         (for-syntax racket/base))

(define-runtime-path grader-tests (build-path "grader"))

(define-syntax-rule (silence body ...)
  (parameterize ([current-output-port (open-output-nowhere)]
                 [current-error-port (open-output-nowhere)])
    body ...))

(define (check-grades subject expected-successes expected-total)
  (define-values (actual-successes actual-total)
    (silence (grade-file/values (build-path grader-tests subject))))
  (check-equal? actual-successes expected-successes
                (format "~a: successes" subject))
  (check-equal? actual-total expected-total
                (format "~a: total" subject)))

(check-grades "1_of_1.rkt" 1 1)
(check-grades "2_of_2.rkt" 2 2)
(check-grades "2_of_3.rkt" 2 3)
(check-grades "0_of_0.rkt" 0 0)
(check-grades "0_of_0_crashes.rkt" 0 0)
(check-grades "0_of_0_loads_crashes.rkt" 0 0)
(check-grades "0_of_0_cheater.rkt" 0 0)
(check-grades "0_of_0_malicious.rkt" 0 0)
(check-grades "1_of_2_out_of_memory.rkt" 1 2)
(parameterize ([grader-time-limit 10])
  (check-grades "0_of_0_infinite_loop.rkt" 0 0))
(check-grades "4_of_4_prints.rkt" 4 4)

(define EPSILON 1E-5)

(define (check-grade/numeric subject expected)
  (check-= (silence (grade-file (build-path grader-tests subject)))
           expected
           EPSILON
           (format "~a: numeric grade" subject)))

(check-grade/numeric "1_of_1.rkt" 1/1)
(check-grade/numeric "2_of_2.rkt" 2/2)
(check-grade/numeric "2_of_3.rkt" 2/3)
(check-grade/numeric "0_of_0.rkt" 0)
