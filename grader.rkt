#lang racket/base

(provide grade-file)

(require (only-in racket/sandbox
                  make-evaluator))

(define (grade-file subject)
  ((make-evaluator 'racket/base
                   `(require (submod ,subject test-info))
                   #:allow-read `("/"))
   '(get-test-info)))

