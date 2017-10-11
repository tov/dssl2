#lang racket/base

(provide grade-file
         grade-file/values
         grader-memory-limit
         grader-time-limit)

(require (only-in racket/sandbox
                  make-evaluator
                  sandbox-eval-limits
                  sandbox-memory-limit))

(define grader-time-limit (make-parameter 10))
(define grader-memory-limit (make-parameter 256))

(define (grade-file/values subject)
  (define (handler exn)
    (fprintf (current-error-port) "ERROR WHILE TESTING: ~e~n" exn)
    (values 0 0))
  (with-handlers ([exn:fail? handler])
    (call-with-input-file subject
      (λ (port)
        (unless (regexp-match? #px"^#lang dssl2 *\n" port)
          (error "Not a #lang dssl2 file!")))
      #:mode 'text)
    (parameterize ([sandbox-eval-limits (list (grader-time-limit)
                                              (grader-memory-limit))]
                   [sandbox-memory-limit (grader-memory-limit)])
      ((make-evaluator 'racket/base
                       `(require (submod ,subject test-info))
                       #:allow-read `("/"))
       '(get-test-info)))))

(define (grade-file subject)
  (define-values (passed total) (grade-file/values subject))
  (if (zero? total)
      0
      (exact->inexact (/ passed total))))
