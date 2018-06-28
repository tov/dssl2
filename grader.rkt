#lang racket/base

(provide grade-file
         grade-file/values
         grader-memory-limit
         grader-time-limit)

(require (only-in racket/format
                  ~a)
         (only-in racket/sandbox
                  make-evaluator
                  sandbox-eval-limits
                  sandbox-memory-limit
                  sandbox-output
                  sandbox-error-output))

(define grader-time-limit (make-parameter 30))
(define grader-memory-limit (make-parameter 256))

(define (grade-file/values subject)
  (define (handler exn)
    (fprintf (current-error-port) "*** ERROR WHILE TESTING: ~a~n"
             (exn-message exn))
    (values 0 0))
  (with-handlers ([exn:fail? handler])
    (call-with-input-file subject
      (λ (port)
        (unless (regexp-match? #px"^#lang dssl2 *\n" port)
          (error "Not a #lang dssl2 file!")))
      #:mode 'text)
    (parameterize ([sandbox-eval-limits (list (grader-time-limit)
                                              (grader-memory-limit))]
                   [sandbox-memory-limit (grader-memory-limit)]
                   [sandbox-output current-output-port]
                   ; Redirecting error to output seems to prevent messages
                   ; from being reordered.
                   [sandbox-error-output current-output-port])
      ((make-evaluator 'racket/base
                       `(require (file ,(~a subject))
                                 (submod (file ,(~a subject)) test-info))
                       #:allow-read (list
                                      "/Applications/Racket"
                                      "/Applications/Racket v6.12"
                                      (current-directory)))
       '(values passed-tests total-tests)))))

(define (grade-file subject)
  (define-values (passed total) (grade-file/values subject))
  (if (zero? total)
      0
      (exact->inexact (/ passed total))))
