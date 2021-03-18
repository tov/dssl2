#lang racket/base

(provide grade-file
         grade-file/values
         grader-read-directories
         grader-memory-limit
         grader-time-limit)

(require (only-in racket/format
                  ~a)
         (only-in racket/sandbox
                  make-evaluator
                  sandbox-eval-limits
                  sandbox-memory-limit
                  sandbox-output
                  sandbox-error-output
                  sandbox-make-code-inspector
                  exn:fail:resource?
                  exn:fail:resource-resource)
         (only-in racket/string
                  string-split))

(define grader-read-directories (make-parameter '()))
(define grader-time-limit (make-parameter 600))
(define grader-memory-limit (make-parameter 256))

(define (eprint/hanging head msg [char #\*])
  (define rule (make-string 80 char))
  (eprintf "~a\n~a ~a\n" rule char head)
  (for ([line (string-split msg "\n" #:trim? #f)])
    (eprintf "~a   ~a\n" char line))
  (eprintf "~a\n" rule))

(define (grade-file/values subject)
  (define (test-error . args)
    (eprintf "\n")
    (eprint/hanging "ERROR WHILE TESTING:" (apply ~a args))
    (eprintf "\n")
    (values 0 0))
  (define (handle-other-fail exn)
    (test-error (exn-message exn)))
  (define (handle-resource-fail exn)
    (case (exn:fail:resource-resource exn)
      [(memory)
       (test-error "Memory limit exceeded ("
                   (grader-memory-limit)
                   " MB)")]
      [(time deep-time)
       (test-error "Possible infinite loop detected"
                   "\n  ("
                   (grader-time-limit)
                   " second time limit exceeded)")]
      [else
        (handle-other-fail exn)]))
  (with-handlers
    ([exn:fail:resource? handle-resource-fail]
     [exn:fail?          handle-other-fail])
    (call-with-input-file subject
      (λ (port)
        (unless (regexp-match? #px"^#lang dssl2\\s" port)
          (error "Not a #lang dssl2 file!")))
      #:mode 'text)
    (parameterize ([current-output-port (current-error-port)]
                   [sandbox-eval-limits (list (grader-time-limit)
                                              (grader-memory-limit))]
                   [sandbox-make-code-inspector
                     (let ([inspector (current-code-inspector)])
                       (λ () inspector))]
                   [sandbox-memory-limit (grader-memory-limit)]
                   [sandbox-output current-output-port]
                   ; Redirecting error to output seems to prevent messages
                   ; from being reordered.
                   [sandbox-error-output current-output-port])
      ((make-evaluator
         'racket/base
         `(require (file ,(~a subject))
                   (submod (file ,(~a subject)) test-info))
         #:allow-read (list*
                        (current-directory)
                        (find-system-path 'pref-dir)  ;; why?
                        (grader-read-directories)))
       '(if (zero? possible-points)
          (values passed-tests total-tests)
          (values actual-points possible-points))))))

(define (grade-file subject)
  (define-values (passed total) (grade-file/values subject))
  (exact->inexact (if (zero? total) 0 (/ passed total))))

