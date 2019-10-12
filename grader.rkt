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
                  exn:fail:resource?
                  exn:fail:resource-resource)
         (only-in racket/string
                  string-split))

(define grader-read-directories
  (make-parameter '("/Applications/Racket")))
(define grader-time-limit (make-parameter 30))
(define grader-memory-limit (make-parameter 256))

(define (eprint/boxed msg
                      #:char [char #\*]
                      #:thickness [thickness 3]
                      #:surround? [surround? #t])
  (define lines    (string-split msg "\n" #:trim? #f))
  (define width    (foldl max 0 (map string-length lines)))
  (define chars    (make-string thickness char))
  (define heading  (make-string (+ width 2 (* 2 thickness)) char))
  (when surround? (eprintf "~a\n" heading))
  (for ([line lines])
    (eprintf "~a ~a ~a\n"
             chars
             (~a line #:min-width width #:align 'left)
             chars))
  (when surround? (eprintf "~a\n" heading)))

(define (grade-file/values subject)
  (define (test-error . args)
    (eprintf "\n")
    (eprint/boxed (apply ~a "ERROR WHILE TESTING\n  " args))
    (eprintf "\n")
    (values 0 0))
  (define (handle-other-fail exn)
    (test-error "~s" (exn-message exn)))
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
        (unless (regexp-match? #px"^#lang dssl2 *\n" port)
          (error "Not a #lang dssl2 file!")))
      #:mode 'text)
    (parameterize ([current-output-port (current-error-port)]
                   [sandbox-eval-limits (list (grader-time-limit)
                                              (grader-memory-limit))]
                   [sandbox-memory-limit (grader-memory-limit)]
                   [sandbox-output current-output-port]
                   ; Redirecting error to output seems to prevent messages
                   ; from being reordered.
                   [sandbox-error-output current-output-port])
      ((make-evaluator 'racket/base
                       `(require (file ,(~a subject))
                                 (submod (file ,(~a subject)) test-info))
                       #:allow-read (cons
                                      (current-directory)
                                      (grader-read-directories)))
       '(values passed-tests total-tests)))))

(define (grade-file subject)
  (define-values (passed total) (grade-file/values subject))
  (exact->inexact (if (zero? total) 0 (/ passed total))))

