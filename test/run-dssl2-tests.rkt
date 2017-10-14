#lang racket/base

(require racket/runtime-path
         file/glob
         "../private/printer.rkt"
         (for-syntax racket/base))

(define-runtime-path TESTS (build-path 'same))

(define pass-count 0)
(define fail-count 0)

(for ([test-file (glob (build-path TESTS "dssl2" "*.rkt"))])
  (define-values (_1 short-name _2) (split-path test-file))
  (printf "~a... " short-name)
  (define should-fail (regexp-match? #rx"fail[.]rkt$" test-file))
  (define result
    (with-handlers ([exn:fail?
                      (λ (e)
                         (if should-fail
                           'okay
                           (format "threw ~a" e)))])
      (parameterize ([global-port-print-handler dssl-print])
        (dynamic-require test-file #f)
        (if should-fail
          (format "expected to throw but didn't")
          'okay))))
  (cond
    [(eq? result 'okay)
     (set! pass-count (add1 pass-count))
     (displayln "passed.")]
    [else
     (set! fail-count (add1 fail-count))
     (displayln "failed.")
     (eprintf "  Failed test details: ~a~n" result)]))

(newline)
(printf "Total tests:  ~a~n" (+ pass-count fail-count))
(printf "Tests passed: ~a~n" pass-count)
(printf "Tests failed: ~a~n" fail-count)

(unless (zero? fail-count)
  (exit 1))
