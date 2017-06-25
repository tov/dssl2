#lang racket

(require racket/runtime-path
         file/glob)

(define-runtime-path TESTS (build-path 'same))

(define pass-count 0)
(define fail-count 0)

(for ([test-file (glob (build-path TESTS "dssl2" "*.rkt"))])
  (define should-fail (regexp-match #rx"fail[.]rkt$" test-file))
  (define result
    (with-handlers ([exn:fail?
                      (λ (e)
                         (if should-fail
                           'okay
                           (format "Test ~a threw ~s" test-file e)))])
      (dynamic-require test-file #f)
      (if should-fail
        (format "Test ~a expected to fail but didn't" test-file)
        'okay)))
  (if (eq? result 'okay)
    (set! pass-count (add1 pass-count))
    (begin
      (set! fail-count (add1 fail-count))
      (displayln result (current-error-port)))))

(printf "Total tests:  ~a~n" (+ pass-count fail-count))
(printf "Tests passed: ~a~n" pass-count)
(printf "Tests failed: ~a~n" fail-count)

(unless (zero? fail-count)
  (exit 1))
