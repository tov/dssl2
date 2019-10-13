#lang racket/base

(require (for-syntax racket/base)
         (only-in racket/file
                  delete-directory/files)
         (only-in racket/runtime-path
                  define-runtime-path)
         (only-in file/glob
                  glob)
         (only-in "../private/printer.rkt"
                  dssl-print))

(define-runtime-path TESTS (build-path 'same))

(define temp-directory
  (build-path (find-system-path 'temp-dir) "dssl2-test"))

(define (slow-test? file)
  (regexp-match #rx"-slow(-[a-z]+)*[.]rkt$" file))

(define (fail-test? file)
  (regexp-match #rx"-fail(-[a-z]+)*[.]rkt$" file))

(define (run-one-test short-name test-file)
  (define-values (handle-exn no-exn-result)
    (if (fail-test? test-file)
      (values (λ (_e) #f)
              "expected to throw but didn’t")
      (values (λ (e) (format "threw ~a" e))
              #f)))
  (with-handlers ([exn:fail? handle-exn])
    (parameterize ([global-port-print-handler dssl-print]
                   [current-directory temp-directory])
      (dynamic-require test-file #f)
      no-exn-result)))

(define (printf/flush fmt . args)
  (apply printf fmt args)
  (flush-output))

(define (run-all-tests [fast? #t])
  (for/fold ([pass-count 0] [fail-count 0])
    ([test-file (glob (build-path TESTS "dssl2" "*.rkt"))])
    (define-values (_1 short-name _2) (split-path test-file))
    (cond
      [(and fast? (slow-test? test-file))
       (printf "skipping ~a.~n" short-name)
       (values pass-count fail-count)]
      [(begin
         (printf/flush "running ~a... " short-name)
         (run-one-test short-name test-file))
       =>
       (λ (message)
          (printf/flush "failed.~n")
          (eprintf " * failed test details: ~a~n" message)
          (values pass-count (add1 fail-count)))]
      [else
        (printf "passed.~n")
        (values (add1 pass-count) fail-count)])))

(define (print-results pass-count fail-count)
  (newline)
  (printf "Total tests:  ~a~n" (+ pass-count fail-count))
  (printf "Tests passed: ~a~n" pass-count)
  (printf "Tests failed: ~a~n" fail-count))

(define-syntax with-fresh-temp-directory
  (syntax-rules ()
    [(_ body ...)
     (dynamic-wind
       (λ ()
          (delete-directory/files temp-directory #:must-exist? #f)
          (make-directory temp-directory))
       (λ () body ...)
       (λ ()
          (delete-directory/files temp-directory #:must-exist? #f)))]))

(define (main fast?)
  (with-fresh-temp-directory
    (define-values (pass-count fail-count) (run-all-tests fast?))
    (print-results pass-count fail-count)
    (unless (zero? fail-count) (exit 1))))

(define (process-arguments args)
  (not (equal? args (vector "-a"))))

(main (process-arguments (current-command-line-arguments)))

