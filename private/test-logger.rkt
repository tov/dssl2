#lang racket/base

(provide dssl-test-logger
         log-test-result)

(require racket/format)

(define-logger dssl-test)

(define counter 1)
(define (log-test-result name passed?)
  (when (log-level? dssl-test-logger 'info)
    (log-message dssl-test-logger
                 'info
                 (string-append "test `" (~a "(#" counter ") " name) "` " (if passed? "passed" "failed"))
                 (cons (~a "(#" counter ") " name) passed?))
    (set! counter (add1 counter))))
