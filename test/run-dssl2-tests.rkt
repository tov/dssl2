#lang racket

(require racket/runtime-path
         file/glob)

(define-runtime-path TESTS (build-path 'same))

(for ([test-file (glob (build-path TESTS "dssl2" "*.rkt"))])
     (load test-file))