#lang racket/base

; Need to turn off #lang check for this test to actually matter.

(module+ test-info
  (provide get-test-info)
  (define (get-test-info)
    (values 9 10)))

(call-with-output-file "some-file.out"
  (λ (port)
    (display "Hello, world!\n" port)))