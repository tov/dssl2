#lang racket/base

(module+ test-info
  (provide get-test-info)
  (define (get-test-info)
    (values 9 10)))

"
#lang dssl2
"