#lang racket/base

(provide syntax-length)
(require syntax/stx)

(define (syntax-length stx0)
  (let loop ([stx stx0] [acc 0])
    (if (stx-pair? stx)
      (loop (stx-cdr stx) (add1 acc))
      acc)))
