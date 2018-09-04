#lang racket/base

(provide syntax-length)

(define (syntax-length stx0)
  (define (loop stx acc)
    (cond
      [(pair? stx)
       (loop (cdr stx) (add1 acc))]
      [(and (syntax? stx) (pair? (syntax-e stx)))
       (loop (cdr (syntax-e stx)) (add1 acc))]
      [else 0]))
  (loop stx0 0))

