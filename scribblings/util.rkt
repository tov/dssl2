#lang racket/base

(provide intersperse)

(define (intersperse x ys)
  (cond
    [(null? ys) '()]
    [(null? (cdr ys)) ys]
    [else (cons (car ys) (cons x (intersperse x (cdr ys))))]))
