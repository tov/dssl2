#lang racket/base

(require (only-in plot plot lines plot-font-size))
(provide (rename-out [-plot plot])
         measure)

(define (measure thunk)
  (define-values (_ cpu real gc)
    (time-apply thunk '()))
  cpu)

(define (-plot title vs)
  (parameterize ([plot-font-size 20])
    (plot (lines vs)
          #:title title
          #:x-label "measurement #"
          #:y-label "time (ms)")))
