#lang racket

(provide defdsslform
         code syn
         q m)
(require scribble/manual
         scribble/racket
         scribble/struct)

(define (q x)
  (elem (racketvalfont x)))

(define (m x)
  (elem (larger x)))

(define (code . codes)
  (elem #:style 'tt codes))

(define-syntax-rule (syn var)
  (code (italic (~a 'var))))

(define-syntax-rule (defdsslform form ...)
  (*defforms #f (list form ...)))

(define (*defforms kind forms)
  (define labeller (add-background-label (or kind "syntax")))
  (define labelled (labeller
                     (list (make-paragraph
                             (list (to-element (code forms)))))))
  (define table-content (list (list labelled)))
  (define table (make-table boxed-style table-content))
  (make-blockquote
    vertical-inset-style
    (list table)))
