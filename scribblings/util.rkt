#lang racket

(provide defexpform defsmplform defcmpdform
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

(define-syntax-rule (defexpform form ...)
  (*defforms "expr" (list form ...)))

(define-syntax-rule (defsmplform form ...)
  (*defforms "simple" (list form ...)))

(define-syntax-rule (defcmpdform form ...)
  (*defforms "compound" (list form ...)))

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
