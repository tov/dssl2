#lang racket

(provide defexpform defexpforms defsmplform defcmpdform
         defconstform
         defprocform defprocforms
         c syn
         q m
         dssl2block code)
(require scribble/manual
         scribble/racket
         scribble/struct
         (prefix-in scribble: scribble/manual))

(define (q x)
  (elem (racketvalfont x)))

(define (m x)
  (elem (larger x)))

(define (c . codes)
  (elem #:style 'tt codes))

(define-syntax-rule (syn var)
  (c (italic (~a 'var))))

(define-syntax-rule (defexpform chunk ...)
  (*defforms "expr" (list (list chunk ...))))

(define-syntax-rule (defexpforms form ...)
  (*defforms "expr" (list form ...)))

(define-syntax-rule (defsmplform chunk ...)
  (*defforms "simple" (list (list chunk ...))))

(define-syntax-rule (defcmpdform chunk ...)
  (*defforms "compound" (list (list chunk ...))))

(define-syntax-rule (defconstform name chunk ...)
  (*defforms "constant" (list (list (defidform/inline name) ": " chunk ...))))

(define-syntax-rule (defprocform name chunk ...)
  (*defforms "procedure" (list (list (defidform/inline name) chunk ...))))

(define-syntax-rule (defprocforms [name chunk ...] ...)
  (*defforms "procedure"
             (list (list (defidform/inline name) chunk ...) ...)))

(define (*defforms kind forms)
  (define labeller (add-background-label (or kind "syntax")))
  (define (make-cell form)
    (list (make-paragraph (list (to-element (c form))))))
  (define table-content
    (cons
      (list (labeller (make-cell (first forms))))
      (map list (map make-cell (rest forms)))))
  (define table (make-table boxed-style table-content))
  (make-blockquote
    vertical-inset-style
    (list table)))

(define-syntax-rule (dssl2block str-expr ...)
  (codeblock
    #:keep-lang-line? #f
    "#lang dssl2\n" str-expr ...))

(define-syntax-rule (code str-expr ...)
  (scribble:code #:lang "dssl2" str-expr ...))
