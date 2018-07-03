#lang racket/base

(require (only-in racket/list
                  first
                  rest)
         (only-in racket/format
                  ~a)
         (for-syntax racket/base))

(provide defexpform defexpforms defsmplform defcmpdform defcmpdforms
         defclassform linkclass Linkclass
         defconstform
         defprocform defprocforms
         defmethform defmethforms
         redefidform/inline
         c syn syn_
         q m t
         dssl2block code
         indent)
(require scribble/manual
         scribble/racket
         scribble/struct
         (prefix-in scribble: scribble/manual)
         (for-syntax syntax/parse))

(define (q x)
  (elem (racketvalfont x)))

(define (t x)
  (elem #:style 'italic (racketvalfont x)))

(define (m x)
  (elem (larger (larger x))))

(define (c . codes)
  (elem #:style 'tt codes))

(define-syntax-rule (syn var)
  (c (italic (~a 'var))))

(define-syntax-rule (syn_ var sub ...)
  (c (italic (~a 'var) (subscript sub ...))))

(define-syntax-rule (defexpform chunk ...)
  (*defforms "expr" (list (list chunk ...))))

(define-syntax-rule (defexpforms form ...)
  (*defforms "expr" (list form ...)))

(define-syntax-rule (defsmplform chunk ...)
  (*defforms "simple" (list (list chunk ...))))

(define-syntax-rule (defcmpdform chunk ...)
  (*defforms "compound" (list (list chunk ...))))

(define-syntax-rule (defcmpdforms (chunk ...) ...)
  (*defforms "compound" (list (list chunk ...) ...)))

(define-syntax-rule (defclassform name)
  (subsubsection #:tag (format "class:~a" 'name)
                 "Class "
                 (c (symbol->string 'name))))

(define-syntax-rule (linkclass name)
  (secref (format "class:~a" 'name)))

(define-syntax-rule (Linkclass name)
  (Secref (format "class:~a" 'name)))

(define-syntax-rule (defconstform name chunk ...)
  (*defforms "constant" (list (list (defidform/inline name) ": " chunk ...))))

(define-syntax-rule (defprocform name chunk ...)
  (*defforms "procedure" (list (list (defidform/inline name) chunk ...))))

(define-syntax-rule (defprocforms name [chunk0 ...] [chunk ...] ...)
  (*defforms "procedure"
             (list (list (defidform/inline name) chunk0 ...)
                   (list (redefidform/inline name) chunk ...) ...)))

(define-syntax-rule (defmethform name chunk ...)
  (*defforms "method"
             (list (list (defidform/inline name) chunk ...))))

(define-syntax-rule (defmethforms name [chunk0 ...] [chunk ...] ...)
  (*defforms "method"
             (list (list (defidform/inline name) chunk0 ...)
                   (link (redefidform/inline name) chunk ...)
                   ...)))

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

(define-syntax-rule (redefidform/inline form)
  (to-element 'form #:defn? #t))

(define-syntax (dssl2block stx)
  (syntax-parse stx
    [(_ str-expr ...)
     ; We want the first parameter to provide the right lexical context
     ; (but this doesn't seem to work anyway).
     (with-syntax ([hash-lang (datum->syntax stx "#lang dssl2\n")])
       #'(codeblock
           #:keep-lang-line? #f
           hash-lang str-expr ...))]))

(define-syntax-rule (code str-expr ...)
  (scribble:code #:lang "dssl2" str-expr ...))

(define (indent . chunks)
  (cons ~ (cons ~ (cons ~ (cons ~ chunks)))))

