#lang racket/base

(require "../private/names.rkt"
         "util.rkt"
         (for-syntax racket/base
                     "util.rkt"
                     (only-in racket/syntax format-id)))

(provide grammar
         defexpform defexpforms defsmplform defcmpdform defcmpdforms
         defclassform linkclass Linkclass
         defconstform
         defprocform defprocforms
         defmethform defmethforms
         proto
         redefidform/inline
         ~opt ~many ~many1 ~many-comma
         c nt nt_ term term_
         q m t
         dssl2block code
         indent)
(require scribble/manual
         scribble/racket
         scribble/struct
         (prefix-in scribble: scribble/manual)
         (for-syntax syntax/parse
                     (only-in racket/string string-contains?)
                     (only-in racket/sequence in-syntax)))

(define ((meta-surround before after) . x)
  (apply ~list (m before) (append x (list (m after)))))

(define ~opt (meta-surround "[" "]"))
(define ~many (meta-surround "{" "}*"))
(define ~many1 (meta-surround "{" "}+"))
(define ~many-comma (meta-surround "{" "},*"))
(define (~list . args)
  (intersperse (tt ~) args))

(define-for-syntax (~nonterminal name0 #:def? [def? #f] #:sub [sub #f])
  (define name   (symbol->string (syntax-e name0)))
  (define tag    (format "nt:~a" name))
  (define elem
    (if def?
      #`(elemtag #,tag #,name)
      #`(elemref #,tag #,name #:underline? #f)))
  #`(list "‹"
          (italic #,elem)
          #,(if sub #`(subscript #,sub) #'"")
          "›"))

(define-syntax (parse-rhs stx)
  (syntax-parse stx
    #:literals (~opt ~many ~many1 ~many-comma quote)
    [(_ (nt:id ...) #f)
     #'""]
    [(_ (nt:id ...) name:id)
     #:when (memq (syntax-e #'name) (syntax->datum #'(nt ...)))
     (~nonterminal #'name)]
    [(_ (nt:id ...) name:id)
     #'(scribble:racket name)]
    [(_ (nt:id ...) token:str)
     #'(q token)]
    [(_ (nt:id ...) (quote token:id))
     #`(t #,(symbol->string (syntax-e #'token)))]
    [(_ (nt:id ...) (~opt arg:expr ...))
     #'(~opt (parse-rhs (nt ...) arg) ...)]
    [(_ (nt:id ...) (~many arg:expr ...))
     #'(~many (parse-rhs (nt ...) arg) ...)]
    [(_ (nt:id ...) (~many1 arg:expr ...))
     #'(~many1 (parse-rhs (nt ...) arg) ...)]
    [(_ (nt:id ...) (~many-comma arg:expr ...))
     #'(~many-comma (parse-rhs (nt ...) arg) ...)]
    [(_ (nt:id ...) (sub:expr ...))
     #'(~list (parse-rhs (nt ...) sub) ...)]))

(define-syntax (grammar stx)
  (syntax-parse stx
    [(_ [non-terminal:id production0:expr production:expr ...] ...)
     (define (interpret-nt nt)
       (or (and nt (~nonterminal nt #:def? #t))
           ""))
     (define (interpret-sym sym)
       (or (and sym #`(m #,sym))
           #'""))
     (define rows '())
     (define (row nt sym rhs)
       (set! rows
         (cons #`(list #,(interpret-nt nt)
                       #,(interpret-sym sym)
                       (parse-rhs (non-terminal ...) #,rhs))
               rows)))
     (for ([non-terminal (in-syntax #'(non-terminal ...))]
           [productions  (in-syntax #'((production0 production ...) ...))])
       (let ([productions (syntax->list productions)])
         (row non-terminal "=" (car productions))
         (for ([production (cdr productions)])
           (row #f "|" production))
         (row #f #f #f)))
     #`(tabular
         #:sep (tt ~)
         #:column-properties '(right center left)
         (list #,@(reverse rows)))]))

(define (q x)
  (elem (racketvalfont x)))

(define (t x)
  (elem #:style 'italic (racketvalfont x)))

(define (m x)
  (racketoutput (larger (larger x))))

(define (c . codes)
  (elem #:style 'tt codes))

(define-syntax-rule (term var)
  (elem #:style 'italic (racketvalfont (format "~a" 'var))))

(define-syntax-rule (term_ var sub ...)
  (racketvalfont (elem #:style 'italic (format "~a" 'var))
                 (subscript sub ...)))

(define-syntax (nt stx)
  (syntax-parse stx
    [(_ name:id) (~nonterminal #'name)]))

(define-syntax (nt_ stx)
  (syntax-parse stx
    [(_ name:id sub:expr ...)
     (~nonterminal #'name #:sub #'(list sub ...))]))

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
  (seclink (format "class:~a" 'name)
           "class " (tt (format "~a" 'name))))

(define-syntax-rule (Linkclass name)
  (seclink (format "class:~a" 'name)
           "Class " (tt (format "~a" 'name))))

(define-syntax-rule (defconstform name chunk ...)
  (*defforms "constant" (list (list (defidform/inline name) ": " chunk ...))))

(define-syntax-rule (defprocform name chunk ...)
  (*defforms "procedure" (list (list (defidform/inline name) chunk ...))))

(define-syntax-rule (defprocforms name [chunk0 ...] [chunk ...] ...)
  (*defforms "procedure"
             (list (list (defidform/inline name) chunk0 ...)
                   (list (redefidform/inline name) chunk ...) ...)))

(define-syntax (typeset-param stx)
  (syntax-parse stx #:datum-literals (...)
    [(_ ...)
     #'"..."]
    [(_ param:id)
     #`(typeset-param #,(datum->syntax
                          #'param (symbol->string (syntax-e #'param))))]
    [(_ param:str)
     #:when (string-contains? (syntax-e #'param) ":")
     (define split (regexp-split #rx":" (syntax-e #'param)))
     #`(list
         #,(car split)
         ": "
         (code #,(datum->syntax #'param (cadr split))))]
    [(_ param:str)
     #'(code param)]))

(define-syntax (typeset-tparam stx)
  (syntax-parse stx
    [(_ tvar:id)
     (datum->syntax #'tvar (symbol->string (syntax-e #'tvar)))]))

(define-syntax (proto stx)
  (syntax-parse stx
    [(_ #:all (tvar:id ...) param:expr ... result:expr)
     (define params (intersperse
                      #'", "
                      (syntax->list #'((typeset-tparam tvar) ...))))
     #`(list "[" #,@params "]" (proto param ... result))]
    [(_ param:expr ... result:expr)
     (define params (intersperse
                      #'", "
                      (syntax->list #'((typeset-param param) ...))))
     #`(list "(" #,@params ") -> " (typeset-param result))]))


(define-syntax (defmethform stx)
  (syntax-parse stx
    [(_ name:id sel:id chunk ...)
     (define method-name (class-qualify #'name #'sel))
     #`(*defforms "method"
                  (list (list (defidform/inline #,method-name) chunk ...)))]))

(define-syntax (defmethforms stx)
  (syntax-parse stx
    [(_ name:id sel:id [chunk0 ...] [chunk ...] ...)
     (define method-name (class-qualify #'name #'sel))
     #`(*defforms "method"
                  (list (list (defidform/inline #,method-name) chunk0 ...)
                        (link (redefidform/inline #,method-name) chunk ...)
                        ...))]))

(define (*defforms kind forms)
  (define labeller (add-background-label (or kind "syntax")))
  (define (make-cell form)
    (list (make-paragraph (list (to-element (c form))))))
  (define table-content
    (cons
      (list (labeller (make-cell (car forms))))
      (map list (map make-cell (cdr forms)))))
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

