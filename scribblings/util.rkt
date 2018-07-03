#lang racket/base

(require (only-in racket/list
                  first
                  rest)
         (only-in racket/format
                  ~a)
         (for-syntax racket/base))

(provide grammar
         defexpform defexpforms defsmplform defcmpdform defcmpdforms
         defclassform linkclass Linkclass
         defconstform
         defprocform defprocforms
         defmethform defmethforms
         redefidform/inline
         ~opt ~many ~many1 ~many-comma
         c syn syn_
         q m t
         dssl2block code
         indent)
(require scribble/manual
         scribble/racket
         scribble/struct
         (prefix-in scribble: scribble/manual)
         (for-syntax syntax/parse
                     racket/sequence))

(define ((meta-surround before after) . x)
  (apply ~list (m before) (append x (list (m after)))))

(define ~opt (meta-surround "[" "]"))
(define ~many (meta-surround "{" "}*"))
(define ~many1 (meta-surround "{" "}⁺"))
(define ~many-comma (meta-surround "{" "},*"))
(define (~list . args)
  (intersperse (tt ~) args))

(define (intersperse x ys)
  (cond
    [(null? ys) '()]
    [(null? (cdr ys)) ys]
    [else (cons (car ys) (cons x (intersperse x (cdr ys))))]))

(define-for-syntax (~nonterminal nt)
  #`(list "‹" (emph #,(symbol->string (syntax-e nt))) "›"))

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
       (or (and nt (~nonterminal nt))
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

