#lang racket/base

(provide grammar
         defform defform*
         defidform defidform*
         defexpform defexpform* defexpidform defexpidform*
         defsmplform defsmplform* defsmplidform defsmplidform*
         defcmpdform defcmpdform* defcmpdidform defcmpdidform*
         defclassform linkclass Linkclass
         defconstform
         defprocform defprocforms
         defmethform defmethforms
         proto
         id-form
         ~opt ~many ~many1 ~many-comma ~... ~……
         c nt nt_ term term_
         q m t (rename-out [id-form k])
         dssl2block code
         indent
         (for-label
           (all-from-out dssl2 racket)))

(require "util.rkt"
         (for-label dssl2
                    (prefix-in racket: racket))
         (for-syntax racket/base
                     "../private/names.rkt"
                     "util.rkt"
                     (only-in racket/syntax format-id))
         (except-in scribble/manual
                    defform defform*
                    defidform defidform/inline)
         scribble/racket
         scribble/struct
         (prefix-in scribble: scribble/manual)
         syntax/parse/define
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
  #`(list "⟨"
          (italic #,elem)
          #,(if sub #`(subscript #,sub) #'"")
          "⟩"))

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

(define-syntax (id-form stx)
  (syntax-parse stx
    [(_ form:id #:def)
     #'(scribble:defidform/inline form)]
    [(_ form:id #:link)
     #'(bold (racket form))]
    [(_ form:id (~optional #:re))
     #'(to-element 'form #:defn? #t)]))

(begin-for-syntax
  (define-syntax-class form-kind
    (pattern #:exp  #:attr kind #'"expr")
    (pattern #:smpl #:attr kind #'"simple")
    (pattern #:cmpd #:attr kind #'"compound")
    (pattern #:proc #:attr kind #'"procedure"))

  (define-splicing-syntax-class opt-form-kw
   (pattern (~seq #:re)    #:attr out #'#:re)
   (pattern (~seq #:link)  #:attr out #'#:link)
   (pattern (~seq #:def)   #:attr out #'#:def)
   (pattern (~seq)         #:attr out #'#:def))

  (define-splicing-syntax-class name-kws
    (pattern (~seq name:id kw:opt-form-kw)
             #:attr [$ 1]  (list #'name #'kw.out)
             #:attr redef  #'(id-form name #:re)
             #:attr def    #'(id-form name kw.out)))

  (define-splicing-syntax-class defidform-head
    (pattern (~seq fk:form-kind kw:name-kws)
             #:attr [$ 1] (cons #'fk (attribute kw.$))
             #:attr kind  #'fk.kind
             #:attr name  #'kw.name
             #:attr def   #'kw.def
             #:attr redef #'kw.redef)))

(define-simple-macro (defform* :form-kind form ...)
  (*defforms kind (list form ...)))

(define-simple-macro (defform kind:form-kind chunk ...)
  (defform* kind (list chunk ...)))

(define-simple-macro
  (defidform* hd:defidform-head form0 form ...)
  (let* ([hd.name hd.redef])
    (*defforms
      hd.kind
      (list (list hd.def form0)
            form ...))))

(define-simple-macro
  (defidform hd:defidform-head chunk ...)
  (defidform* hd.$ ... (list chunk ...)))

(define-syntax-rule (defexpform chunk ...)
  (defform #:exp chunk ...))

(define-syntax-rule (defexpform* form ...)
  (defform* #:exp form ...))

(define-syntax-rule (defsmplform chunk ...)
  (defform #:smpl chunk ...))

(define-syntax-rule (defsmplform* form ...)
  (defform* #:smpl form ...))

(define-syntax-rule (defcmpdform chunk ...)
  (defform #:cmpd chunk ...))

(define-syntax-rule (defcmpdform* form ...)
  (defform* #:cmpd form ...))

(define-syntax-parser define-def*idform
  [(_ short-name:id)
   (with-syntax
     ([form-name  (format->stx string->symbol "def~aidform" #'short-name)]
      [form-name* (format->stx string->symbol "def~aidform*" #'short-name)]
      [kw         (format->stx string->keyword "~a" #'short-name)])
     #'(begin
         (define-simple-macro (form-name* nk:name-kws first . rest)
           (defidform* kw nk.$ (... ...) (list ~ first) . rest))
         (define-simple-macro (form-name nk:name-kws . rest)
           (form-name* nk.$ (... ...) (list . rest)))))])

; format->stx : [String -> X] [Format-string-like "~a"] Identifier
;               -> [Syntax-of X]
(define-for-syntax (format->stx cvt fmt stx)
  (datum->syntax stx (cvt (format fmt (syntax-e stx)))))

(define-def*idform exp)
(define-def*idform smpl)
(define-def*idform cmpd)

(define-syntax-rule (defclassform name)
  (subsection #:tag (format "class:~a" 'name)
                 "Class "
                 (c (symbol->string 'name))))

(define-syntax-rule (linkclass name)
  (seclink (format "class:~a" 'name)
           "class " (tt (format "~a" 'name))))

(define-syntax-rule (Linkclass name)
  (seclink (format "class:~a" 'name)
           "Class " (tt (format "~a" 'name))))

(define-syntax-rule (defconstform name chunk ...)
  (*defforms "constant"
             (list (list (id-form name #:def) ": " chunk ...))))

(define-syntax-rule (defprocform name chunk ...)
  (defidform #:proc name chunk ...))

(define-syntax-rule (defprocforms name form0 form ...)
  (defidform* #:proc name form0 (list name form) ...))

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
                  (list (list (id-form #,method-name #:def) chunk ...)))]))

(define-syntax (defmethforms stx)
  (syntax-parse stx
    [(_ name:id sel:id [chunk0 ...] [chunk ...] ...)
     (define method-name (class-qualify #'name #'sel))
     #`(*defforms "method"
                  (list (list (id-form #,method-name #:def) chunk0 ...)
                        (link (id-form #,method-name #:re ) chunk ...)
                        ...))]))

(define (*defforms kind forms)
  (define labeller (add-background-label kind))
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

(define ~... (m "⋮"))
(define ~…… (racketoutput "..."))

(define (indent . chunks)
  (list* ~ ~ ~ ~ chunks))

