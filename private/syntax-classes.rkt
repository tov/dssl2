#lang racket/base

(provide req-timeout
         opt-timeout
         unary-operator
         binary-operator
         real-id
         var
         var&ctc
         unique-identifiers
         super-interface
         opt-implements
         opt-return-ctc
         opt-ctc-vars
         vec-lit-size-expr
         test-points-expr)

(require (for-syntax
           racket/base
           syntax/parse)
         syntax/parse
         (only-in racket/format
                  ~a)
         (for-template
           (only-in "prims.rkt"
                    AnyC
                    OrC
                    bool?
                    nat?
                    num?
                    pos?)
           (only-in racket/base
                    #%app
                    #%datum)))

(define-splicing-syntax-class req-timeout
  #:attributes (seconds)
  (pattern (~seq #:timeout raw-seconds)
           #:declare raw-seconds
           (expr/c #'pos?
                   #:positive "DSSL2"
                   #:name "timeout seconds")
           #:attr seconds #'raw-seconds.c))

(define-splicing-syntax-class opt-timeout
  #:attributes (seconds loc)
  (pattern (~seq (~and loc (:req-timeout))))
  (pattern (~seq)
           #:attr seconds #'#f
           #:attr loc     (datum->syntax #f #f)))

(define-syntax ~datums
  (pattern-expander
    (λ (stx)
       (syntax-parse stx
         [(_ datum:id ...)
          #'(~or (~datum datum) ...)]))))

(define-syntax-class unary-operator
  #:attributes (name)
  (pattern (~and lit (~datums not ~))
           #:attr name #`#,(~a (syntax-e #'lit))))

(define-syntax-class binary-operator
  #:attributes (name)
  (pattern (~and lit
                 (~datums == != ≠
                          <= ≤ <
                          >= ≥ >
                          in ∈ |not in| ∉
                          is |is not|))
           #:attr name #`#,(~a (syntax-e #'lit))))

(define (stx-underscore? stx)
  (eq? '_ (syntax-e stx)))

(define-syntax-class real-id
  (pattern id:id
           #:fail-when (stx-underscore? #'id)
           "_ is not a real identifier"))

(define-syntax-class var
  (pattern raw:id
           #:attr id (if (stx-underscore? #'raw)
                       #`#,(gensym '_)
                       #'raw)))

(define-syntax-class var&ctc
  #:attributes (var ctc)
  #:description "association of identifier with contract"
  (pattern [v:var ctc:expr]
           #:attr var #'v.id)
  (pattern v:var
           #:attr var #'v.id
           #:attr ctc (datum->syntax #'AnyC 'AnyC)))

(define-syntax-class unique-identifiers
  #:attributes ([var 1])
  #:description "sequence of unique identifiers"
  (pattern (var:real-id ...)
           #:fail-when (check-duplicate-identifier
                         (syntax->list #'(var ...)))
           "duplicate identifier name"))

(define-syntax-class super-interface
  #:description "a superinterface, possibly with parameters"
  (pattern (name:id params:expr ...))
  (pattern name:id
           #:with (params ...) #'()))

(define-splicing-syntax-class opt-implements
  #:description "optional implements clause"
  (pattern (~seq #:implements (interface:id ...)))
  (pattern (~seq)
           #:with (interface:id ...) #'()))

(define-splicing-syntax-class opt-return-ctc
  #:description "optional return contract"
  (pattern (~seq #:-> result:expr))
  (pattern (~seq)
           #:with result (datum->syntax #'AnyC 'AnyC)))

(define-splicing-syntax-class opt-ctc-vars
  #:description "optional forall-quantified contract variables"
  (pattern (~seq (~or #:forall #:∀) vars:unique-identifiers)
           #:with (var ...) #'(vars.var ...))
  (pattern (~seq)
           #:with (var ...) #'()))

(define-syntax-class vec-lit-size-expr
  #:attributes (n)
  (pattern size
           #:declare size
           (expr/c #'nat?
                   #:positive "DSSL2"
                   #:name "vector size"
                   #:macro "[…; …]")
           #:attr n #'size.c))

(define-syntax-class test-points-expr
  #:attributes (value)
  (pattern points
           #:declare points
           (expr/c #'(OrC num? bool?)
                   #:positive "DSSL2"
                   #:name "test points"
                   #:macro "test = …")
           #:attr value #'points.c))
