#lang racket/base

(provide define-dssl-class
         define-dssl-interface
         dssl-self)

(require (for-syntax syntax/srcloc)) ; delete me

(require (for-syntax racket/base
                     syntax/parse
                     (only-in racket/sequence in-syntax)
                     (only-in racket/string string-prefix?)
                     (only-in racket/syntax format-id)
                     "errors.rkt"
                     "interface.rkt"
                     "names.rkt"
                     "util.rkt"))
(require (only-in racket/contract
                  blame-swap
                  contract
                  raise-blame-error)
         (only-in racket/unsafe/undefined
                  unsafe-undefined
                  check-not-unsafe-undefined)
         (only-in racket/vector vector-memq)
         (only-in racket/stxparam
                  define-syntax-parameter
                  syntax-parameterize)
         (only-in syntax/parse/define define-simple-macro)
         "contract.rkt"
         "errors.rkt"
         "generic.rkt"
         "names.rkt"
         "object.rkt"
         "provide.rkt"
         "struct.rkt"
         "stxparams.rkt"
         "util.rkt")

(define (contract-params-match? cs1 cs2)
  (for/and ([c1 (in-vector cs1)]
            [c2 (in-vector cs2)])
    (eq? c1 c2)))

(define-for-syntax (lookup-interface interface-name)
  (define result
    (syntax-local-value
      interface-name
      (λ () (syntax-error interface-name "undefined interface"))))
  (if (interface-info? result)
    result
    (syntax-error interface-name "is not an interface")))

(define (union-interfaces i1 i2)
  (define result (make-hasheq))
  (for ([(key value) (in-hash i1)])
    (hash-set! result key value))
  (for ([(key value) (in-hash i2)])
    (hash-set! result key
               (cond
                 [(hash-ref result key #f)
                  =>
                  (λ (value*) (λ (x) (value* (value x))))]
                 [else value])))
  (make-immutable-hasheq (hash->list result)))

(define (((apply-interface-contract name
                                    interface-table
                                    contract-parameters
                                    interface-token
                                    first-order?)
          blame)
         value neg-party)
  (cond
    [(and (object-base? value)
          (assq interface-token
                (object-base-contract-paramses value)))
     =>
     (λ (pair)
        (if (contract-params-match?
              (cdr pair)
              contract-parameters)
          value
          (raise-blame-error
            blame #:missing-party neg-party value
            (string-append
              "cannot re-protect with with same interface"
              " (~a) and different contract parameters")
            name)))]
    [(first-order? value)
     ((object-info-projector (object-base-info value))
      value
      (cons interface-token contract-parameters)
      (λ (sel method)
         (cond
           [(hash-ref interface-table sel #f)
            =>
            (λ (apply-contract) (apply-contract method))]
           [else
             (λ _
                (raise-blame-error
                  (blame-swap blame)
                  #:missing-party neg-party value
                  "interface ~a is protecting method ~a"
                  name sel))])))]
    [(object-base? value)
     (raise-blame-error
       blame #:missing-party neg-party value
       "class ~a does not implement interface ~a"
       (object-info-name (object-base-info value))
       name)]
    [else
      (raise-blame-error
        blame #:missing-party neg-party value
        "value ~e does not implement interface ~a"
        value name)]))

(define-syntax (define-dssl-interface stx)
  (syntax-parse stx
    [(_ name:id
        (cv:id ...)
        ((super-name:id super-param:expr ...) ...)
        ([method-name:id
           (method-cv:id ...)
           (method-param-contract:expr ...)
           method-result-contract:expr]
         ...))
     #:fail-when (check-duplicate-identifier
                   (syntax->list #'(method-name ...)))
                 "duplicate method name"
     ; Checking and metadata generation:
     (for ([method-name (in-syntax #'(method-name ...))])
       (unless (public-method-name? method-name)
         (syntax-error method-name "interface methods cannot be private")))
     (define super-infos
       (for/list ([super-name (in-syntax #'(super-name ...))])
         (lookup-interface super-name)))
     (define interface-token (gensym))
     (define interface-runtime-name (datum->syntax stx (gensym)))
     (define interface-static-info
       (interface-info
         #'name
         interface-token
         interface-runtime-name
         super-infos
         (for/list ([name   (in-syntax #'(method-name ...))]
                    [m-cvs  (in-syntax #'((method-cv ...) ...))]
                    [params (in-syntax #'((method-param-contract ...) ...))])
           (with-syntax
             ([(param ...) params])
             (imethod-info
               name
               (syntax-length m-cvs)
               (syntax-length params))))))
     (check-interface-consistency interface-static-info)
     (define name-length (string-length (symbol->string (syntax-e #'name))))
     ; Code generation
     (define base-interface-table
       #'(make-immutable-hasheq
           (list (cons 'method-name
                       (let
                         ([the-contract
                            (square-bracket-proc-contract
                              method-name
                              [method-cv ...]
                              (ensure-contract 'def method-param-contract)
                              ...
                              (ensure-contract 'def method-result-contract))])
                         (λ (method)
                            (contract
                              the-contract
                              method
                              (format "method ~a of interface ~a"
                                      'method-name 'name)
                              "method caller"
                              #f
                              (get-srcloc method-name)))))
                 ...)))
     (define extended-interface-table
       (for/fold ([table base-interface-table])
         ([super-info   (in-list super-infos)]
          [super-params (in-syntax #'((super-param ...) ...))])
         #`(union-interfaces
             #,table
             (#,(interface-info-runtime super-info) #,@super-params))))
     (syntax-property
       (syntax-property
         #`(begin
             (dssl-provide name
                           #,(struct-predicate-name #'name)
                           #,(interface-contract-name #'name))
             (define (#,interface-runtime-name cv ...)
               #,extended-interface-table)
             (define-syntax name #,(reflect-interface interface-static-info))
             (define (first-order? obj)
               (and (object-base? obj)
                    (vector-memq '#,interface-token
                                 (object-info-interfaces
                                   (object-base-info obj)))))
             (define (make-projection cv ...)
               (define interface-table (#,interface-runtime-name cv ...))
               (define contract-parameters (vector-immutable cv ...))
               (apply-interface-contract
                 'name
                 interface-table
                 contract-parameters
                 '#,interface-token
                 first-order?))
             (define (#,(struct-predicate-name #'name) obj)
               (and (first-order? obj) #t))
             (define #,(interface-contract-name #'name)
               (square-bracket-contract
                 #,(interface-contract-name #'name)
                 ([cv AnyC] ...)
                 #:first-order first-order?
                 #:late-neg-projection
                 (make-projection cv ...))))
         'sub-range-binders
         (cons
           (vector (syntax-local-introduce (struct-predicate-name #'name))
                   0 name-length 0.5 0.5
                   (syntax-local-introduce #'name)
                   0 name-length 0.5 0.5)
           (vector (syntax-local-introduce (interface-contract-name #'name))
                   0 name-length 0.5 0.5
                   (syntax-local-introduce #'name)
                   0 name-length 0.5 0.5)))
       'disappeared-use
       (map syntax-local-introduce
            (syntax->list #'(super-name ...))))]))

(define-syntax-parameter
  dssl-self
  (lambda (stx)
    (syntax-error stx "use of self outside of method")))

(define-syntax (define-field stx)
  (syntax-parse stx
    [(_ external-name:id internal-name:id actual-name:id the-contract:expr)
     ; (define arrow-name (property-arrow-name #'external-name))
     #`(begin
         (define actual-name unsafe-undefined)
         ; (define-syntax #,arrow-name #'external-name)
         (define-syntax internal-name
           (make-set!-transformer
             (λ (stx)
                (syntax-parse stx #:literals (set!)
                  [(set! _:id e:expr)
                   (quasisyntax/loc #'the-contract
                     (set! actual-name
                       #,(quasisyntax/loc #'e
                           (contract
                             the-contract e
                             (format "field assignment at ~a"
                                     (srcloc->string (get-srcloc e)))
                             'external-name
                             'external-name
                             (get-srcloc the-contract)))))]
                  [_:id
                    #'(check-not-unsafe-undefined
                        actual-name
                        'external-name)]
                  [(_:id . args)
                   (with-syntax ([app (datum->syntax stx '#%app)])
                     #'(app
                         (check-not-unsafe-undefined
                           actual-name
                           'external-name)
                         . args))])))))]))

(define-simple-macro (bind-self class:id self:id actual-self:id body:expr)
  (syntax-parameterize
    ([dssl-self
       (syntax-parser
         [(_ prop:id)          (class-qualify #'class #'prop)]
         [(_ prop:id rhs:expr) #`(set! #,(class-qualify #'class #'prop) rhs)]
         [_:id                 #'actual-self])])
    (begin
      (define-syntax self
        (make-set!-transformer
          (syntax-parser
            [use:id
              (syntax-property
                #'dssl-self
                'disappeared-use
                (syntax-local-introduce #'use))]
            [(set! lhs:id _)
             (syntax-error #'lhs "cannot assign to self parameter")]
            [(op . _)
             (syntax-error #'op "self parameter is not a function")])))
      body)))

(define-for-syntax (find-constructor method-names method-paramses stx)
  (let/ec return
    (for ([method-name   (in-list method-names)]
          [method-params (in-syntax method-paramses)])
      (when (eq? '__init__ (syntax-e method-name))
        (return method-name
                (generate-temporaries method-params))))
    (syntax-error stx "class must have a constructor __init__")))

(define-for-syntax (check-class-against-interface
                     interface-info0
                     class-name
                     class-method-names
                     class-method-cvarses
                     class-method-paramses)
  (define class-methods (make-hasheq))
  (for ([name   (in-list class-method-names)]
        [cvars  (in-list class-method-cvarses)]
        [params (in-list class-method-paramses)])
    (hash-set! class-methods
               (syntax-e name)
               (imethod-info name
                             (syntax-length cvars)
                             (syntax-length params))))
  (define (loop interface-info)
    (define interface-name (syntax-e (interface-info-name interface-info)))
    (for-each loop (interface-info-supers interface-info))
    (for ([i-m-info (interface-info-methods interface-info)])
      (define method-name (syntax-e (imethod-info-name i-m-info)))
      (cond
        [(hash-ref class-methods method-name #f)
         =>
         (λ (c-m-info)
            (cond
              [(compare-imethod-info interface-name c-m-info i-m-info)
               =>
               (λ (message) (syntax-error class-name message))]
              [else (void)]))]
        [else
          (syntax-error
            class-name
            "class missing method ~a, required by interface ~a"
            method-name
            interface-name)])))
  (loop interface-info0))

(define-syntax (make-generic-class-predicate stx)
  (syntax-parse stx
    [(_ name?:id internal-name?:id (cvs:id ...))
     #'(square-bracket-proc
         name?
         #:generic (cvs ...)
         (let ([contract-params (vector-immutable cvs ...)])
           (λ (value)
              (cond
                [(and (internal-name? value)
                      (assq #f (object-base-contract-paramses value)))
                 =>
                 (λ (pair)
                    (contract-params-match? (cdr pair) contract-params))]
                [else #f])))
         #:default internal-name?)]))

(define-syntax (define-class-predicate stx)
  (syntax-parse stx
    [(_ name:id internal-name:id [cvs0:id ...])
     (define name-length (string-length (symbol->string (syntax-e #'name))))
     (with-syntax
       ([name?          (struct-predicate-name #'name)]
        [internal-name? (struct-predicate-name #'internal-name)])
       (syntax-property
        #`(begin
            (dssl-provide name?)
            (define name?
              #,(syntax-parse #'(cvs0 ...)
                  [()
                   #'(λ (v) (internal-name? v))]
                  [(cvs:id ...+)
                   #'(make-generic-class-predicate
                       name? internal-name? (cvs ...))])))
        'sub-range-binders
        (vector (syntax-local-introduce #'name?)
                0 name-length 0.5 0.5
                (syntax-local-introduce #'name)
                0 name-length 0.5 0.5)))]))

(define-syntax (define-dssl-class stx)
  (syntax-parse stx
    [(_ name:id
        (cv:id ...)
        (interface:id ...)
        ([field-var:id field-contract:expr]
         ...)
        ([method-name:id
           (method-cv:id ...)
           method-self:id
           ([method-param-var:id method-param-contract:expr]
            ...)
           method-result-contract:expr
           method-body:expr]
         ...))
     #:fail-when (check-duplicate-identifier
                   (cons (datum->syntax #'name '__class__)
                         (syntax->list #'(method-name ...))))
                 "duplicate method name"
     (for ([method-info
             (in-syntax #'((method-self method-param-var ...) ...))])
       (cond
         [(check-duplicate-identifier (syntax->list method-info))
          =>
          (λ (duplicate)
             (syntax-error duplicate "duplicate method parameter"))]
         [else (void)]))
     ; Extract the defined names:
     (define field-names  (syntax->list #'(field-var ...)))
     (define method-names (syntax->list #'(method-name ...)))
     (define-values (constructor constructor-params)
       (find-constructor
         method-names
         #'((method-param-var ...) ...)
         #'name))
     ; Lookup and check interfaces:
     (define interfaces
       (for/list ([interface-name (in-syntax #'(interface ...))])
         (define interface-info (lookup-interface interface-name))
         (check-class-against-interface
           interface-info
           #'name
           method-names
           (syntax->list #'((method-cv ...) ...))
           (syntax->list #'((method-param-var ...) ...)))
         interface-info))
     ; Generate new names:
     (define (self. property) (class-qualify #'name property))
     (with-syntax
       ([internal-name
          (format-id #f "c:~a" #'name)]
        [(interface-token ...)
         (interface-info-all-tokens/list interfaces)]
        [(actual-field-name ...)
         (generate-temporaries field-names)]
        [(self.field-name ...)
         (map self. field-names)]
        [(self.method-name ...)
         (map self. method-names)]
        [(public-method-name ...)
         (filter public-method-name? method-names)]
        [(self.public-method-name ...)
         (map self. (filter public-method-name? method-names))])
       (syntax-property
         #`(begin
             (struct internal-name object-base
               (__class__
                 public-method-name
                 ...)
               #:transparent)
             ; Used for applying interfact contracts:
             (define (map-fields val contract-params visitor)
               (internal-name
                 (object-base-info val)
                 (cons contract-params
                       (object-base-contract-paramses val))
                 (object-base-reflect val)
                 (visitor '__class__
                          ((struct-getter-name internal-name __class__)
                           val))
                 (visitor 'public-method-name
                          ((struct-getter-name
                             internal-name
                             public-method-name)
                           val))
                 ...))
             (define internal-object-info
               (object-info
                 'name
                 map-fields
                 (vector-immutable 'interface-token ...)
                 (vector-immutable
                   (method-info
                     '__class__
                     (struct-getter-name internal-name __class__))
                   (method-info
                     'public-method-name
                     (struct-getter-name internal-name public-method-name))
                   ...)))
             (define-class-predicate name internal-name [cv ...])
             (dssl-provide name)
             (define name
               (square-bracket-class-lambda
                 name ([cv AnyC] ...) #,constructor-params
                 (define-field field-var
                               self.field-name
                               actual-field-name
                               field-contract)
                 ...
                 (define-square-bracket-proc
                   ((self.method-name method-cv ...)
                    [method-param-var
                      (ensure-contract 'def method-param-contract)] ...)
                   (ensure-contract 'def method-result-contract)
                   (bind-self name method-self actual-self
                              (wrap-procedure-body method-body)))
                 ...
                 (define self.__class__ name)
                 (define actual-self
                   (internal-name
                     internal-object-info
                     (list (cons #f (vector-immutable cv ...)))
                     (λ () (vector-immutable
                              (cons 'field-var self.field-name) ...))
                     ; methods:
                     self.__class__
                     self.public-method-name ...))
                 (#,(self. constructor) #,@constructor-params)
                 (when (eq? unsafe-undefined actual-field-name)
                   (raise-runtime-error
                     #:context (capture-context #,constructor)
                     "constructor for class %s did not assign field %s"
                     'name 'field-var))
                 ...
                 actual-self)))
         'disappeared-use
         (map syntax-local-introduce
              (syntax->list #'(interface ...)))))]))
