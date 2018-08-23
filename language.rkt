#lang racket/base

(provide #%app
         #%datum
         #%top
         #%require)
(provide (rename-out
           ; special syntax
           [dssl-module-begin           #%module-begin]
           [dssl-top-interaction        #%top-interaction]
           ; syntax
           [begin               begin]
           [if                  if-e]
           [else                else]
           [void                pass]
           [vec-lit             vec-lit]
           [dssl-True           True]
           [dssl-False          False]
           [dssl-assert         assert]
           [dssl-assert-eq      assert_eq]
           [dssl-assert-error   assert_error]
           [dssl-break          break]
           [dssl-class          class]
           [dssl-continue       continue]
           [dssl-def            def]
           [dssl-elif           elif]
           [dssl-error          error]
           [dssl-for            for]
           [dssl-for/vec        for/vec]
           [dssl-if             if]
           [dssl-interface      interface]
           [dssl-lambda         lambda]
           [dssl-let            let]
           [dssl-make-vec       make-vec]
           [dssl-import         import]
           [dssl-return         return]
           [dssl-=              =]
           [dssl-struct         struct]
           [dssl-struct-ref     struct-ref]
           [dssl-test           test]
           [dssl-time           time]
           [dssl-vec-ref        vec-ref]
           [dssl-while          while])
         (all-from-out "private/prims.rkt")
         (all-from-out "private/operators.rkt"))

(require "private/names.rkt"
         "private/errors.rkt"
         "private/operators.rkt"
         "private/struct.rkt"
         "private/object.rkt"
         "private/generic.rkt"
         "private/prims.rkt"
         "private/printer.rkt"
         racket/stxparam
         racket/splicing
         racket/contract/region
         (only-in racket/unsafe/undefined
                  unsafe-undefined
                  check-not-unsafe-undefined)
         syntax/parse/define
         rackunit
         (only-in racket/contract/base
                  ->
                  contract
                  rename-contract)
         (only-in racket/contract/parametric
                  parametric->/c)
         (only-in racket/math
                  natural?)
         (only-in racket/vector
                  vector-memq)
         (only-in syntax/location
                  quote-srcloc))
(require (prefix-in racket: racket/base)
         (prefix-in racket: racket/contract/base)
         (prefix-in racket: racket/contract/combinator))

(require (for-syntax racket/base
                     syntax/parse
                     (only-in racket/sequence in-syntax)
                     (only-in racket/syntax format-id syntax-local-eval)
                     (only-in racket/string string-prefix?)
                     (only-in racket/list make-list)
                     "private/names.rkt"
                     "private/errors.rkt"
                     "private/find-lib.rkt"))

(define dssl-True #t)
(define dssl-False #f)

(define-syntax-parameter
  inc-passed-tests!
  (lambda (stx)
    (syntax-error
      stx "test blocks cannot be used in the interactions window")))

(define-syntax-parameter
  inc-total-tests!
  (lambda (stx)
    (syntax-error
      stx "test blocks cannot be used in the interactions window")))

(define-syntax (dssl-module-begin stx)
  (syntax-case stx ()
    [(_ expr ...)
     #`(#%module-begin
        (module* configure-runtime racket/base
          (require dssl2/private/rte)
          (setup-rte))
        ; (#%provide #,(datum->syntax stx '(all-defined)))
        (module test-info racket/base
          (provide passed-tests total-tests inc-passed! inc-total!)
          (define passed-tests 0)
          (define total-tests 0)
          (define (inc-passed!) (set! passed-tests (add1 passed-tests)))
          (define (inc-total!) (set! total-tests (add1 total-tests))))
        (require 'test-info)
        (splicing-syntax-parameterize
          ([inc-passed-tests! (syntax-rules () [(_) (inc-passed!)])]
           [inc-total-tests!  (syntax-rules () [(_) (inc-total!)])])
          (dssl-begin expr ...))
        (print-test-results passed-tests total-tests))]))

(define (print-test-results passed total)
  (cond
    [(zero? total)       (void)]
    [(= passed total 1)  (printf "The only test passed\n")]
    [(= passed total 2)  (printf "Both tests passed\n")]
    [(= total 1)         (printf "The only test failed\n")]
    [(and (= total 2) (zero? passed))
                         (printf "Both tests failed\n")]
    [(= passed total)    (printf "All ~a tests passed\n" total)]
    [(zero? passed)      (printf "All ~a tests failed\n" total)]
    [else                (printf "~a of ~a tests passed\n" passed total)]))

(define-syntax-rule (dssl-top-interaction . expr)
  (dssl-begin expr))

(define-syntax (dssl-provide stx)
  (define (each-spec spec)
    (syntax-parse spec #:literals (for-syntax)
      [name:id #:when (public-method-name? #'name)
        #'(provide name)]
      [(for-syntax name:id) #:when (public-method-name? #'name)
        #'(provide (for-syntax name))]
      [_
        #'(begin)]))
  (if (eq? 'module (syntax-local-context))
    (syntax-parse stx
      [(_ spec ...)
       #`(begin
           #,@(map each-spec (syntax->list #'(spec ...))))])
    #'(begin)))

; This is so that the documentation will consider elif a keyword.
(define-syntax-parameter
  dssl-elif
  (lambda (stx)
    (syntax-error stx "use of elif keyword")))

; We define return (for lambda) as a syntax parameter, and then
; syntax-parameterize it inside dssl-lambda.
(define-syntax-parameter
  dssl-return
  (lambda (stx)
    (syntax-error stx "use of return keyword not in a function")))

(define-simple-macro (make-set!able f)
  (unless (zero? (random 1))
    (set! f (void))))

(define-syntax dssl-if
  (syntax-rules (else dssl-elif)
    [(_ [test0 result0 ...]
        [dssl-elif test result ...] ...
        [else else-result ...])
     (cond [test0 (dssl-begin result0 ...)]
           [test  (dssl-begin result ...)]
           ...
           [else (dssl-begin else-result ... )])]))

(define-syntax (dssl-lambda stx)
  (syntax-parse stx
    [(_ (param:id ...) expr:expr ...)
     (quasisyntax/loc stx
       (lambda (param ...) (dssl-begin expr ...)))]))

(begin-for-syntax
  (define-syntax-class
    var&contract
    #:description "association of identifier with contract"
    (pattern [var:id contract:expr])
    (pattern var:id
             #:with contract #'AnyC))

  (define-syntax-class
    unique-identifiers
    #:description "sequence of unique identifiers"
    (pattern (var:id ...)
             #:fail-when (check-duplicate-identifier
                           (syntax->list #'(var ...)))
             "duplicate identifier name"))

  (define-splicing-syntax-class
    optional-implements
    #:description "optional implements clause"
    (pattern (~seq #:implements (interface:id ...)))
    (pattern (~seq)
             #:with (interface:id ...) #'()))

  (define-splicing-syntax-class
    optional-return-contract
    #:description "optional return contract"
    (pattern (~seq #:-> result:expr))
    (pattern (~seq)
             #:with result #'AnyC))

  (define-splicing-syntax-class
    optional-contract-vars
    #:description "optional forall-quantified contract variables"
    (pattern (~seq (~or #:forall #:∀) vars:unique-identifiers)
             #:with (var ...) #'(vars.var ...))
    (pattern (~seq)
             #:with (var ...) #'())))

(define-syntax-rule (ensure-contract who contract)
  (ensure-contract/fn (get-srclocs contract) who contract))

(define-simple-macro (with-return expr:expr ...)
  (let/ec return-f
    (syntax-parameterize
      ([dssl-return (syntax-rules ()
                      [(_)        (return-f (void))]
                      [(_ result) (return-f result)])])
      (dssl-begin expr ...))))

(define-simple-macro
  (dssl-def (f:id cvs:optional-contract-vars bs:var&contract ...)
            result-contract:optional-return-contract
            expr:expr ...)
  #:fail-when (check-duplicate-identifier
                (syntax->list #'(cvs.var ... bs.var ...)))
              "duplicate argument name"
  (begin
    (dssl-provide f)
    (define-square-bracket-proc
      ((f cvs.var ...) [bs.var (ensure-contract 'def bs.contract)] ...)
      (ensure-contract 'def result-contract.result)
      (with-return expr ...))))

(define-syntax (dssl-let stx)
  (syntax-parse stx
    [(_ :var&contract)
     #'(begin
         (define real-var unsafe-undefined)
         (make-set!able real-var)
         (dssl-provide var)
         (define-syntax var
           (make-set!-transformer
            (λ (stx)
              (syntax-parse stx #:literals (set!)
                [(set! _:id e:expr)
                 (quasisyntax/loc #'contract
                   (set! real-var
                     #,(quasisyntax/loc #'e
                         (racket:contract
                           contract e
                           (format "assignment at ~a"
                                   (srcloc->string (get-srcloc e)))
                           'var
                           'var (get-srcloc contract)))))]
                [_:id
                 #'(check-not-unsafe-undefined real-var 'var)]
                [(_:id . args)
                 (with-syntax ([app (datum->syntax stx '#%app)])
                   #'(app
                       (check-not-unsafe-undefined real-var 'var)
                       . args))])))))]
    [(_ :var&contract expr)
     #'(begin
         (dssl-provide var)
         (define/contract var
                          (ensure-contract 'let contract)
                          expr)
         (make-set!able var))]))

; while uses two syntax parameters, break and continue (shared by for)
(define-syntax-parameter
  dssl-break
  (lambda (stx)
    (syntax-error stx "use of break keyword not in a loop")))

(define-syntax-parameter
  dssl-continue
  (lambda (stx)
    (syntax-error stx "use of continue keyword not in a loop")))

(define-syntax-rule (dssl-while test expr ...)
  (let/ec break-f
    (let loop ()
      (define (continue-f) (loop) (break-f (void)))
      (syntax-parameterize
        ([dssl-break (syntax-rules () [(_) (break-f (void))])]
         [dssl-continue (syntax-rules () [(_) (continue-f)])])
        (when test
          (dssl-begin expr ...)
          (loop))))))

(define-syntax (dssl-for stx)
  (syntax-parse stx
    [(_ [(i:id j:id) v:expr] expr:expr ...+)
     #:fail-when (and (bound-identifier=? #'i #'j) #'j)
                 "duplicate variable name"
     #'(let/ec break-f
         (for ([i (in-naturals)]
               [j (dssl-in-value v)])
           (let/ec continue-f
             (syntax-parameterize
               ([dssl-break    (syntax-rules () [(_) (break-f (void))])]
                [dssl-continue (syntax-rules () [(_) (continue-f)])])
               expr ...))))]
    [(_ [i:id v:expr] expr:expr ...+)
     #'(dssl-for [(_ i) v] (dssl-begin expr ...))]))

(define-syntax (dssl-for/vec stx)
  (syntax-parse stx
    [(_ [(i:id j:id) v:expr] expr:expr)
     #:fail-when (and (bound-identifier=? #'i #'j) #'j)
                 "duplicate variable name"
     #'(for/vector ([i (in-naturals)]
                    [j (dssl-in-value v)])
         expr)]
    [(_ [j:id v:expr] expr:expr)
     #'(dssl-for/vec [(_ j) v] expr)]
    [(_ [(i:id j:id) v:expr] #:when when expr:expr)
     #:fail-when (and (bound-identifier=? #'i #'j) #'j)
                 "duplicate variable name"
     #'(for/vector ([i (in-naturals)]
                    [j (dssl-in-value v)]
                    #:when when)
         expr)]
    [(_ [j:id v:expr] #:when when:expr expr:expr)
     #'(dssl-for/vec [(_ j) v] #:when when expr)]))

(define (dssl-in-value/value srclocs v)
  (cond
    [(vec? v)      (in-vector v)]
    [(natural? v)  (in-range v)]
    [(str? v)      (in-string v)]
    [else          (type-error #:srclocs srclocs
                               'for v "something iterable")]))

(define-syntax (dssl-in-value stx)
  (syntax-parse stx
    [(_ v:expr)
     #'(dssl-in-value/value (get-srclocs v) v)]))

(define-syntax (dssl-import stx)
  (unless (memq (syntax-local-context) '(module top-level))
    (syntax-error stx "import can only appear at top-level"))
  (define filename
    (syntax-parse stx
      [(_ lib:string) (syntax-e #'lib)]
      [(_ lib:id)
       (path->string
         (build-path
           lib-directory
           (format "~a.rkt" (syntax-e #'lib))))]))
  (datum->syntax stx `(#%require (file ,filename))))

; setf! is like Common Lisp setf, but it just recognizes three forms. We
; use this to translate assignments.
(define-syntax (dssl-= stx)
  (syntax-parse stx #:literals (dssl-vec-ref dssl-struct-ref)
    [(_ (dssl-vec-ref v:expr i:expr ...+) rhs:expr)
     #'(dssl-vec-set! v (list i ...) rhs)]
    [(_ (dssl-struct-ref s:expr f:id) rhs:expr)
     #'(dssl-struct-set! s f rhs)]
    [(_ i:id rhs:expr)
     (cond
       [(identifer-set!-error #'i)
        =>
        (λ (msg) (syntax-error #'i msg))]
       [else
         #'(set! i rhs)])]))

(define-for-syntax (identifer-set!-error id)
  (define id-info (identifier-binding id 0))
  (cond
    [(eq? id-info 'lexical) #f]
    [(pair? id-info)
      (define-values (_mod-path base-path)
        (module-path-index-split (car id-info)))
      (and base-path
           "cannot assign variable imported from another module")]
    [else
      "variable must be defined with ‘let’ before it can be assigned"]))

(define (dssl-vec-ref v i . rest)
  (cond
    [(get-method-value v '__index_ref__)
     =>
     (λ (index) (apply index i rest))]
    [(generic-base? v)
     (apply (generic-base-instantiate v) i rest)]
    [else
      (runtime-error "not a vector or indexable object: %p" v)]))

(define (dssl-vec-set! v is a)
  (cond
    [(get-method-value v '__index_set__)
     =>
     (λ (set) (apply set (append is (list a))))]
    [else
      (runtime-error "not a vector or indexable object: %p" v)]))

(define-syntax (dssl-struct/early stx)
  (syntax-parse stx
    #:context 'struct
    [(_ (name:id internal-name:id) . fields:unique-identifiers)
     #`(begin
         (struct internal-name struct-base (fields.var ...)
           #:mutable
           #:transparent)
         (dssl-provide #,(struct-predicate-name #'name))
         (define (#,(struct-predicate-name #'name) value)
           (#,(struct-predicate-name #'internal-name) value)))]))

(define-syntax dssl-begin
  (syntax-rules ()
    [(_ defn ...) (dssl-begin/acc () () defn ...)]))

(define-syntax (dssl-begin/acc stx)
  (syntax-parse stx
    #:literals (dssl-struct dssl-let begin)
    ; Done, splice together the early and late defns
    [(_ (early-defns ...) (late-defns ...))
     #'(begin
         early-defns ...
         late-defns ...)]
    ; Descend into begins
    [(_ (early-defns ...) (late-defns ...)
        (begin firsts ...)
        rest ...)
     #'(dssl-begin/acc (early-defns ...) (late-defns ...)
                       firsts ... rest ...)]
    ; Interpret structs
    [(_ (early-defns ...) (late-defns ...)
        (dssl-struct name:id (dssl-let :var&contract) ...)
        rest ...)
     (with-syntax ([s:cons (format-id #f "s:~a" #'name)])
       #`(dssl-begin/acc
           (early-defns
             ...
             (dssl-struct/early (name s:cons) var ...))
           (late-defns
             ...
             (dssl-struct/late (name s:cons) (var contract) ...))
           rest ...))]
    ; Pass everything else through
    [(_ (early-defns ...) (late-defns ...)
        first rest ...)
     #'(dssl-begin/acc (early-defns ...)
                       (late-defns ... first)
                       rest ...)]))

(define-syntax (dssl-struct stx)
  (syntax-error
    stx
    (string-append "saw dssl-struct, which should be changed to "
                   "dssl-struct/late by #%module-begin")))

(define-syntax (dssl-struct/late stx)
  (syntax-parse stx
    [(_ (name:id internal-name:id) (formal-field:id contract:expr) ...)
     (with-syntax ([s:cons #'internal-name]
                   [(setter-name ...)
                    (map (λ (field)
                            (format-id field "field ‘~a’ assignment" field))
                         (syntax->list #'(formal-field ...)))]
                   [(contract-name ...)
                    (generate-temporaries
                      (syntax->list #'(formal-field ...)))])
       #`(begin
           (define contract-name (ensure-contract 'struct contract))
           ...
           (define the-struct-info
             (struct-info
               'name
               (vector-immutable
                 (field-info
                   'formal-field
                   (struct-getter-name s:cons formal-field)
                   (racket:contract
                     (-> AnyC contract-name AnyC)
                     (struct-setter-name s:cons formal-field)
                     (format "field ~a of struct ~a" 'formal-field 'name)
                     "client performing assignment"
                     'name (get-srcloc contract)))
                 ...)))
           (dssl-provide name)
           (define/contract (name formal-field ...)
             (-> contract-name ... AnyC)
             (s:cons the-struct-info formal-field ...))
           ; The name on the next line is generated by the parser:
           (dssl-provide #,(struct-special-name #'name))
           (define-syntax (#,(struct-special-name #'name) stx)
             (syntax-parse stx
               [(ctor:id [field:id expr:expr] (... ...))
                #:fail-when (check-duplicate-identifier
                              (syntax->list #'(field (... ...))))
                "duplicate field name"
                (begin
                  (define actual-fields
                    (syntax->list #'(field (... ...))))
                  (define actual-exprs
                    (syntax->list #'(expr (... ...))))
                  (define field-exprs
                    (map (λ (f e) (cons (syntax-e f) e))
                         actual-fields actual-exprs))
                  (define formal-fields
                    (syntax->datum #'(formal-field ...)))
                  (define exprs
                    (for/list ([field (in-list formal-fields)])
                      (cond
                        [(assq field field-exprs) => cdr]
                        [else
                          (syntax-error
                            #'ctor
                            "struct requires field ~a"
                            field)])))
                  (for ([field (in-list actual-fields)])
                    (unless (memq (syntax-e field) formal-fields)
                      (syntax-error
                        #'ctor
                        "struct does not have field ~a"
                        (syntax-e field))))
                  #`(name #,@exprs))]))))]))

(define (get-field-info/or-else #:srclocs [srclocs '()] struct field)
  (or (get-field-info struct field)
      (runtime-error #:srclocs srclocs
                     "struct %p does not have field %s"
                     struct field)))

(define-syntax (get-method-value/or-else stx)
  (syntax-parse stx #:literals (quote)
    [(_ #:srclocs srclocs:expr object:expr (quote method:id))
     #'(let ([value object])
         (or (get-method-value value 'method)
             (runtime-error #:srclocs srclocs
                            "object %p does not have method %s"
                            object 'method)))]
    [(_ object:expr (quote method:id))
     #'(get-method-value/or-else #:srclocs '() object 'method)]))

(define-syntax (dssl-struct-ref stx)
  (syntax-parse stx
    [(_ target0:expr property:id)
     (syntax-parse (local-expand #'target0 'expression (list #'dssl-self))
       #:literals (dssl-self)
       [dssl-self
         #'(dssl-self property)]
       [target:expr
         #'(let ([value target])
             (cond
               [(struct-base? value)
                ((field-info-getter (get-field-info/or-else
                                      #:srclocs (get-srclocs expr)
                                      value 'property))
                 value)]
               [else
                (get-method-value/or-else #:srclocs (get-srclocs expr)
                                          value 'property)]))])]))

(define-syntax (dssl-struct-set! stx)
  (syntax-parse stx
    [(_ target:expr property:id rhs:expr)
     (syntax-parse (local-expand #'target 'expression (list #'dssl-self))
       #:literals (dssl-self)
       [dssl-self
         #'(dssl-self property rhs)]
       [struct
         (quasisyntax/loc #'property
           (let ([value struct])
             (cond
               [(struct-base? value)
                ((field-info-setter
                   (get-field-info/or-else
                     #:srclocs (get-srclocs struct)
                     value 'property))
                 value rhs)]
               [(object-base? value)
                (runtime-error
                  #:srclocs (get-srclocs struct)
                  "cannot assign to object properties from outside")]
               [else
                 (runtime-error #:srclocs (get-srclocs struct)
                                "value ‘%p’ is not a struct"
                                struct)])))])]))

(define-syntax (dssl-test stx)
  (syntax-parse stx
    [(_ name:expr body:expr ...+)
     #`(test-case (format "~a (line ~a)" name '#,(syntax-line stx))
                  (inc-total-tests!)
                  (dssl-begin body ...)
                  (inc-passed-tests!))]))

(define-syntax (dssl-time stx)
  (syntax-parse stx
    [(_ name:expr body:expr ...)
     #'(let ([lab name])
         (define-values (_lst cpu real gc)
           (time-apply (λ () (dssl-begin body ...)) '()))
         (printf "~a: cpu: ~a real: ~a gc: ~a\n" lab cpu real gc))]))

(define/contract (dssl-make-vec a b)
  (-> int? AnyC vec?)
  (make-vector a b))

(define (vec-lit . elements)
  (list->vector elements))

(define-syntax-rule (dssl-assert expr)
  (unless expr
    (assertion-error #:srclocs (get-srclocs expr)
                     'assert "did not evaluate to true")))

(define-syntax-rule (dssl-assert-eq e1 e2)
  (begin
    (define v1 e1)
    (define v2 e2)
    (unless (dssl-equal? v1 v2)
      (assertion-error #:srclocs (get-srclocs e1 e2)
                       'assert_eq "%p ≠ %p" v1 v2))))

(define (dssl-assert-error/thunk srclocs thunk string-pattern)
  (define pattern
    (regexp
      (regexp-quote string-pattern #false)))
  (define (handler exception)
    (if (regexp-match? pattern (exn-message exception))
      #false
      (format "errored as expected, but didn’t match the pattern\n message: ~a"
              (exn-message exception))))
  (define message (with-handlers ([exn:fail? handler])
                    (thunk)
                    "did not error as expected"))
  (when (string? message)
    (assertion-error #:srclocs srclocs 'assert-error message)))

(define-syntax (dssl-assert-error stx)
  (syntax-parse stx
    [(_ code:expr expected:expr)
     #`(dssl-assert-error/thunk (get-srclocs code) (λ () code) expected)]
    [(_ code:expr)
     #`(dssl-assert-error/thunk (get-srclocs code) (λ () code) "")]))

(define-syntax-parameter
  dssl-self
  (lambda (stx)
    (syntax-error stx "use of self outside of method")))

(define (contract-params-match? cs1 cs2)
  (for/and ([c1 (in-vector cs1)]
            [c2 (in-vector cs2)])
    (eq? c1 c2)))

(define-for-syntax (public-method-name? stx)
  (define name (symbol->string (syntax-e stx)))
  (or (not (string-prefix? name "_"))
      (string-prefix? name "__")))

(define-syntax (dssl-interface stx)
  (syntax-parse stx
    #:literals (dssl-def)
    [(_ name:id cvs:optional-contract-vars
        (dssl-def (method-name:id
                    method-cvs:optional-contract-vars
                    method-self:id
                    method-params:var&contract ...)
                  method-result:optional-return-contract) ...)
     #:fail-when (check-duplicate-identifier
                   (syntax->list #'(method-name ...)))
                 "duplicate method name"
     (for ([method-name (in-syntax #'(method-name ...))])
       (unless (public-method-name? method-name)
         (syntax-error method-name "interface methods cannot be private")))
     #`(begin
         (define interface-token (gensym))
         (define interface-method-table
           (for/hasheq
             ([srcloc
                (in-vector (vector (get-srcloc method-name) ...))]
              [each-method-name
                (in-vector (vector 'method-name ...))]
              [method-ctc
                (in-vector
                  (vector
                    (λ (cvs.var ...)
                       (square-bracket-proc-contract
                         method-name
                         [method-cvs.var ...]
                         (ensure-contract 'def method-params.contract)
                         ...
                         (ensure-contract 'def method-result.result)))
                    ...))])
             (values each-method-name (cons method-ctc srcloc))))
         (dssl-provide (for-syntax name))
         (define-for-syntax name
           (list
             #'interface-token
             (list 'method-name
                   (length (syntax->list #'(method-cvs.var ...)))
                   (length (syntax->list #'(method-params ...))))
             ...))
         (define (first-order? obj)
           (and (object-base? obj)
                (vector-memq interface-token
                             (object-info-interfaces
                               (object-base-info obj)))))
         (define (#,(struct-predicate-name #'name) obj)
           (and (first-order? obj)
                #t))
         (define (make-projection cvs.var ...)
           (define contract-parameters (vector-immutable cvs.var ...))
           (λ (blame)
              (λ (val neg-party)
                 (cond
                   [(and (object-base? val)
                         (assq interface-token
                               (object-base-contract-paramses val)))
                    =>
                    (λ (pair)
                       (if (contract-params-match?
                             (cdr pair)
                             contract-parameters)
                         val
                         (racket:raise-blame-error
                           blame #:missing-party neg-party val
                           (string-append
                             "cannot re-protect with with same interface"
                             " (~a) and different contract parameters")
                           'name)))]
                   [(first-order? val)
                    ((object-info-projector (object-base-info val))
                     val
                     (cons interface-token contract-parameters)
                     (λ (sel method)
                        (cond
                          [(hash-ref interface-method-table sel #f)
                           =>
                           (λ (pair)
                              (racket:contract
                                ((car pair) cvs.var ...) method
                                (format "method ~a of interface ~a"
                                        sel 'name)
                                "method caller"
                                #f
                                (cdr pair)))]
                          [else
                            (λ _
                               (racket:raise-blame-error
                                 (racket:blame-swap blame)
                                 #:missing-party neg-party val
                                 "interface ~a is protecting method ~a"
                                 'name sel))])))]
                   [(object-base? val)
                    (racket:raise-blame-error
                      blame #:missing-party neg-party val
                      "class ~a does not implement interface ~a"
                      (object-info-name (object-base-info val))
                      'name)]
                   [else
                     (racket:raise-blame-error
                       blame #:missing-party neg-party val
                       "value ~e does not implement interface ~a"
                       val 'name)]))))
         (dssl-provide #,(interface-contract-name #'name))
         (define #,(interface-contract-name #'name)
           (square-bracket-contract
             #,(interface-contract-name #'name)
             ([cvs.var AnyC] ...)
             #:first-order first-order?
             #:late-neg-projection
             (make-projection cvs.var ...))))]))

(define-syntax (define-field stx)
  (syntax-parse stx
    [(_ external-name:id internal-name:id actual-name:id contract:expr)
     #`(begin
         (define actual-name unsafe-undefined)
         (define-syntax internal-name
           (make-set!-transformer
             (λ (stx)
                (syntax-parse stx #:literals (set!)
                  [(set! _:id e:expr)
                   (quasisyntax/loc #'contract
                     (set! actual-name
                       #,(quasisyntax/loc #'e
                           (racket:contract
                             contract e
                             (format "field assignment at ~a"
                                     (srcloc->string (get-srcloc e)))
                             'external-name
                             'external-name
                             (get-srcloc contract)))))]
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
            [_:id #'dssl-self]
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

(define-for-syntax (lookup-interfaces class-name interfaces)
  (for/fold ([names     '()]
             [tokens    '()]
             [methodses '()])
            ([interface (in-syntax interfaces)])
    (unless (identifier-binding interface 1)
      (syntax-error interface "undefined interface"))
    (define interface-info (syntax-local-eval interface))
    (values (cons interface names)
            (cons (car interface-info) tokens)
            (cons (cdr interface-info) methodses))))

(define-for-syntax (check-class-against-interface
                     interface-name
                     interface-methods
                     class-name
                     class-methods
                     class-method-cvarses
                     class-method-paramses)
  (define class-method-names (map syntax-e class-methods))
  (for ([method-info (in-list interface-methods)])
    (unless (memq (car method-info) class-method-names)
      (syntax-error
        class-name
        "class missing method ~a, required by interface ~a"
        (car method-info)
        (syntax-e interface-name))))
  (for ([method-stx    (in-list class-methods)]
        [method-name   (in-list class-method-names)]
        [method-cvars  (in-list class-method-cvarses)]
        [method-params (in-list class-method-paramses)])
    (cond
      [(assq method-name interface-methods)
       =>
       (lambda (method-info)
         (define cvar-arity (length (syntax->list method-cvars)))
         (unless (= cvar-arity (cadr method-info))
           (syntax-error
             class-name
             "method ~a takes ~a contract params, but interface ~a specifies ~a"
             method-name
             cvar-arity
             (syntax-e interface-name)
             (cadr method-info)))
         (define actual-arity (length (syntax->list method-params)))
         (unless (= actual-arity (caddr method-info))
           (syntax-error
             class-name
             "method ~a takes ~a params, but interface ~a specifies ~a"
             method-name
             (add1 actual-arity)
             (syntax-e interface-name)
             (add1 (caddr method-info)))))])))

(define-syntax (define-class-predicate stx)
  (syntax-parse stx
    [(_ name:id internal-name:id [])
     #`(begin
         (dssl-provide #,(struct-predicate-name #'name))
         (define (#,(struct-predicate-name #'name) v)
           (#,(struct-predicate-name #'internal-name) v)))]
    [(_ name:id internal-name:id [cvs:id ...+])
     #`(begin
         (dssl-provide #,(struct-predicate-name #'name))
         (define #,(struct-predicate-name #'name)
           (square-bracket-proc
             #,(struct-predicate-name #'name)
             #:generic (cvs ...)
             (let ([contract-params (vector-immutable cvs ...)])
               (λ (value)
                  (cond
                    [(and (#,(struct-predicate-name #'internal-name) value)
                          (assq #f (object-base-contract-paramses value)))
                     =>
                     (λ (pair)
                        (contract-params-match? (cdr pair) contract-params))]
                    [else #f])))
             #:default #,(struct-predicate-name #'internal-name))))]))

(define-syntax (dssl-class stx)
  (syntax-parse stx
    #:literals (dssl-let dssl-def)
    [(_ name:id
        cvs:optional-contract-vars
        interfaces:optional-implements
        (dssl-let field:var&contract) ...
        (dssl-def (method-name:id
                    method-cvs:optional-contract-vars
                    method-self:id
                    method-params:var&contract ...)
                  method-result:optional-return-contract
                  method-body:expr ...) ...)
     #:fail-when (check-duplicate-identifier
                   (cons (datum->syntax #'name '__class__)
                         (syntax->list #'(method-name ...))))
                 "duplicate method name"
     ; Extract the defined names:
     (define field-names  (syntax->list #'(field.var ...)))
     (define method-names (syntax->list #'(method-name ...)))
     (define-values (constructor constructor-params)
       (find-constructor
         method-names
         #'((method-params.var ...) ...)
         #'name))
     ; Lookup and check interfaces:
     (define-values (interface-names interface-tokens interface-methodses)
       (lookup-interfaces #'name #'(interfaces.interface ...)))
     (for ([interface-name    interface-names]
           [interface-methods interface-methodses])
       (check-class-against-interface
         interface-name
         interface-methods
         #'name
         method-names
         (syntax->list #'((method-cvs.var ...) ...))
         (syntax->list #'((method-params ...) ...))))
     ; Generate new names:
     (define (self. property) (class-qualify #'name property))
     (with-syntax
       ([internal-name
          (format-id #f "c:~a" #'name)]
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
               (vector-immutable #,@interface-tokens)
               (vector-immutable
                 (method-info
                   '__class__
                   (struct-getter-name internal-name __class__))
                 (method-info
                   'public-method-name
                   (struct-getter-name internal-name public-method-name))
                 ...)))
           (define-class-predicate name internal-name [cvs.var ...])
           (dssl-provide name)
           (define name
             (square-bracket-class-lambda
               name ([cvs.var AnyC] ...) #,constructor-params
               (define-field field.var
                             self.field-name
                             actual-field-name
                             field.contract)
               ...
               (define-square-bracket-proc
                 ((self.method-name method-cvs.var ...)
                  [method-params.var
                    (ensure-contract 'def method-params.contract)] ...)
                 (ensure-contract 'def method-result.result)
                 (bind-self name method-self actual-self
                            (with-return method-body ...)))
               ...
               (define self.__class__ name)
               (define actual-self
                 (internal-name
                   internal-object-info
                   (list (cons #f (vector-immutable cvs.var ...)))
                   (λ () (vector-immutable
                            (cons 'field.var self.field-name) ...))
                   ; methods:
                   self.__class__
                   self.public-method-name ...))
               (#,(self. constructor) #,@constructor-params)
               (when (eq? unsafe-undefined actual-field-name)
                 (runtime-error
                   #:srclocs (get-srclocs #,constructor)
                   "constructor for class %s did not assign field %s"
                   'name 'field.var))
               ...
               actual-self))))]))

