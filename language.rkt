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
           [else                else]
           [void                pass]
           [vec-lit             vec-lit]
           [dssl-True           True]
           [dssl-False          False]
           [dssl-None           None]
           [dssl-assert         assert]
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
           [dssl-if-e           if-e]
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

(require "private/class-system.rkt"
         "private/contract.rkt"
         "private/errors.rkt"
         "private/generic.rkt"
         "private/object.rkt"
         "private/operators.rkt"
         "private/names.rkt"
         "private/prims.rkt"
         "private/printer.rkt"
         "private/provide.rkt"
         "private/return.rkt"
         "private/singletons.rkt"
         "private/struct.rkt"
         racket/stxparam
         racket/splicing
         racket/contract/region
         (only-in racket/unsafe/undefined
                  unsafe-undefined
                  check-not-unsafe-undefined)
         syntax/parse/define
         (only-in rackunit
                  test-case)
         (only-in racket/contract/base
                  ->
                  contract
                  rename-contract)
         (only-in racket/contract/parametric
                  parametric->/c)
         (only-in racket/math
                  natural?)
         (only-in racket/sandbox
                  call-with-limits
                  exn:fail:resource?)
         (only-in syntax/location
                  quote-srcloc))
(require (prefix-in racket: racket/base)
         (prefix-in racket: racket/contract/base)
         (prefix-in racket: racket/contract/combinator))

(require (for-syntax racket/base
                     syntax/parse
                     (only-in racket/sequence in-syntax)
                     (only-in racket/syntax format-id)
                     (only-in racket/list make-list)
                     "private/interface.rkt"
                     "private/names.rkt"
                     "private/errors.rkt"
                     "private/find-lib.rkt"
                     "private/util.rkt"))


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

; This is so that the documentation will consider elif a keyword.
(define-syntax-parameter
  dssl-elif
  (lambda (stx)
    (syntax-error stx "use of elif keyword")))

(define-simple-macro (make-set!able f)
  (unless (zero? (random 1))
    (set! f (void))))

(define-syntax dssl-if
  (syntax-rules (else dssl-elif)
    [(_ [test0 result0 ...]
        [dssl-elif test result ...] ...
        [else else-result ...])
     (truthy-cond
       [test0 (dssl-begin result0 ...)]
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

  (define-syntax-class
    super-interface
    #:description "a superinterface, possibly with parameters"
    (pattern (name:id params:expr ...))
    (pattern name:id
             #:with (params ...) #'()))

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
      (with-return (dssl-begin expr ...)))))

(define-syntax (dssl-let stx)
  (syntax-parse stx
    [(_ var:id)
     #'(begin
         (define real-var unsafe-undefined)
         (make-set!able real-var)
         (dssl-provide var)
         (define-syntax var
           (make-set!-transformer
            (λ (stx)
              (syntax-parse stx #:literals (set!)
                [(set! _:id e:expr)
                 #'(set! real-var e)]
                [_:id
                 #'(check-not-unsafe-undefined real-var 'var)]
                [(_:id . args)
                 (with-syntax ([app (datum->syntax stx '#%app)])
                   #'(app
                       (check-not-unsafe-undefined real-var 'var)
                       . args))])))))]
    [(_ [var:id contract:expr])
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
    [(_ var:id rhs:expr)
     #'(begin
         (dssl-provide var)
         (define var rhs)
         (make-set!able var))]
    [(_ [var:id contract:expr] rhs:expr)
     #'(begin
         (dssl-provide var)
         (define/contract var
                          (ensure-contract 'let contract)
                          rhs)
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
        (when (truthy? test)
          (dssl-begin expr ...)
          (loop))))))

(define (get-try-advance srclocs obj who)
  (define iterator
    (dssl-send obj 'iterator
               #:or-else
               (type-error #:srclocs srclocs who obj
                           "object responding to .iterator()")))
  (get-method-value/or-else #:srclocs srclocs iterator 'try_advance))

(define-syntax (dssl-for stx)
  (syntax-parse stx
    [(_ [(i:id j:id) v:expr] expr:expr ...+)
     #:fail-when (and (bound-identifier=? #'i #'j) #'j)
                 "duplicate variable name"
     #'(let/ec break-f
         (let* ([obj  v]
                [next (get-try-advance (get-srclocs v) obj "for loop")])
           (let loop ([real-i 0])
             (when
               (let ([i real-i])
                 (next
                   (λ (j)
                      (let/ec continue-f
                              (syntax-parameterize
                                ([dssl-break
                                   (syntax-rules () [(_) (break-f (void))])]
                                 [dssl-continue
                                   (syntax-rules () [(_) (continue-f)])])
                                (dssl-begin expr ...))))))
               (loop (add1 real-i))))))]
    [(_ [i:id v:expr] expr:expr ...+)
     #'(dssl-for [(_ i) v] (dssl-begin expr ...))]))

(struct vector-builder (vector size) #:mutable)
(define (make-vector-builder)
  (vector-builder (make-vector 8 #false) 0))
(define (vector-builder-push-back vb element)
  (define old-vector (vector-builder-vector vb))
  (define old-capacity (vector-length old-vector))
  (define ix (vector-builder-size vb))
  (when (= ix old-capacity)
    (set-vector-builder-vector!
      vb
      (build-vector (* 2 old-capacity)
                    (λ (i)
                       (if (racket:< i old-capacity)
                         (vector-ref old-vector i)
                         #false)))))
  (vector-set! (vector-builder-vector vb)
               ix
               element)
  (set-vector-builder-size! vb (add1 ix)))
(define (vector-builder-copy vb)
  (define vector (vector-builder-vector vb))
  (build-vector (vector-builder-size vb)
                (λ (i) (vector-ref vector i))))

(define (dssl-for/vec/fun srclocs v when? body)
  (define next   (get-try-advance srclocs v "vector comprehension"))
  (define result (make-vector-builder))
  (let loop ([i 0])
    (when (next (λ (j)
                   (when (truthy? (when? i j))
                     (vector-builder-push-back result (body i j)))))
      (loop (add1 i))))
  (vector-builder-copy result))

(define-syntax (dssl-for/vec stx)
  (syntax-parse stx
    [(_ [j:id v:expr] expr:expr)
     #'(dssl-for/vec [(_ j) v] #:when #true expr)]
    [(_ [(i:id j:id) v:expr] expr:expr)
     #'(dssl-for/vec [(i j) v] #:when #true expr)]
    [(_ [j:id v:expr] #:when when:expr expr:expr)
     #'(dssl-for/vec [(_ j) v] #:when when expr)]
    [(_ [(i:id j:id) v:expr] #:when when expr:expr)
     #:fail-when (and (bound-identifier=? #'i #'j) #'j)
                 "duplicate variable name"
     #'(dssl-for/vec/fun (get-srclocs v)
                         v
                         (λ (i j) when)
                         (λ (i j) expr))]))

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

(begin-for-syntax
  (define (split-dssl-definitions stx0)
    (define (loop stx early late)
      (syntax-parse stx
        #:literals (dssl-struct dssl-let begin)
        [()
         (values early late)]
        [((begin . first) . rest)
         (let-values ([(early late) (loop #'first early late)])
           (loop #'rest early late))]
        [((dssl-struct name:id [dssl-let :var&contract] ...) . rest)
         (with-syntax ([s:cons (format-id #f "s:~a" #'name)])
           (loop
            #'rest
            (cons #'(dssl-struct/early (name s:cons) var ...)
                  early)
            (cons #'(dssl-struct/late (name s:cons) (var contract) ...)
                  late)))]
        [(first . rest)
         (loop #'rest early (cons #'first late))]))
    (define-values (early late) (loop stx0 '() '()))
    (values (reverse early) (reverse late)))

  (define (expand-dssl-begin stx)
    (define-values (early late) (split-dssl-definitions stx))
    #`(begin #,@early #,@late)))

(define-syntax-parser dssl-begin
  [(_ . defns)
   (expand-dssl-begin #'defns)])

(define-syntax (dssl-struct stx)
  (syntax-error
    stx
    (string-append "saw dssl-struct, which should be changed to "
                   "dssl-struct/late by #%module-begin")))

(define-syntax (dssl-struct/late stx)
  (syntax-parse stx
    [(_ (name:id internal-name:id) (formal-field:id contract:expr) ...)
     (define name-length (string-length (symbol->string (syntax-e #'name))))
     (with-syntax ([s:cons #'internal-name]
                   [(setter-name ...)
                    (map (λ (field)
                            (format-id field "field ‘~a’ assignment" field))
                         (syntax->list #'(formal-field ...)))]
                   [(contract-name ...)
                    (generate-temporaries
                      (syntax->list #'(formal-field ...)))])
       (syntax-property
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
                    #`(name #,@exprs))])))
         'sub-range-binders
         (vector (syntax-local-introduce (struct-special-name #'name))
                 0 name-length 0.5 0.5
                 (syntax-local-introduce #'name)
                 0 name-length 0.5 0.5)))]))

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
     (define target (local-expand #'target0 'expression (list #'dssl-self)))
     (syntax-parse target
       #:literals (dssl-self)
       [dssl-self
         (syntax-property
           #'(dssl-self property)
           'disappeared-use
           (syntax-property target 'disappeared-use))]
       [_
         #`(let ([value #,target])
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
    [(_ target0:expr property:id rhs:expr)
     (define target (local-expand #'target0 'expression (list #'dssl-self)))
     (syntax-parse target
       #:literals (dssl-self)
       [dssl-self
         (syntax-property
           #'(dssl-self property rhs)
           'disappeared-use
           (syntax-property target 'disappeared-use))]
       [_
         (quasisyntax/loc #'property
           (let ([value #,target])
             (cond
               [(struct-base? value)
                ((field-info-setter
                   (get-field-info/or-else
                     #:srclocs (get-srclocs #,target)
                     value 'property))
                 value rhs)]
               [(object-base? value)
                (runtime-error
                  #:srclocs (get-srclocs #,target)
                  "cannot assign to object properties from outside")]
               [else
                 (runtime-error #:srclocs (get-srclocs #,target)
                                "value ‘%p’ is not a struct"
                                #,target)])))])]))

(define-syntax (dssl-test stx)
  (syntax-parse stx
    [(_ name:expr body:expr ...+)
     #`(test-case
         (format "~a (line ~a)" name '#,(syntax-line stx))
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

(define-simple-macro (dssl-if-e e1:expr e2:expr e3:expr)
  (truthy-cond
    [e1   e2]
    [else e3]))

(begin-for-syntax
  (define-splicing-syntax-class timeout
    (pattern (~seq #:timeout seconds)
             #:declare seconds (expr/c #'positive?)))

  (define-syntax-class unary-operator
    #:attributes (name)
    (pattern (~literal not)
             #:attr name #'"not"))

  (define-syntax-class binary-operator
    #:attributes (name)
    (pattern (~literal ==)
             #:attr name #'"==")
    (pattern (~literal !=)
             #:attr name #'"!=")
    (pattern (~literal <=)
             #:attr name #'"<=")
    (pattern (~literal <)
             #:attr name #'"<")
    (pattern (~literal >=)
             #:attr name #'">=")
    (pattern (~literal >)
             #:attr name #'">")
    (pattern (~literal is)
             #:attr name #'"is")
    (pattern (~literal |is not|)
             #:attr name #'"is not")))

(define (call-with-timeout srclocs seconds thunk)
  (with-handlers
    ([exn:fail:resource?
       (λ (_exn)
          (assertion-error #:srclocs srclocs "ran out of time"))])
    (call-with-limits seconds #f thunk)))

(define-simple-macro (with-timeout loc time:expr body:expr)
  (call-with-timeout (get-srclocs loc) time (λ () body)))

(define-syntax-parser dssl-assert
  [(_ e:expr time:timeout)
   #'(with-timeout e time.seconds (dssl-assert e))]
  [(_ (~and e (op:binary-operator e1:expr e2:expr)))
   #'(let ()
       (define v1 e1)
       (define v2 e2)
       (when (falsy? (op v1 v2))
         (assertion-error #:srclocs (get-srclocs e)
                          "%p %s %p" v1 op.name v2)))]
  [(_ (~and e (op:unary-operator e1:expr)))
   #'(let ()
       (define v1 e1)
       (when (falsy? (op v1))
         (assertion-error #:srclocs (get-srclocs e)
                          "%s %p" op.name v1)))]
  [(_ e:expr)
   #'(let ()
       (define v e)
       (when (falsy? v)
         (assertion-error #:srclocs (get-srclocs e)
                          "%p" v)))])

(define (dssl-assert-error/thunk srclocs thunk str-pattern)
  (define re-pattern (regexp (regexp-quote str-pattern #f)))
  (define (handler exception)
    (if (regexp-match? re-pattern (exn-message exception))
      #f
      (format "~a\n ~a~s\n ~a~s"
              "got a different error than expected:"
              "error message:  "
              (exn-message exception)
              "should contain: "
              str-pattern)))
  (define message
    (with-handlers ([exn:fail? handler])
      (thunk)
      "did not error as expected"))
  (when message (assertion-error #:srclocs srclocs message)))

(define-syntax (dssl-assert-error stx)
  (syntax-parse stx
    [(_ code:expr msg:expr time:timeout)
     #'(with-timeout code time.seconds (dssl-assert-error code msg))]
    [(_ code:expr msg:expr)
     #`(dssl-assert-error/thunk (get-srclocs code) (λ () code) msg)]
    [(_ code:expr time:timeout)
     #'(dssl-assert-error code "" #:timeout time.seconds)]
    [(_ code:expr)
     #'(dssl-assert-error code "")]))

(define-syntax (dssl-interface stx)
  (syntax-parse stx
    #:literals (dssl-def)
    [(_ name:id
        cvs:optional-contract-vars
        (super:super-interface ...)
        (dssl-def (method-name:id
                    method-cvs:optional-contract-vars
                    method-self:id
                    method-params:var&contract ...)
                  method-result:optional-return-contract) ...)
     #'(define-dssl-interface
         name
         (cvs.var ...)
         ((super.name super.params ...) ...)
         ([method-name
            (method-cvs.var ...)
            (method-params.contract ...)
            method-result.result]
          ...))]))

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
     #'(define-dssl-class
         name
         (cvs.var ...)
         (interfaces.interface ...)
         ([field.var field.contract]
          ...)
         ([method-name
            (method-cvs.var ...)
            method-self
            ([method-params.var method-params.contract]
             ...)
            method-result.result
            (dssl-begin method-body ...)]
          ...))]))

