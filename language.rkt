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
           [vec                 vec-lit]
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

(require "private/errors.rkt"
         "private/equal.rkt"
         "private/prims.rkt"
         "private/operators.rkt"
         "private/struct.rkt"
         "private/object.rkt"
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
         (only-in syntax/location quote-srcloc))
(require (prefix-in racket: racket/base)
         (prefix-in racket: racket/contract/base))

(require (for-syntax racket/base
                     syntax/parse
                     (only-in racket/syntax format-id)
                     (only-in racket/string string-prefix?)
                     "private/errors.rkt"
                     "private/find-lib.rkt"))

(define dssl-True #t)
(define dssl-False #f)

(define-syntax-parameter
  inc-passed-tests!
  (lambda (stx)
    (raise-syntax-error #f "use of inc-passed-tests!" stx)))

(define-syntax-parameter
  inc-total-tests!
  (lambda (stx)
    (raise-syntax-error #f "use of inc-total-tests!" stx)))

(define-syntax (dssl-module-begin stx)
  (syntax-case stx ()
    [(_ expr ...)
     #`(#%module-begin
        (module* configure-runtime racket/base
          (require dssl2/private/rte)
          (setup-rte))
        (#%provide
         (#,(datum->syntax stx 'all-defined-except) passed-tests total-tests))
        (define passed-tests 0)
        (define total-tests 0)
        (module+ test-info
          (provide get-test-info)
          (define (get-test-info)
            (values passed-tests total-tests)))
        (splicing-syntax-parameterize
          ([inc-passed-tests!  (syntax-rules ()
                                 [(_) (set! passed-tests (add1 passed-tests))])]
           [inc-total-tests!   (syntax-rules ()
                                 [(_) (set! total-tests (add1 total-tests))])])
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
    (raise-syntax-error #f "use of elif keyword" stx)))

; We define return (for lambda) as a syntax parameter, and then
; syntax-parameterize it inside dssl-lambda.
(define-syntax-parameter
  dssl-return
  (lambda (stx)
    (raise-syntax-error #f "use of return keyword not in a function" stx)))

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
    optional-return-contract
    #:description "optional return contract"
    (pattern (~seq #:-> result:expr))
    (pattern (~seq)
             #:with result #'AnyC))

  (define-splicing-syntax-class
    optional-implements
    #:description "optional #:implements clause"
    (pattern (~seq #:implements interface:id))
    (pattern (~seq)))

  (define-splicing-syntax-class
    optional-contract-vars
    #:description "optional forall-quantified contract variables"
    (pattern (~seq (~or #:forall #:∀) vars:unique-identifiers)
             #:with (var ...) #'(vars.var ...))
    (pattern (~seq)
             #:with (var ...) #'())))

(define-syntax-rule (ensure-contract who contract)
  (ensure-contract/fn (get-srclocs contract) who contract))

(define (ensure-contract/fn srclocs who contract)
  (if (contract? contract)
    contract
    (runtime-error #:srclocs srclocs
                   "~a: expected a contract\n got: ~e"
                   who contract)))

(define-simple-macro
  (dssl-def (f:id cvs:optional-contract-vars bs:var&contract ...)
            result-contract:optional-return-contract
            expr:expr ...)
   #:fail-when (check-duplicate-identifier
                 (syntax->list #'(cvs.var ... bs.var ...)))
               "duplicate argument name"
  (begin
    (define/contract
      f
      (maybe-parametric->/c [cvs.var ...]
                            (-> (ensure-contract 'def bs.contract)
                                ...
                                (ensure-contract 'def result-contract.result)))
      (lambda (bs.var ...)
        (let/ec return-f
                (syntax-parameterize
                  ([dssl-return (syntax-rules ()
                                  [(_)        (return-f (void))]
                                  [(_ result) (return-f result)])])
                  (dssl-begin expr ...)))))
    (make-set!able f)))

(define-syntax (maybe-parametric->/c stx)
  (syntax-parse stx
    [(_ [] contract:expr) #'contract]
    [(_ [cv:id ...] contract:expr)
     #'(parametric->/c [cv ...] contract)]))

(define-syntax (dssl-let stx)
  (syntax-parse stx
    [(_ :var&contract)
     #'(begin
         (define real-var unsafe-undefined)
         (make-set!able real-var)
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
         (define/contract var
                          (ensure-contract 'let contract)
                          expr)
         (make-set!able var))]))

; while uses two syntax parameters, break and continue (shared by for)
(define-syntax-parameter
  dssl-break
  (lambda (stx)
    (raise-syntax-error #f "use of break keyword not in a loop" stx)))

(define-syntax-parameter
  dssl-continue
  (lambda (stx)
    (raise-syntax-error #f "use of continue keyword not in a loop" stx)))

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
    [(vector? v)   (in-vector v)]
    [(natural? v)  (in-range v)]
    [(string? v)   (in-vector (explode v))]
    [else          (type-error #:srclocs srclocs
                               'for v "something iterable")]))

(define-syntax (dssl-in-value stx)
  (syntax-parse stx
    [(_ v:expr)
     #'(dssl-in-value/value (get-srclocs v) v)]))

(define-syntax (dssl-import stx)
  (define filename
    (syntax-parse stx
      [(_ lib:string) (syntax->datum #'lib)]
      [(_ lib:id)
       (path->string
         (build-path
           lib-directory
           (format "~a.rkt" (syntax->datum #'lib))))]))
  (datum->syntax stx `(#%require (file ,filename))))

; setf! is like Common Lisp setf, but it just recognizes three forms. We
; use this to translate assignments.
(define-syntax (dssl-= stx)
  (syntax-parse stx #:literals (dssl-vec-ref dssl-struct-ref)
    [(_ (dssl-vec-ref v:expr i:expr) rhs:expr)
     #'(vector-set! v i rhs)]
    [(_ (dssl-struct-ref s:expr f:id) rhs:expr)
     #'(dssl-struct-set! s f rhs)]
    [(_ i:id rhs:expr)
     #'(set! i rhs)]))

(define (dssl-vec-ref v i)
  (vector-ref v i))

(define-syntax (dssl-struct/early stx)
  (syntax-parse stx
    #:context 'struct
    [(_ (name:id internal-name:id) . fields:unique-identifiers)
     #`(begin
         (define-struct (internal-name struct-base) (fields.var ...)
                        #:mutable
                        #:transparent)
         (define (#,(format-id #'name "~a?" #'name) value)
           (#,(format-id #'internal-name "~a?" #'internal-name) value)))]))

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

(define-syntax (struct-constructor-name stx)
  (syntax-parse stx
    [(_ name:id)
     (format-id #'name "make-~a" #'name)]))

(define-syntax (struct-getter-name stx)
  (syntax-parse stx
    [(_ name:id field:id)
     (format-id #'name "~a-~a" #'name #'field)]))

(define-syntax (struct-setter-name stx)
  (syntax-parse stx
    [(_ name:id field:id)
     (format-id #'name "set-~a-~a!" #'name #'field)]))

(define-syntax (dssl-struct stx)
  (raise-syntax-error
    #f
    (string-append "Saw dssl-struct, which should be changed to "
                   "dssl-struct/late by #%module-begin")
    stx))

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
           (define struct-info
             (make-struct-info
               'name
               (vector-immutable
                 (make-field-info
                   'formal-field
                   (struct-getter-name s:cons formal-field)
                   (let ()
                     (define/contract setter-name
                       (rename-contract
                         (-> AnyC contract-name AnyC)
                         'assignment)
                       (struct-setter-name s:cons formal-field))
                     setter-name))
                 ...)))
           (define/contract (name formal-field ...)
             (-> contract-name ... AnyC)
             (s:cons struct-info formal-field ...))
           ; The name on the next line is generated by the parser:
           (define-syntax (#,(format-id #'name "m:~a" #'name) stx)
             (syntax-parse stx
               [(_ [field:id expr:expr] (... ...))
                #:fail-when (check-duplicate-identifier
                              (syntax->list #'(field (... ...))))
                "duplicate field name"
                (begin
                  (define actual-fields
                    (map syntax->datum (syntax->list #'(field (... ...)))))
                  (define actual-exprs
                    (syntax->list #'(expr (... ...))))
                  (define field-exprs
                    (map cons actual-fields actual-exprs))
                  (define formal-fields (syntax->list #'(formal-field ...)))
                  (define exprs
                    (for/list ([field formal-fields])
                      (cond
                        [(assq (syntax->datum field) field-exprs) => cdr]
                        [else
                          (raise-syntax-error
                            #f
                            (format "Struct ~e requires field ~a"
                                    'name (syntax->datum field))
                            stx)])))
                  (for ([field actual-fields])
                    (unless (memq field (map syntax->datum formal-fields))
                      (raise-syntax-error
                        #f
                        (format "Struct ~e does not have field ~a"
                                'name field)
                        field)))
                  #`(name #,@exprs))]))))]))

(define (get-field-info #:srclocs [srclocs '()] struct field)
  (let/ec return
    (define info-vector (struct-info-field-infos
                          (struct-base-struct-info struct)))
    (for ([info (in-vector info-vector)])
      (when (eq? field (field-info-name info))
        (return info)))
    (runtime-error #:srclocs srclocs
                   "Struct ~e does not have field ~a"
                   struct field)))

(define (get-method-info #:srclocs [srclocs '()] object method)
  (let/ec return
    (define info-vector (object-info-method-infos
                          (object-base-object-info object)))
    (for ([info (in-vector info-vector)])
      (when (eq? method (method-info-name info))
        (return info)))
    (runtime-error #:srclocs srclocs
                   "Object ~e does not have method ~a"
                   object method)))

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
                ((field-info-getter (get-field-info
                                      #:srclocs (get-srclocs expr)
                                      value 'property))
                 value)]
               [(object-base? value)
                ((method-info-getter (get-method-info
                                       #:srclocs (get-srclocs expr)
                                       value 'property))
                 value)]
               [else
                 (runtime-error #:srclocs (get-srclocs target)
                                "Value ‘~e’ is not a struct or object"
                                value)]))])]))

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
                   (get-field-info #:srclocs (get-srclocs struct)
                                   value 'property))
                 value rhs)]
               [(object-base? value)
                (runtime-error #:srclocs (get-srclocs struct)
                               "Cannot assign to object methods")]
               [else
                 (runtime-error #:srclocs (get-srclocs struct)
                                "Value ‘~e’ is not a struct"
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

(define (vec . args)
  (list->vector args))

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
                       'assert_eq "~e ≠ ~e" v1 v2))))

(define (dssl-assert-error/thunk srclocs thunk string-pattern)
  (define pattern (regexp (regexp-quote string-pattern #false)))
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

; This is so that the documentation will consider elif a keyword.
(define-syntax-parameter
  dssl-self
  (lambda (stx)
    (raise-syntax-error #f "use of self outside of method" stx)))

(define-syntax (dssl-interface stx)
  (syntax-parse stx
    #:literals (dssl-def)
    [(_ name:id cvs:optional-contract-vars
        (dssl-def (method-name:id
                    method-cvs:optional-contract-vars
                    method-self:id
                    method-params:var&contract ...)
                  result-contract:optional-return-contract) ...)
     #`(begin
         (printf "interface ~a:\n" 'name)
         (printf "\tdef ~a(~a, ...)\n" 'method-name 'method-self) ...)]))

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

(define-for-syntax (qualify qualifier property)
  (format-id qualifier "~a.~a" qualifier property))

(define-simple-macro (bind-self class:id self:id actual-self:id body:expr)
  (syntax-parameterize
    ([dssl-self
       (syntax-parser
         [(_ property:id)           (qualify #'class #'property)]
         [(_ property:id rhs:expr)  #`(set! #,(qualify #'class #'property) rhs)]
         [_:id                      #'actual-self])])
    (begin
      (define-syntax (self stx)
        (syntax-parse stx
          [_:id #'dssl-self]
          [_ (raise-syntax-error
               #f
               "self parameter is not a function"
               stx)]))
      body)))

(define-simple-macro (define/contract/immutable name:id ctc:expr rhs:expr)
  (begin
    (define real-name
      (let ([contract ctc]
            [name     rhs])
        (racket:contract
          contract name
          (format "method ~a at ~a" 'name (srcloc->string (get-srcloc name)))
          "caller")))
    (define-syntax name
      (make-set!-transformer
        (syntax-parser
          [(set! method _)
           (raise-syntax-error #f "cannot assign to method" #'method)]
          [_:id #'real-name]
          [(_:id . args) #'(real-name . args)])))))

(define-syntax (dssl-class stx)
  (syntax-parse stx
    #:literals (dssl-let dssl-def)
    #:datum-literals (__init__)
    [(_ name:id
        cvs:optional-contract-vars
        implements:optional-implements
        (dssl-let field:var&contract) ...
        (dssl-def (__init__
                    ctor-self:id
                    ctor-params:var&contract ...)
                  ctor-body:expr ...)
        (dssl-def (method-name:id
                    method-cvs:optional-contract-vars
                    method-self:id
                    method-params:var&contract ...)
                  method-result:optional-return-contract
                  body:expr ...) ...)
     (define (self. property) (qualify #'name property))
     (define (is-public? id)
       (define name (symbol->string (syntax->datum id)))
       (or (not (string-prefix? name "_"))
           (string-prefix? name "__")))
     (with-syntax
       ([actual-self (format-id #f "self")]
        [internal-name (format-id #f "c:~a" #'name)]
        [(actual-field-name ...)
           (map (λ (id) (format-id #'name "actual-~a" id))
                (syntax->list #'(field.var ...)))]
        [(self.field-name ...)
         (map self. (syntax->list #'(field.var ...)))]
        [(self.method-name ...)
         (map self. (syntax->list #'(method-name ...)))]
        [(public-method-name ...)
         (filter is-public? (syntax->list #'(method-name ...)))]
        [(self.public-method-name ...)
         (map self. (filter is-public?
                            (syntax->list #'(method-name ...))))])
       #`(begin
           (define-struct (internal-name object-base)
                          (__class__
                           __contract_params__
                           public-method-name
                           ...)
                          #:transparent)
           (define internal-object-info
             (make-object-info
               'name
               (vector-immutable
                 (make-method-info
                   '__class__
                   (struct-getter-name internal-name __class__))
                 (make-method-info
                   '__contract_params__
                   (struct-getter-name internal-name __contract_params__))
                 (make-method-info
                   'public-method-name
                   (struct-getter-name internal-name public-method-name))
                 ...)))
           (define (#,(format-id #'name "~a?" #'name) value)
             (#,(format-id #'internal-name "~a?" #'internal-name) value))
           (define/contract
             name
             (-> #,@(map (λ (_) #'contract?)
                         (syntax->list #'(cvs.var ...)))
                 (ensure-contract 'class ctor-params.contract)
                 ...
                 AnyC)
             (lambda (cvs.var ... ctor-params.var ...)
               (define-field field.var
                             self.field-name
                             actual-field-name
                             field.contract)
               ...
               (define actual-self unsafe-undefined)
               (define/contract/immutable
                 self.method-name
                 (maybe-parametric->/c
                   [method-cvs.var ...]
                   (-> (ensure-contract 'def method-params.contract)
                       ...
                       (ensure-contract 'def method-result.result)))
                 (lambda (method-params.var ...)
                   (let/ec return-f
                           (syntax-parameterize
                             ([dssl-return
                                (syntax-rules ()
                                  [(_)        (return-f (void))]
                                  [(_ result) (return-f result)])])
                             (bind-self name method-self actual-self
                               (dssl-begin body ...))))))
               ...
               (define __class__
                 name)
               (define (__contract_params__)
                 (vector cvs.var ...))
               (set! actual-self
                 ((struct-constructor-name internal-name)
                  internal-object-info
                  __class__
                  __contract_params__
                  self.public-method-name ...))
               (bind-self name ctor-self actual-self
                 (dssl-begin ctor-body ...))
               (when (eq? unsafe-undefined actual-field-name)
                 (runtime-error
                   #:srclocs (get-srclocs #'(ctor-body ...))
                   "Constructor for class ~a did not assign field ~a"
                   'name 'field.var))
               ...
               actual-self))))]))

