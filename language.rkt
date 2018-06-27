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
         (prefix-in racket: racket/contract/base)
         (prefix-in racket: racket/contract/combinator))

(require (for-syntax racket/base
                     syntax/parse
                     (only-in racket/syntax format-id syntax-local-eval)
                     (only-in racket/string string-prefix?)
                     "private/errors.rkt"
                     "private/find-lib.rkt"))

(define dssl-True #t)
(define dssl-False #f)

(define-syntax-parameter
  inc-passed-tests!
  (lambda (stx)
    (syntax-error stx "use of inc-passed-tests!")))

(define-syntax-parameter
  inc-total-tests!
  (lambda (stx)
    (syntax-error stx "use of inc-total-tests!")))

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
    optional-return-contract
    #:description "optional return contract"
    (pattern (~seq #:-> result:expr))
    (pattern (~seq)
             #:with result #'AnyC))

  (define-splicing-syntax-class
    optional-implements
    #:description "optional #:implements clause"
    (pattern (~seq #:implements interface:id))
    (pattern (~seq)
             #:with interface #f))

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
    (define/contract
      f
      (maybe-parametric->/c [cvs.var ...]
                            (-> (ensure-contract 'def bs.contract)
                                ...
                                (ensure-contract 'def result-contract.result)))
      (lambda (bs.var ...)
        (with-return expr ...)))
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

(define-syntax (struct-constructor-name stx)
  (syntax-parse stx
    [(_ name:id)
     (format-id #'name "make-~a" #'name)]))

(define-for-syntax (struct-predicate-name name)
  (format-id name "~a?" name))

(define-syntax (struct-getter-name stx)
  (syntax-parse stx
    [(_ name:id field:id)
     (format-id #'name "~a-~a" #'name #'field)]))

(define-syntax (struct-setter-name stx)
  (syntax-parse stx
    [(_ name:id field:id)
     (format-id #'name "set-~a-~a!" #'name #'field)]))

(define-syntax (dssl-struct stx)
  (syntax-error
    stx
    (string-append "Saw dssl-struct, which should be changed to "
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
           (define struct-info
             (make-struct-info
               'name
               (vector-immutable
                 (make-field-info
                   'formal-field
                   (struct-getter-name s:cons formal-field)
                   (racket:contract
                     (-> AnyC contract-name AnyC)
                     (struct-setter-name s:cons formal-field)
                     (format "field ~a of struct ~a" 'formal-field 'name)
                     "client performing assignment"
                     'name (get-srcloc contract)))
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
                          (syntax-error
                            stx "Struct ~e requires field ~a"
                            'name (syntax->datum field))])))
                  (for ([field actual-fields])
                    (unless (memq field (map syntax->datum formal-fields))
                      (syntax-error
                        field
                        "Struct ~e does not have field ~a"
                        'name field)))
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
    (syntax-error stx "use of self outside of method")))

(define (contract-parameters-match? cs1 cs2)
  (for/and ([c1 (in-vector cs1)]
            [c2 (in-vector cs2)])
    (eq? c1 c2)))

(define-for-syntax (public-method-name? name)
  (define str (symbol->string name))
  (or (not (string-prefix? str "_"))
      (string-prefix? str "__")))

(define-syntax (dssl-interface stx)
  (syntax-parse stx
    #:literals (dssl-def)
    [(_ name:id cvs:optional-contract-vars
        (dssl-def (method-name:id
                    method-cvs:optional-contract-vars
                    method-self:id
                    method-params:var&contract ...)
                  method-result:optional-return-contract) ...)
     (for ([method-name (syntax->list #'(method-name ...))])
       (unless (public-method-name? (syntax->datum method-name))
         (syntax-error method-name "interface methods cannot be private")))
     #`(begin
         (define interface-token (gensym))
         (define-struct (interface-struct object-base)
           [method-name ...])
         (define interface-info
           (make-object-info 'name interface-token
                             (vector-immutable
                               (make-method-info
                                 'method-name
                                 (struct-getter-name interface-struct
                                                     method-name))
                               ...)))
         (define-for-syntax name
           (list
             #'interface-token
             (list 'method-name
                   (length (syntax->list #'(method-params ...))))
             ...))
         (define (first-order? obj)
           (and (object-base? obj)
                (eq? interface-token (object-info-interface
                                       (object-base-object-info obj)))))
         (define (project-method object method contract srcloc)
           (define method-value
             ((method-info-getter (get-method-info object method))
              object))
           (racket:contract
             contract method-value
             'name "method caller"
             (format "~a.~a" 'name method) srcloc))
         (define (make-projection cvs.var ...)
           (define contract-parameters (vector-immutable cvs.var ...))
           (λ (blame)
              (λ (val neg-party)
                 (cond
                   [(#,(struct-predicate-name #'name) val)
                    (if (contract-parameters-match?
                          (object-base-contract-params val)
                          contract-parameters)
                      val
                      (racket:raise-blame-error
                        blame #:missing-party neg-party val
                        (string-append
                          "Value ~e already implements "
                          "interface ~a with contract parameters ~e")
                        val 'name (object-base-contract-params val)))]
                   [(first-order? val)
                    (make-interface-struct
                      interface-info
                      contract-parameters
                      (project-method
                        val
                        'method-name
                        (maybe-parametric->/c
                          [method-cvs.var ...]
                          (-> (ensure-contract 'def method-params.contract)
                              ...
                              (ensure-contract 'def method-result.result)))
                        (get-srcloc method-name))
                      ...)]
                   [(object-base? val)
                    (racket:raise-blame-error
                      blame #:missing-party neg-party val
                      "Class ~a does not implement interface ~a"
                      (object-info-name (object-base-object-info val))
                      'name)]
                   [else
                     (racket:raise-blame-error
                       blame #:missing-party neg-party val
                       "Value ~e does not implement interface ~a"
                       val 'name)]))))
         #,(if (null? (syntax->list #'cvs))
             #'(define name
                 (racket:make-contract
                   #:name 'name
                   #:first-order first-order?
                   #:late-neg-projection (make-projection)))
             #'(define (name cvs.var ...)
                 (racket:make-contract
                   #:name 'name
                   #:first-order first-order?
                   #:late-neg-projection (make-projection cvs.var ...))))
         (define (#,(struct-predicate-name #'name) value)
           (interface-struct? value)))]))

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
          [_ (syntax-error stx "self parameter is not a function")]))
      body)))

(define-simple-macro (define/contract/immutable name:id ctc:expr rhs:expr)
  (begin
    (define real-name
      (let ([contract ctc]
            [name     rhs])
        (racket:contract
          contract name
          (format "method ~a at ~a" 'name (srcloc->string (get-srcloc name)))
          "method caller"
          'name (get-srcloc ctc))))
    (define-syntax name
      (make-set!-transformer
        (syntax-parser
          [(set! method _)
           (syntax-error #'method "cannot assign to method")]
          [_:id #'real-name]
          [(_:id . args) #'(real-name . args)])))))

(define-for-syntax (find-constructor method-names stx)
  (let/ec return
    (for ([method-name (in-list method-names)])
      (when (eq? '__init__ (syntax-e method-name))
        (return method-name)))
    (syntax-error stx "class must have a constructor")))

(define-syntax (dssl-class stx)
  (syntax-parse stx
    #:literals (dssl-let dssl-def)
    [(_ name:id
        cvs:optional-contract-vars
        implements:optional-implements
        (dssl-let field:var&contract) ...
        (dssl-def (method-name:id
                    method-cvs:optional-contract-vars
                    method-self:id
                    method-params:var&contract ...)
                  method-result:optional-return-contract
                  method-body:expr ...) ...)
     (define constructor
       (find-constructor (syntax->list #'(method-name ...)) #'name))
     (define interface-name (syntax-e #'implements.interface))
     (define interface-info
       (if interface-name (syntax-local-eval interface-name) '(#f)))
     (define interface-token (car interface-info))
     (define interface-methods (cdr interface-info))
     (define method-names
       (map syntax-e (syntax->list #'(method-name ...))))
     (for ([method-info interface-methods])
       (unless (memq (car method-info) method-names)
         (syntax-error
           #'implements.interface
           "Class ~a missing method ~a, required by interface"
           (syntax-e #'name) (car method-info))))
     (for ([method-name (syntax->list #'(method-name ...))]
           [method-params (syntax->list #'((method-params ...) ...))])
       (cond
         [(assq (syntax-e method-name) interface-methods)
          =>
          (lambda (method-info)
            (define actual-arity (length (syntax->list method-params)))
            (unless (= actual-arity (cadr method-info))
              (syntax-error
                method-name
                "Method ~a takes ~a params, but interface ~a specifies ~a"
                (syntax-e method-name) (add1 actual-arity)
                interface-name (add1 (cadr method-info)))))]))
     (define (self. property) (qualify #'name property))
     (define (is-public? id)
       (public-method-name? (syntax->datum id)))
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
                           public-method-name
                           ...)
                          #:transparent)
           (define internal-object-info
             (make-object-info
               'name
               #,interface-token
               (vector-immutable
                 (make-method-info
                   '__class__
                   (struct-getter-name internal-name __class__))
                 (make-method-info
                   'public-method-name
                   (struct-getter-name internal-name public-method-name))
                 ...)))
           (define (#,(struct-predicate-name #'name) value)
             (#,(struct-predicate-name #'internal-name) value))
           (define (name cvs.var ... . rest)
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
                 (bind-self name method-self actual-self
                   (with-return method-body ...))))
             ...
             (define self.__class__ name)
             (set! actual-self
               ((struct-constructor-name internal-name)
                internal-object-info
                (vector-immutable cvs.var ...)
                self.__class__
                self.public-method-name ...))
             (apply #,(self. constructor) rest)
             (when (eq? unsafe-undefined actual-field-name)
               (runtime-error
                 #:srclocs (get-srclocs constructor)
                 "Constructor for class ~a did not assign field ~a"
                 'name 'field.var))
             ...
             actual-self)))]))

