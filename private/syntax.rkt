#lang racket/base

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
          [dssl-while          while]))

(require "class-system.rkt"
         "contract.rkt"
         "errors.rkt"
         "generic.rkt"
         "object.rkt"
         "names.rkt"
         "printer.rkt"
         "provide.rkt"
         "stxparams.rkt"
         "singletons.rkt"
         "struct.rkt"
         (prefix-in p: "prims.rkt")
         (only-in syntax/parse/define
                  define-simple-macro
                  define-syntax-parser)
         (only-in racket/unsafe/undefined
                  unsafe-undefined
                  check-not-unsafe-undefined)
         (only-in rackunit
                  test-case)
         (only-in racket/contract/base
                  ->
                  contract
                  rename-contract)
         (only-in racket/contract/parametric
                  parametric->/c)
         (only-in racket/contract/region
                  define/contract)
         (only-in racket/format
                  ~a)
         (only-in racket/math
                  natural?)
         (only-in racket/sandbox
                  with-deep-time-limit
                  exn:fail:resource?)
         (only-in racket/string
                  string-contains?)
         (only-in syntax/location
                  quote-srcloc))

(require (for-syntax racket/base
                     syntax/parse
                     (only-in racket/format
                              ~a)
                     (only-in racket/sequence
                              in-syntax)
                     (only-in racket/syntax
                              format-id)
                     (only-in racket/list
                              make-list)
                     "interface.rkt"
                     "names.rkt"
                     "errors.rkt"
                     "find-lib.rkt"
                     "util.rkt"
                     "syntax-classes.rkt"))

(define-syntax (dssl-module-begin stx)
  (syntax-case stx ()
    [(_ expr ...)
     #`(#%module-begin
        (module* configure-runtime racket/base
          (require dssl2/private/rte)
          (setup-rte))
        (module test-info racket/base
          (provide passed-tests total-tests inc-passed! inc-total!)
          (define passed-tests 0)
          (define total-tests 0)
          (define (inc-passed!) (set! passed-tests (add1 passed-tests)))
          (define (inc-total!) (set! total-tests (add1 total-tests))))
        (require 'test-info)
        (with-test-counters [inc-passed! inc-total!]
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
(define-syntax (dssl-elif stx)
  (syntax-error stx "bad use of elif keyword"))

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
    [(_ (param:var ...) expr:expr ...)
     (quasisyntax/loc stx
       (with-masked-control
           (lambda (param.id ...) (dssl-begin expr ...))))]))

(define-simple-macro
  (dssl-def (f:real-id cvs:opt-ctc-vars bs:var&ctc ...)
            result-ctc:opt-return-ctc
            expr:expr ...)
  #:fail-when (check-duplicate-identifier
               (syntax->list #'(cvs.var ... bs.var ...)))
  "duplicate argument name"
  (begin
    (dssl-provide f)
    (define-square-bracket-proc
      ((f cvs.var ...) [bs.var (ensure-contract 'def bs.ctc)] ...)
      (ensure-contract 'def result-ctc.result)
      (wrap-procedure-body (dssl-begin expr ...)))))

(define-syntax (dssl-let stx)
  (syntax-parse stx
    [(_ var:real-id)
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
    [(_ [var:real-id ctc:expr])
     #'(begin
         (define real-var unsafe-undefined)
         (make-set!able real-var)
         (dssl-provide var)
         (define-syntax var
           (make-set!-transformer
            (λ (stx)
              (syntax-parse stx #:literals (set!)
                [(set! _:id e:expr)
                 (quasisyntax/loc #'ctc
                   (set! real-var
                         #,(quasisyntax/loc #'e
                             (contract
                              ctc e
                              (format "assignment at ~a"
                                      (srcloc->string (get-srcloc e)))
                              'var
                              'var (get-srcloc ctc)))))]
                [_:id
                 #'(check-not-unsafe-undefined real-var 'var)]
                [(_:id . args)
                 (with-syntax ([app (datum->syntax stx '#%app)])
                   #'(app
                      (check-not-unsafe-undefined real-var 'var)
                      . args))])))))]
    [(_ var:var rhs:expr)
     #'(begin
         (dssl-provide var.id)
         (define var.id rhs)
         (make-set!able var.id))]
    [(_ [var:var ctc:expr] rhs:expr)
     #'(begin
         (dssl-provide var.id)
         (define/contract var.id
           (ensure-contract 'let ctc)
           rhs)
         (make-set!able var.id))]))

(define-syntax-rule (dssl-while test expr ...)
  (with-break
      (let loop ()
        (when (truthy? test)
          (with-continue
              (dssl-begin expr ...))
          (loop)))))

(define-syntax (dssl-for stx)
  (syntax-parse stx
    [(_ [(i:var j:var) v:expr] expr:expr ...+)
     #:fail-when (and (bound-identifier=? #'i.id #'j.id) #'j.id)
     "duplicate variable name"
     #'(with-break
           (dssl-for/fun
            (get-srclocs v)
            v
            (dssl-for-loop-body (i.id j.id) expr ...)))]
    [(_ [i:var v:expr] expr:expr ...+)
     #'(dssl-for [(_ i.id) v] expr ...)]))

(define (dssl-for/fun srclocs obj body)
  (define next (p:get-try-advance srclocs obj "for loop"))
  (let loop ([my-i 0])
    (when (next (body my-i))
      (loop (add1 my-i)))))

(define-simple-macro (dssl-for-loop-body (i j) body:expr ...)
  (λ (i)
    (λ (j)
      (with-continue
          (dssl-begin body ...)))))

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
                     (if (< i old-capacity)
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
  (define next   (p:get-try-advance srclocs v "vector comprehension"))
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
    [(_ [(i:var j:var) v:expr] #:when when expr:expr)
     #:fail-when (and (bound-identifier=? #'i.id #'j.id) #'j.id)
     "duplicate variable name"
     #'(dssl-for/vec/fun (get-srclocs v)
                         v
                         (λ (i.id j.id) when)
                         (λ (i.id j.id) expr))]))

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
    [(_ (dssl-struct-ref s:expr f:real-id) rhs:expr)
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
    [(p:get-method-value v '__index_ref__)
     =>
     (λ (index) (apply index i rest))]
    [(generic-base? v)
     (apply (generic-base-instantiate v) i rest)]
    [else
     (runtime-error "not a vector or indexable object: %p" v)]))

(define (dssl-vec-set! v is a)
  (cond
    [(p:get-method-value v '__index_set__)
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
        [((dssl-struct name:id [dssl-let :var&ctc] ...) . rest)
         (with-syntax ([s:cons (format-id #f "s:~a" #'name)])
           (loop
            #'rest
            (cons #'(dssl-struct/early (name s:cons) var ...)
                  early)
            (cons #'(dssl-struct/late (name s:cons) (var ctc) ...)
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
   (~a "saw dssl-struct, which should be changed to "
       "dssl-struct/late by #%module-begin")))

(define-syntax (dssl-struct/late stx)
  (syntax-parse stx
    [(_ (name:id internal-name:id) (formal-field:real-id ctc:expr) ...)
     (define name-length (string-length (~a (syntax-e #'name))))
     (with-syntax ([s:cons #'internal-name]
                   [(setter-name ...)
                    (map (λ (field)
                           (format-id field "field ‘~a’ assignment" field))
                         (syntax->list #'(formal-field ...)))]
                   [(ctc-name ...)
                    (generate-temporaries
                     (syntax->list #'(formal-field ...)))])
       (syntax-property
        #`(begin
            (define ctc-name (ensure-contract 'struct ctc))
            ...
            (define the-struct-info
              (struct-info
               'name
               (vector-immutable
                (field-info
                 'formal-field
                 (struct-getter-name s:cons formal-field)
                 (contract
                  (-> AnyC ctc-name AnyC)
                  (struct-setter-name s:cons formal-field)
                  (format "field ~a of struct ~a" 'formal-field 'name)
                  "client performing assignment"
                  'name (get-srcloc ctc)))
                ...)))
            (dssl-provide name)
            (define/contract (name formal-field ...)
              (-> ctc-name ... AnyC)
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
               (p:get-method-value/or-else #:srclocs (get-srclocs expr)
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
  (-> p:nat? AnyC p:vec?)
  (make-vector a b))

(define (vec-lit . elements)
  (list->vector elements))

(define-simple-macro (dssl-if-e e1:expr e2:expr e3:expr)
  (truthy-cond
   [e1   e2]
   [else e3]))


(define current-dssl-assertion-timeout
  (make-parameter +inf.0))

(define-simple-macro (with-timeout loc time:expr body:expr ...+)
  (call-with-timeout (get-srclocs loc) time (λ () body ...)))

(define (call-with-timeout srclocs seconds0 thunk)
  (define seconds
    (or seconds0 (current-dssl-assertion-timeout)))
  (define (handle _)
    (assertion-error
     #:srclocs srclocs
     "out of time\n timeout: %p seconds"
     seconds))
  (cond
    [(= +inf.0 seconds) (thunk)]
    [else
     (with-handlers ([exn:fail:resource? handle])
       (call-with-deep-time-limit/exceptions seconds thunk))]))

(define (call-with-deep-time-limit/exceptions seconds thunk)
  (define finish (λ () (error "IMPOSSIBLE! Please report this bug.")))
  (define (handle exn) (set! finish (λ () (raise exn))))
  (with-deep-time-limit seconds
    (with-handlers ([(λ (_) #t) handle])
      (define result (thunk))
      (set! finish (λ () result))))
  (finish))

(define-syntax-parser dssl-assert
  ; changing the default timeout
  [(_ timeout:req-timeout)
   #'(current-dssl-assertion-timeout timeout.seconds)]
  ; binary operators:
  [(_ (~and e (op:binary-operator e1:expr e2:expr))
      timeout:opt-timeout)
   #'(with-timeout e timeout.seconds
       (define v1 e1)
       (define v2 e2)
       (when (falsy? (op v1 v2))
         (assertion-error #:srclocs (get-srclocs e)
                          "%p %s %p" v1 op.name v2)))]
  ; unary operators:
  [(_ (~and e (op:unary-operator e1:expr))
      timeout:opt-timeout)
   #'(with-timeout e timeout.seconds
       (define v1 e1)
       (when (falsy? (op v1))
         (assertion-error #:srclocs (get-srclocs e)
                          "%s %p" op.name v1)))]
  ; arbitrary expressions:
  [(_ e:expr timeout:opt-timeout)
   #'(with-timeout e timeout.seconds
       (define v e)
       (when (falsy? v)
         (assertion-error #:srclocs (get-srclocs e)
                          "%p" v)))])

(define (dssl-assert-error/thunk srclocs thunk pattern)
  (define (handler exn)
    (if (string-contains? (exn-message exn) pattern)
        #f
        (~a "got a different error than expected:\n"
            " error message:  " (exn-message exn) "\n"
            " should contain: " pattern)))
  (define message
    (with-handlers ([exn:fail? handler])
      (thunk)
      "did not error as expected"))
  (when message (assertion-error #:srclocs srclocs message)))

(define-syntax (dssl-assert-error stx)
  (syntax-parse stx
    [(_ code:expr msg:expr timeout:opt-timeout)
     #`(with-timeout code timeout.seconds
         (dssl-assert-error/thunk (get-srclocs code)
                                  (λ () code)
                                  msg))]
    [(_ code:expr timeout:opt-timeout)
     #'(dssl-assert-error code "" . timeout)]))

(define-syntax (dssl-interface stx)
  (syntax-parse stx
    #:literals (dssl-def)
    [(_ name:real-id
        cvs:opt-ctc-vars
        (super:super-interface ...)
        (dssl-def (method-name:real-id
                   method-cvs:opt-ctc-vars
                   method-self:var
                   method-params:var&ctc ...)
                  method-result:opt-return-ctc) ...)
     #'(define-dssl-interface
         name
         (cvs.var ...)
         ((super.name super.params ...) ...)
         ([method-name
           (method-cvs.var ...)
           (method-params.ctc ...)
           method-result.result]
          ...))]))

(define-syntax (dssl-class stx)
  (syntax-parse stx
    #:literals (dssl-let dssl-def)
    [(_ name:real-id
        cvs:opt-ctc-vars
        interfaces:opt-implements
        (dssl-let field:var&ctc) ...
        (dssl-def (method-name:real-id
                   method-cvs:opt-ctc-vars
                   method-self:var
                   method-params:var&ctc ...)
                  method-result:opt-return-ctc
                  method-body:expr ...) ...)
     #'(define-dssl-class
         name
         (cvs.var ...)
         (interfaces.interface ...)
         ([field.var field.ctc]
          ...)
         ([method-name
           (method-cvs.var ...)
           method-self.id
           ([method-params.var method-params.ctc]
            ...)
           method-result.result
           (dssl-begin method-body ...)]
          ...))]))

