#lang racket/base

(provide (rename-out
          ; special syntax
          [dssl-module-begin           #%module-begin]
          [dssl-top-interaction        #%top-interaction]
          [dssl-app                    #%app]
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
         (only-in "rte.rkt" current-dssl-test-points)
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
         (only-in racket/port
                  with-output-to-string)
         (only-in racket/sandbox
                  with-deep-time-limit
                  exn:fail:resource?)
         (only-in racket/string
                  string-contains?
                  string-replace)
         (only-in syntax/location
                  quote-srcloc))

(require (for-syntax racket/base
                     syntax/parse
                     (only-in racket/exn
                              exn->string)
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
          (require (only-in dssl2/private/rte setup-rte))
          (setup-rte))
        (module test-info racket/base
          (require (only-in dssl2/private/rte
                            current-dssl-test-points))
          (provide passed-tests
                   total-tests
                   actual-points
                   possible-points
                   inc-case-number!
                   inc-passed!
                   inc-total!)
          (define test-case-number 0)
          (define passed-tests 0)
          (define total-tests 0)
          (define actual-points 0)
          (define possible-points 0)
          (define (inc-case-number!)
            (set! test-case-number (add1 test-case-number))
            test-case-number)
          (define (inc-passed!)
            (set! passed-tests (add1 passed-tests))
            (define test-points (current-dssl-test-points))
            (when (number? test-points)
              (set! actual-points (+ test-points actual-points))))
          (define (inc-total!)
            (set! total-tests (add1 total-tests))
            (define test-points (current-dssl-test-points))
            (when (number? test-points)
              (set! possible-points (+ test-points possible-points)))))
        (require 'test-info)
        (with-test-counters [inc-passed! inc-total! inc-case-number!]
          (dssl-begin expr ...))
        (print-test-results passed-tests
                            total-tests
                            actual-points
                            possible-points))]))

(define-syntax-parser dssl-app
  [(~and e (_ arg ...+))
   #'(with-error-context (e)
       (#%app arg ...))])

(define (print-test-results passed total actual possible)
  (cond
    [(zero? total)       (void)]
    [(= passed total 1)  (printf "The only test passed\n")]
    [(= passed total 2)  (printf "Both tests passed\n")]
    [(= total 1)         (printf "The only test failed\n")]
    [(and (= total 2) (zero? passed))
     (printf "Both tests failed\n")]
    [(= passed total)    (printf "All ~a tests passed\n" total)]
    [(zero? passed)      (printf "All ~a tests failed\n" total)]
    [else                (printf "~a of ~a tests passed\n" passed total)])
  (when (positive? possible)
    (printf "\n    Points: ~a / ~a\n" actual possible)))

(define-syntax-rule (dssl-top-interaction . expr)
  (dssl-begin expr))

(define-syntax-rule (begin-void stmt ...)
  (begin stmt ... (void)))

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
       (lambda (param.id ...)
         (wrap-procedure-body (dssl-begin expr ...))))]))

(define-simple-macro
  (dssl-def (f:real-id cvs:opt-ctc-vars bs:var&ctc ...)
            result-ctc:opt-return-ctc
            expr:expr ...)
  #:fail-when (check-duplicate-identifier
               (syntax->list #'(cvs.var ... bs.var ...)))
  "duplicate argument name"
  (begin-void
    (dssl-provide f)
    (define-square-bracket-proc
      ((f cvs.var ...) [bs.var (ensure-contract 'def bs.ctc)] ...)
      (ensure-contract 'def result-ctc.result)
      (wrap-procedure-body (dssl-begin expr ...)))))

(define-syntax (dssl-let stx)
  (syntax-parse stx
    [(_ var:real-id)
     (with-syntax ([real-var (syntax-e #'var)])
       #'(begin-void
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
                        . args))]))))))]
    [(_ [var:real-id ctc:expr])
     (with-syntax ([real-var (syntax-e #'var)])
       #'(begin-void
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
                        . args))]))))))]
    [(_ var:var rhs:expr)
     #'(begin-void
         (dssl-provide var.id)
         (define var.id rhs)
         (make-set!able var.id))]
    [(_ [var:var ctc:expr] rhs:expr)
     #'(begin-void
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

(define-syntax-parser dssl-for
  [(_ [(i:var j:var) v:expr] expr:expr ...+)
   #:fail-when (and (bound-identifier=? #'i.id #'j.id) #'j.id)
   "duplicate variable name"
   #'(with-break
       (dssl-for/fun
         v
         (dssl-for-loop-body (i.id j.id) expr ...)
         (capture-context v)))]
  [(m [i:var v:expr] expr:expr ...+)
   #'(m [(_ i.id) v] expr ...)])

(define (dssl-for/fun obj body context)
  (define next (p:get-try-advance obj "for loop" context))
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

(define (dssl-for/vec/fun v when? body context)
  (define next   (p:get-try-advance v "vector comprehension" context))
  (define result (make-vector-builder))
  (let loop ([i 0])
    (when (next (λ (j)
                  (when (truthy? (when? i j))
                    (vector-builder-push-back result (body i j)))))
      (loop (add1 i))))
  (vector-builder-copy result))

(define-syntax-parser dssl-for/vec
  [(m [j:id v:expr] expr:expr)
   #'(m [(_ j) v] #:when #true expr)]
  [(m [(i:id j:id) v:expr] expr:expr)
   #'(m [(i j) v] #:when #true expr)]
  [(m [j:id v:expr] #:when when:expr expr:expr)
   #'(m [(_ j) v] #:when when expr)]
  [(_ [(i:var j:var) v:expr] #:when when expr:expr)
   #:fail-when (and (bound-identifier=? #'i.id #'j.id) #'j.id)
   "duplicate variable name"
   #'(dssl-for/vec/fun v
                       (λ (i.id j.id) when)
                       (λ (i.id j.id) expr)
                       (capture-context v))])

(define-syntax (dssl-import stx)
  (unless (memq (syntax-local-context) '(module top-level))
    (syntax-error stx "import can only appear at top-level"))
  (define spec
    (syntax-parse stx
      [(_ lib:string)
       `(file ,#'lib)]
      [(_ lib:id)
       (define basename (~a (syntax-e #'lib) ".rkt"))
       (ensure-readable
         (build-path lib-directory basename)
         (λ (msg)
            (format "import: library module does not exist: ~a\n  reason: ~a"
                    (syntax-e #'lib)
                    msg)))
       `(lib ,basename "dssl2/lib")]))
  (datum->syntax stx `(#%require ,spec)))

; path [string -> string] ->
(define-for-syntax (ensure-readable filename fmt-msg)
  (with-handlers ([exn:fail? (compose error fmt-msg exn-message)])
    (close-input-port (open-input-file filename))))


; setf! is like Common Lisp setf, but it just recognizes three forms. We
; use this to translate assignments.
(define-syntax (dssl-= stx)
  (syntax-parse stx #:literals (dssl-vec-ref dssl-struct-ref)
    [(_ (dssl-vec-ref v:expr i:expr ...+) rhs:expr)
     #'(dssl-vec-set! v (i ...) rhs)]
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

(define-syntax-parser dssl-vec-ref
  [(_ v:expr i:expr ...+)
   (with-syntax
     ([(xi ...) (generate-temporaries (syntax->list #'(i ...)))])
     #'(let ()
         (define xv v)
         (define xi i)
         ...
         (with-error-context (v)
           ((dssl-vec-ref/fun xv) xi ...))))])

(define (dssl-vec-ref/fun v)
  (cond
    [(p:get-method-value v '__index_ref__)]
    [(generic-base? v)
     (generic-base-instantiate v)]
    [else
     (raise-runtime-error "not a vector or indexable object: %p" v)]))

(define-syntax-parser dssl-vec-set!
  [(_ v:expr (i:expr ...+) a:expr)
   (with-syntax
     ([(xi ...) (generate-temporaries (syntax->list #'(i ...)))])
     #'(let ()
         (define xv v)
         (define xi i)
         ...
         (define xa a)
         (with-error-context (v)
           ((dssl-vec-set!/fun xv) xi ... xa))))])

(define (dssl-vec-set!/fun v)
  (cond
    [(p:get-method-value v '__index_set__)]
    [else
     (raise-runtime-error "not a vector or indexable object: %p" v)]))

(define-syntax (dssl-struct/early stx)
  (syntax-parse stx
    #:context 'struct
    [(_ (name:id internal-name:id) . fields:unique-identifiers)
     (with-syntax
       ([external-predicate (struct-predicate-name #'name)]
        [internal-predicate (struct-predicate-name #'internal-name)])
       (define name-length (string-length (~a (syntax-e #'name))))
       (define expansion
         #'(begin
             (struct internal-name struct-base (fields.var ...)
               #:mutable
               #:transparent)
             (dssl-provide external-predicate)
             (define external-predicate
               (procedure-rename internal-predicate
                                 'external-predicate))))
       (syntax-property
         expansion
         'sub-range-binders
         (vector (syntax-local-introduce #'external-predicate)
                 0 name-length 0.5 0.5
                 (syntax-local-introduce #'name)
                 0 name-length 0.5 0.5)))]))


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

(define-syntax-parser dssl-struct/late
  [(_ (name:id internal-name:id) (formal-field:real-id ctc:expr) ...)
   (define name-length    (string-length (~a (syntax-e #'name))))
   (define formal-fields  (syntax->list #'(formal-field ...)))
   (with-syntax
     ([special-name
        ; This special-name is also generated by the parser:
        (struct-special-name #'name)]
      [(setter-name ...)
       (for/list ([field (in-list formal-fields)])
         (format-id field "field ‘~a’ assignment" field))]
      [(ctc-name ...)
       (generate-temporaries formal-fields)])
     (define expansion
       #'(begin-void
           (define ctc-name (ensure-contract 'struct ctc))
           ...
           (define the-struct-info
             (struct-info
               'name
               (vector-immutable
                 (field-info
                   'formal-field
                   (struct-getter-name internal-name formal-field)
                   (contract
                     (-> AnyC ctc-name AnyC)
                     (struct-setter-name internal-name formal-field)
                     (format "field ~a of struct ~a" 'formal-field 'name)
                     "client performing assignment"
                     'name
                     (get-srcloc ctc)))
                 ...)))
           (dssl-provide name)
           (define/contract (name formal-field ...)
                            (-> ctc-name ... AnyC)
                            (internal-name the-struct-info formal-field ...))
           (dssl-provide special-name)
           (define-syntax (special-name stx)
             (syntax-parse stx
               [(ctor:id [field:id expr:expr] (... ...))
                #:fail-when (check-duplicate-identifier
                              (syntax->list #'(field (... ...))))
                "duplicate field name"
                (begin
                  (define actual-fields #'(field (... ...)))
                  (define actual-exprs #'(expr (... ...)))
                  (define field-exprs
                    (for/list ([f (in-syntax actual-fields)]
                               [e (in-syntax actual-exprs)])
                      (cons (syntax-e f) e)))
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
                  (for ([field (in-syntax actual-fields)])
                    (unless (memq (syntax-e field) formal-fields)
                      (syntax-error
                        #'ctor
                        "struct does not have field ~a"
                        (syntax-e field))))
                  #`(name #,@exprs))]))))
   (syntax-property
     expansion
     'sub-range-binders
     (vector (syntax-local-introduce (struct-special-name #'name))
             0 name-length 0.5 0.5
             (syntax-local-introduce #'name)
             0 name-length 0.5 0.5)))])

(define (get-field-info/or-else struct field context)
  (or (get-field-info struct field)
      (raise-runtime-error "struct %p does not have field %s"
                           struct field
                           #:context context)))

(define-syntax-parser dssl-struct-ref
  [(_ target0:expr property:id)
   (define target
     (local-expand #'target0 'expression (list #'dssl-self)))
   (syntax-parse target
     #:literals (dssl-self)
     [dssl-self
      (syntax-property
       #'(dssl-self property)
       'disappeared-use
       (syntax-property
         target
         'disappeared-use))]
     [other
       #'(let ([value other])
           (cond
             [(struct-base? value)
              ((field-info-getter
                 (get-field-info/or-else
                   value
                   'property
                   (capture-context target0)))
               value)]
             [else
               (p:get-method-value/or-else
                 value
                 'property
                 (capture-context target0))]))])])

(define-syntax (dssl-struct-set! stx)
  (syntax-parse stx
    [(_ target0:expr property:id rhs:expr)
     (define target
       (local-expand #'target0 'expression (list #'dssl-self)))
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
                   value 'property
                   (capture-context target0)))
                value rhs)]
              [(object-base? value)
               (raise-runtime-error
                "cannot assign to object properties from outside"
                #:context (capture-context target0))]
              [else
                (raise-runtime-error "value ‘%p’ is not a struct"
                               #,target
                               #:context (capture-context #,target))])))])]))

(define current-dssl-test-timeout
  (make-parameter +inf.0))

(define-syntax-parser dssl-test
  #:literals (dssl-=)
  [(_ timeout:req-timeout)
   #'(current-dssl-test-timeout timeout.seconds)]
  [(_ #:points points:test-points-expr)
   #`(current-dssl-test-points points.value)]
  [(m name:expr timeout:opt-timeout body:expr ...+)
   #`(dssl-test/fun name
                    (inc-case-number!)
                    timeout.seconds
                    (λ ()
                       (inc-total-tests!)
                       (dssl-begin body ...)
                       (inc-passed-tests!))
                    (capture-context m)
                    (capture-context timeout.loc m))])

(define (dssl-test/fun name
                       case-number
                       maybe-timeout
                       thunk
                       test-context
                       timeout-context)
  (define points (current-dssl-test-points))
  (cond
    [(number? points)
     (dssl-test/points
       name
       case-number
       points
       (or maybe-timeout (current-dssl-test-timeout))
       thunk
       timeout-context)]
    [points
      (dssl-test/no-points
        name
        (or maybe-timeout (current-dssl-test-timeout))
        thunk
        (source-context-srcloc test-context)
        timeout-context)]
    [else (void)]))

(define (dssl-test/points name
                          case-number
                          points
                          timeout
                          thunk
                          timeout-context)
  (define (display-error _exn msg)
    (parameterize ([current-output-port (current-error-port)])
      (printf "TEST CASE #~a FAILED!\n~a\n"
              case-number
              (string-indent msg 1))))
  (printf "Test Case #~a (~a point~a): "
          case-number
          points
          (if (= 1 points) "" "s"))
  (print-test-name name)
  (newline)
  (dssl-test-case thunk
                  timeout
                  timeout-context
                  display-error)
  (newline))

(define (dssl-test/no-points name
                             timeout
                             thunk
                             loc
                             timeout-context)
  (define (display-error exn msg)
    ((error-display-handler)
     (format "Failed test: ~a (~a)\n~a"
             (with-output-to-string
               (λ () (print-test-name name)))
             (srcloc->string loc)
             (string-indent msg 1))
     exn)
    (newline (current-error-port)))
  (dssl-test-case thunk
                  timeout
                  timeout-context
                  display-error))

(define (dssl-test-case thunk
                        timeout
                        timeout-context
                        display-error)
  (define (display-timeout-error exn)
    (display-error
      exn
      (format "reason:     out of time\ntime limit: ~a sec."
              (exn:fail:dssl:timeout-seconds exn))))
  (define (display-assertion-error exn)
    (display-error
      exn
      (format "reason:     assertion failure\ncondition:  ~a"
              (exn:fail:dssl:assert-condition exn))))
  (define (display-any-error exn)
    (display-error
      exn
      (format "reason:     an error occurred\ndetails:    ~a"
              (exn-message exn))))
  (define (display-other-error exn)
    (display-error
      exn
      (format "reason:     an error occurred\ndetails:    ~a"
              exn)))
  (with-handlers
    ([exn:fail:dssl:timeout? display-timeout-error]
     [exn:fail:dssl:assert?  display-assertion-error]
     [exn?                   display-any-error]
     [(λ (_) #t)             display-other-error])
    (call-with-timeout timeout thunk timeout-context)))

(define (string-indent str amount)
  (define pad (make-string amount #\space))
  (string-append
    pad
    (string-replace str "\n" (string-append "\n" pad))))

(define (print-test-name name)
  (cond
    [(vector? name)
     (define sep void)
     (for ([line  (in-vector name)]
           #:when (and line (not (void? line))))
       (sep)
       (display line)
       (set! sep newline))]
    [else
      (display name)]))

(define (print-srcloc loc [port (current-output-port)])
  (fprintf port
           "~a:~a:~a"
           (srcloc-source loc)
           (srcloc-line loc)
           (srcloc-column loc)))

(dssl-begin
 (dssl-struct timing
              (dssl-let [label   string?])
              (dssl-let [cpu     natural?])
              (dssl-let [real    natural?])
              (dssl-let [gc      natural?])
              (dssl-let [result  AnyC])))

(define-syntax (dssl-time stx)
  (syntax-parse stx
    [(_ name:expr body:expr ...)
     #'(let ()
         (define label name)
         (define-values (lst cpu real gc)
           (time-apply (λ () (dssl-begin body ...)) '()))
         (timing label cpu real gc (car lst)))]))

(define-syntax-parser dssl-make-vec
  [(_ size:vec-lit-size-expr element:expr)
   #'(make-vector size.n element)])

(define-simple-macro (vec-lit element:expr ...)
  (vector element ...))

(define-simple-macro (dssl-if-e e1:expr e2:expr e3:expr)
  (truthy-cond
   [e1   e2]
   [else e3]))

(define current-dssl-assertion-timeout
  (make-parameter +inf.0))

(define (call-with-assertion-timeout timeout thunk context)
  (call-with-timeout (or timeout (current-dssl-assertion-timeout))
                     thunk
                     context))

(define-for-syntax (build-assertion-thunk e)
  (syntax-parse e
    ; binary operators:
    [(op:binary-operator e1:expr e2:expr)
     #`(λ ()
          (define v1 e1)
          (define v2 e2)
          (when (falsy? (op v1 v2))
            (raise-assertion-error/loc #,e "%p %s %p" v1 op.name v2)))]
    ; unary operators:
    [(op:unary-operator e1:expr)
     #`(λ ()
          (define v1 e1)
          (when (falsy? (op v1))
            (raise-assertion-error/loc #,e "%s %p" op.name v1)))]
    ; arbitrary expressions:
    [_
      #`(λ ()
           (define v #,e)
           (when (falsy? v)
             (raise-assertion-error/loc #,e "%p" v)))]))

(define-syntax-parser dssl-assert
  ; changing the default timeout
  [(_ timeout:req-timeout)
   #'(current-dssl-assertion-timeout timeout.seconds)]
  ; assertions
  [(m e:expr timeout:opt-timeout)
   #`(call-with-assertion-timeout
       timeout.seconds
       #,(build-assertion-thunk #'e)
       (capture-context timeout.loc m))])

(define (dssl-assert-error/thunk thunk pattern context)
  (define (handler exn)
    (if (string-contains? (exn-message exn) pattern)
        #f
        (~a "got a different error than expected:\n"
            " error message:  " (exn-message exn) "\n"
            " should contain: " pattern)))
  (λ ()
     (define message
       (with-handlers ([exn:fail? handler])
                      (thunk)
                      "did not error as expected"))
     (when message
       (raise-assertion-error message #:context context))))

(define-syntax-parser dssl-assert-error
  [(m code:expr msg:expr timeout:opt-timeout)
   #`(call-with-assertion-timeout
       timeout.seconds
       (dssl-assert-error/thunk
         (λ () code)
         msg
         (capture-context code))
       (capture-context timeout.loc m))]
  [(m code:expr timeout:opt-timeout)
   #'(m code "" . timeout)])

(define (call-with-timeout seconds thunk context)
  (define (handle _)
    (raise-timeout-error seconds #:context context))
  (cond
    [(= +inf.0 seconds) (thunk)]
    [else
     (with-handlers ([exn:fail:resource? handle])
       (call-with-deep-time-limit/exceptions seconds thunk))]))

(define (call-with-deep-time-limit/exceptions seconds thunk)
  (define failure #f)
  (define success #f)
  (define (handle exn) (set! failure exn))
  (with-deep-time-limit seconds
    (with-handlers ([(λ (_) #t) handle])
      (set! success (thunk))))
  (if failure
    (raise failure)
    success))

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
     #'(begin-void
         (define-dssl-interface
           name
           (cvs.var ...)
           ((super.name super.params ...) ...)
           ([method-name
             (method-cvs.var ...)
             (method-params.ctc ...)
             method-result.result]
            ...)))]))

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
     #'(begin-void
         (define-dssl-class
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
            ...)))]))

