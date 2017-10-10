#lang racket

(provide #%app
         #%datum
         #%top)
(provide (rename-out
           ; special syntax
           [dssl-module-begin           #%module-begin]
           [dssl-top-interaction        #%top-interaction]
           ; built-in operators
           [modulo              %]
           [expt                **]
           [equal?              ==]
           [eq?                 ===]
           [not                 !]
           [bitwise-and         &]
           [bitwise-ior         \|]
           [bitwise-xor         ^]
           [bitwise-not         ~]
           [-                   -]
           [*                   *]
           [/                   /]
           [dssl-+              +]
           [dssl-!=             !=]
           [dssl-!==            !==]
           [dssl-<              <]
           [dssl->              >]
           [dssl-<=             <=]
           [dssl->=             >=]
           [dssl->>             >>]
           [dssl-<<             <<]
           ; syntax
           [and                 and]
           [or                  or]
           [begin               begin]
           [if                  if]
           [else                else]
           [void                pass]
           [vector              vector]
           [dssl-True           True]
           [dssl-False          False]
           [dssl-assert         assert]
           [dssl-assert-eq      assert_eq]
           [dssl-assert-error   assert_error]
           [dssl-break          break]
           [dssl-cond           cond]
           [dssl-continue       continue]
           [dssl-def            def]
           [dssl-defstruct      defstruct]
           [dssl-elif           elif]
           [dssl-error          error]
           [dssl-for            for]
           [dssl-for/vector     for/vector]
           [dssl-lambda         lambda]
           [dssl-let            let]
           [dssl-make-vector    make-vector]
           [dssl-object         object]
           [dssl-import         import]
           [dssl-return         return]
           [dssl-setf!          setf!]
           [dssl-setf!          =]
           [dssl-struct-ref     struct-ref]
           [dssl-test           test]
           [dssl-time           time]
           [dssl-vector-ref     vector-ref]
           [dssl-while          while])
         (all-from-out dssl2/private/prims))

(require dssl2/private/errors
         dssl2/private/prims
         dssl2/private/struct)
(require racket/stxparam
         racket/splicing
         racket/contract/region
         syntax/parse/define
         rackunit)
(require (prefix-in racket: racket))

(require (for-syntax syntax/parse))
(require (for-syntax dssl2/private/find-lib))

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

(define-syntax-rule (dssl-module-begin expr ...)
  (#%module-begin
   (#%provide (all-defined))
   (module* configure-runtime racket/base
     (require dssl2/private/parser
              dssl2/private/printer
              racket/pretty)
     (pretty-print-size-hook dssl-print-size-hook)
     (pretty-print-print-hook dssl-print-print-hook)
     (current-print dssl-print)
     (current-read-interaction
       (λ (src in)
          (let loop ()
            (define-values (line column position) (port-next-location in))
            (define the-line (read-line in))
            (cond
              [(eof-object? the-line)
               the-line]
              [(regexp-match #rx"^ *$" the-line)
               (loop)]
              [else
                (define line-port (open-input-string the-line))
                (port-count-lines! line-port)
                (define relocated
                  (relocate-input-port line-port line column position
                                       #true #:name src))
                (parse-dssl2 src relocated #t)])))))
   (define passed-tests 0)
   (define total-tests 0)
   (splicing-syntax-parameterize
     ([inc-passed-tests!  (syntax-rules ()
                            [(_) (set! passed-tests (add1 passed-tests))])]
      [inc-total-tests!   (syntax-rules ()
                            [(_) (set! total-tests (add1 total-tests))])])
     (dssl-begin expr ...))
   (print-test-results passed-tests total-tests)))

(define (print-test-results passed total)
  (cond
    [(zero? total)       (void)]
    [(= passed total 1)  (printf "The only test passed\n")]
    [(= total 1)         (printf "The only test failed\n")]
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

(define-syntax dssl-cond
  (syntax-rules (else)
    [(_ [test result ...] ... [else else-result ...])
     (cond [test (dssl-begin result ...)]
           ...
           [else (dssl-begin else-result ... )])]))

(define-syntax-rule (dssl-lambda (param ...) expr ...)
  (lambda (param ...)
    (let/ec return-f
       (syntax-parameterize
         ([dssl-return (syntax-rules ()
                         [(_)         (return-f (void))]
                         [(_ ?result) (return-f ?result)])])
         (dssl-begin expr ...)))))

(define-simple-macro (dssl-def (f:id [tv:id ...]
                                     (formal:id contract:expr) ...)
                               result-contract:expr
                               expr:expr ...)
   #:fail-when (check-duplicate-identifier
                 (syntax->list #'(tv ... formal ...)))
               "duplicate argument name"
  (begin
    (define/contract f
                     (parametric->/c [tv ...]
                       (-> contract ... result-contract))
                     (dssl-lambda (formal ...) expr ...))
    (make-set!able f)))

(define-syntax dssl-let
  (syntax-rules ()
    [(_ (name contract))
     (begin
       (define/contract name contract (void))
       (make-set!able name))]
    [(_ (name contract) expr)
     (begin
       (define/contract name contract expr)
       (make-set!able name))]))

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
      (define (continue-f) (loop) (break-f))
      (syntax-parameterize
        ([dssl-break (syntax-rules () [(_) (break-f)])]
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
               ([dssl-break    (syntax-rules () [(_) (break-f)])]
                [dssl-continue (syntax-rules () [(_) (continue-f)])])
               expr ...))))]
    [(_ [i:id v:expr] expr:expr ...+)
     #'(dssl-for [(_ i) v] (dssl-begin expr ...))]))

(define-syntax (dssl-for/vector stx)
  (syntax-parse stx
    [(_ [(i:id j:id) v:expr] expr:expr)
     #:fail-when (and (bound-identifier=? #'i #'j) #'j)
                 "duplicate variable name"
     #'(for/vector ([i (in-naturals)]
                    [j (dssl-in-value v)])
         expr)]
    [(_ [j:id v:expr] expr:expr)
     #'(dssl-for/vector [(_ j) v] expr)]
    [(_ [(i:id j:id) v:expr] #:when when expr:expr)
     #:fail-when (and (bound-identifier=? #'i #'j) #'j)
                 "duplicate variable name"
     #'(for/vector ([i (in-naturals)]
                    [j (dssl-in-value v)]
                    #:when when)
         expr)]
    [(_ [j:id v:expr] #:when when:expr expr:expr)
     #'(dssl-for/vector [(_ j) v] #:when when expr)]))

(define (dssl-in-value v)
  (cond
    [(vector? v)   (in-vector v)]
    [(natural? v)  (in-range v)]
    [(string? v)   (in-vector (explode v))]
    [else          (type-error 'for v "something iterable")]))

(define-syntax (dssl-import stx)
  (syntax-parse stx
    [(_ lib:id)
     (let ([filename (path->string
                       (build-path
                         lib-directory
                         (format "~a.rkt" (syntax->datum #'lib))))])
       #`(#%require
          #,(datum->syntax stx `(file ,filename))))]))

; setf! is like Common Lisp setf, but it just recognizes three forms. We
; use this to translate assignments.
(define-syntax dssl-setf!
  (syntax-rules (dssl-vector-ref dssl-struct-ref)
    [(_ (dssl-vector-ref v i) rhs)
     (vector-set! v i rhs)]
    [(_ (dssl-struct-ref s f) rhs)
     (dssl-struct-set! s 'f rhs)]
    [(_ i rhs)
     (set! i rhs)]))

(define (dssl-vector-ref v i)
  (vector-ref v i))

(define-for-syntax (format-stx fmt stx0 . stxs)
  (datum->syntax stx0
    (string->symbol
      (apply format fmt
             (syntax->datum stx0)
             (map syntax->datum stxs)))))

(define-syntax (dssl-defstruct/early stx)
  (syntax-parse stx
    [(_ (name:id internal-name:id) (field:id ...))
     #`(begin
         (define-struct (internal-name struct-base) (field ...)
                        #:mutable
                        #:transparent
                        #:methods gen:custom-write
                        [(define write-proc dssl-write-struct)])
         (define (#,(format-stx "~a?" #'name) value)
           (#,(format-stx "~a?" #'internal-name) value)))]))

(define-syntax dssl-begin
  (syntax-rules ()
    [(_ defn ...) (dssl-begin/acc () () defn ...)]))

(define-syntax (dssl-begin/acc stx)
  (syntax-parse stx
    #:literals (dssl-defstruct begin)
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
    ; Interpret defstructs
    [(_ (early-defns ...) (late-defns ...)
        (dssl-defstruct name:id ((field:id ctc:expr) ...))
        rest ...)
     #:fail-when (check-duplicate-identifier
                   (syntax->list #'(field ...))) "duplicate field name"
     (with-syntax ([s:cons (format-stx "s:~a" #'name)])
       #`(dssl-begin/acc
           (early-defns
             ...
             (dssl-defstruct/early (name s:cons) (field ...)))
           (late-defns
             ...
             (dssl-defstruct/late (name s:cons) ((field ctc) ...)))
           rest ...))]
    ; Pass everything else through
    [(_ (early-defns ...) (late-defns ...)
        first rest ...)
     #'(dssl-begin/acc (early-defns ...)
                       (late-defns ... first)
                       rest ...)]))

(define-syntax (struct-getter-name stx)
  (syntax-parse stx
    [(_ name:id field:id)
     (format-stx "~a-~a" #'name #'field)]))

(define-syntax (struct-setter-name stx)
  (syntax-parse stx
    [(_ name:id field:id)
     (format-stx "set-~a-~a!" #'name #'field)]))

(define-syntax (dssl-defstruct stx)
  (raise-syntax-error
    #f
    (string-append "Saw dssl-defstruct, which should be changed to "
                   "dssl-defstruct/late by #%module-begin")
    stx))

(define-syntax (dssl-defstruct/late stx)
  (syntax-parse stx
    [(_ (name:id internal-name:id) ((formal-field:id contract:expr) ...))
     (with-syntax ([s:cons #'internal-name]
                   [(setter-name ...)
                    (map (λ (field)
                            (format-stx "field ‘~a’ assignment" field))
                         (syntax->list #'(formal-field ...)))]
                   [(contract-name ...)
                    (generate-temporaries
                      (syntax->list #'(formal-field ...)))])
       #`(begin
           (define contract-name contract) ...
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
           (define-syntax (#,(format-stx "m:~a" #'name) stx)
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
                            (format "Struct ~a requires field ~a"
                                    'name (syntax->datum field))
                            stx)])))
                  (for ([field actual-fields])
                    (unless (memq field (map syntax->datum formal-fields))
                      (raise-syntax-error
                        #f
                        (format "Struct ~a does not have field ~a"
                                'name field)
                        field)))
                  #`(name #,@exprs))]))))]))

(define-syntax (dssl-object stx)
  (syntax-parse stx
    [(_ name:id [field:id expr:expr] ...)
     #:fail-when (check-duplicate-identifier
                   (syntax->list #'(field ...)))
                 "duplicate field name"
     #`(let ()
         (dssl-begin
           (dssl-defstruct name ((field AnyC) ...))
           (name expr ...)))]))

(define (get-field-info struct field)
  (let/ec return
    (unless (struct-base? struct)
      (runtime-error "Value ‘~a’ is not a struct" struct))
    (define info-vector (struct-info-field-infos
                          (struct-base-struct-info struct)))
    (for ([info (in-vector info-vector)])
      (when (eq? field (field-info-name info))
        (return info)))
    (runtime-error "Struct ‘~a’ does not have field ‘~a’"
                   struct field)))

(define-syntax-rule (dssl-struct-ref expr field)
  (let ([value expr])
    ((field-info-getter (get-field-info value 'field)) value)))

(define (dssl-struct-set! struct field rhs)
  ((field-info-setter (get-field-info struct field)) struct rhs))

(define-syntax (dssl-test stx)
  (syntax-parse stx
    [(_ name:expr body:expr ...+)
     #'(test-case (~a name)
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

(define/contract (dssl-make-vector a b)
  (-> int? AnyC vec?)
  (make-vector a b))

(define (vector . args)
  (list->vector args))

(define-syntax-rule (dssl-assert expr)
  (unless expr
    (assertion-error "‘~a’ did not evaluate to true" 'expr)))

(define-syntax-rule (dssl-assert-eq e1 e2)
  (begin
    (define v1 e1)
    (define v2 e2)
    (unless (equal? v1 v2)
      (assertion-error "‘~a’ ≠ ‘~a’" v1 v2))))

(define (dssl-assert-error/thunk thunk string-pattern)
  (define pattern (regexp (regexp-quote string-pattern #false)))
  (define (handler exception)
    (if (regexp-match? pattern (exn-message exception))
      #false
      "assert_error: errored as expected, but didn’t match the pattern"))
  (define message (with-handlers ([exn:fail? handler])
                    (thunk)
                    "assert_error: did not error as expected"))
  (when (string? message) (assertion-error message)))

(define-syntax (dssl-assert-error stx)
  (syntax-parse stx
    [(_ code:expr expected:str)
     #'(dssl-assert-error/thunk (λ () code) expected)]
    [(_ code:expr)
     #'(dssl-assert-error/thunk (λ () code) "")]))

(define/contract (/ a b)
  (-> num? num? num?)
  (cond
    [(and (int? a) (int? b))
     (quotient a b)]
    [else
     (racket:/ a b)]))

(define dssl-+
  (case-lambda
    [(a)
     (cond
       [(number? a)
        a]
       [else
         (runtime-error
           "unary + expects a number, but given ‘~s’"
           a)])]
    [(a b)
     (cond
       [(and (number? a) (number? b))
        (+ a b)]
       [(or (string? a) (string? b))
        (format "~a~a" a b)]
       [else
         (runtime-error
           "+ expects 2 numbers or at least 1 string, but given ‘~s’ and ‘~s’"
           a b)])]))

(define (dssl-!= a b)
  (not (equal? a b)))

(define (dssl-!== a b)
  (not (eq? a b)))

(define-syntax-rule (make-comparison name string-cmp number-cmp)
  (define (name a b)
    (cond
      [(and (string? a) (string? b))
       (string-cmp a b)]
      [(and (number? a) (number? b))
       (number-cmp a b)]
      [else
        (runtime-error
          "Comparator ‘~a’ only applies to 2 strings or 2 numbers"
          'number-cmp)])))

(make-comparison dssl-< string<? <)
(make-comparison dssl-> string>? >)
(make-comparison dssl-<= string<=? <=)
(make-comparison dssl->= string>=? >=)

(define (dssl-<< n m)
  (arithmetic-shift n m))

(define (dssl->> n m)
  (arithmetic-shift n (- m)))

(define-syntax-rule (dssl-error msg arg ...)
  (error (format msg arg ...)))

