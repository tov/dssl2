#lang racket

(provide #%app
         #%datum
         #%module-begin
         #%top
         #%top-interaction)
(provide + - * /
         make-vector vector
         procedure? string? number? vector?
         integer? zero? positive? negative? even? odd?
         begin
         cond
         else
         or
         (rename-out
           ; values
           [modulo              %]
           [expt                **]
           [equal?              ==]
           [eq?                 ===]
           [not                 !]
           [void                pass]
           [bitwise-and         &]
           [bitwise-ior         bitwise-or]
           [bitwise-xor         ^]
           [bitwise-not         ~]
           [identity            identity]
           [build-vector        build_vector]
           [vector-length       len]
           [dssl-!=             !=]
           [dssl-!==            !==]
           [dssl-<              <]
           [dssl->              >]
           [dssl-<=             <=]
           [dssl->=             >=]
           [dssl->>             >>]
           [dssl-<<             <<]
           [dssl-explode        explode]
           [dssl-implode        implode]
           [dssl-print          print]
           [dssl-println        println]
           [dssl-map            map]
           [dssl-filter         filter]
           ; syntax
           [and                 &&]
           [dssl-assert         assert]
           [dssl-assert-eq      assert-eq]
           [dssl-break          break]
           [dssl-continue       continue]
           [dssl-def            def]
           [dssl-defstruct      defstruct]
           [dssl-error          error]
           [dssl-for            for]
           [dssl-for/vector     for/vector]
           [dssl-lambda         lambda]
           [dssl-let            let]
           [dssl-return         return]
           [dssl-setf!          setf!]
           [dssl-struct-ref     struct-ref]
           [dssl-vector-ref     vector-ref]
           [dssl-while          while]))
(require racket/stxparam
         syntax/parse/define)
(require (for-syntax syntax/parse))

; We define return (for lambda) as a syntax parameter, and then
; syntax-parameterize it inside dssl-lambda.
(define-syntax-parameter
  dssl-return
  (lambda (stx)
    (raise-syntax-error #f "use of return keyword not in a function" stx)))

(define-syntax-rule (dssl-lambda (param ...) expr ...)
  (lambda (param ...)
    (let/ec return-f
       (syntax-parameterize
         ([dssl-return (syntax-rules ()
                         [(_ ?result) (return-f ?result)])])
         (begin expr ...)))))

(define-simple-macro (dssl-def (f:id formal:id ...) expr:expr ...)
   #:fail-when (check-duplicate-identifier
                 (syntax->list #'(formal ...)))
               "duplicate argument name"
  (define f (dssl-lambda (formal ...) expr ...)))

(define-syntax dssl-let
  (syntax-rules ()
    [(_ name)
     (define name (void))]
    [(_ name expr)
     (begin
       (define name (void))
       (set! name expr))]))

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
          expr ...
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
     #'(dssl-for [(_ i) v] expr ...)]))

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
    [(string? v)   (in-vector (dssl-explode v))]
    [else          (runtime-error "Value ‘~a’ is not iterable" v)]))

; setf! is like Common Lisp setf, but it just recognizes three forms. We
; use this to translate assignments.
(define-syntax dssl-setf!
  (syntax-rules (dssl-vector-ref dssl-struct-ref)
    [(_ (dssl-vector-ref v i) rhs)
     (vector-set! v i rhs)]
    [(_ (dssl-struct-ref s f) rhs)
     (dssl-struct-set! s f rhs)]
    [(_ i rhs)
     (set! i rhs)]))

(define dssl-vector-ref vector-ref)

(define-struct struct [name fields]
               #:transparent)
(define-struct field [name value]
               #:transparent
               #:mutable)

(define (struct-assq name fields)
  (cond
    [(memf (λ (field) (eq? (field-name field) name)) fields)
     => first]
    [else #false]))

(define-syntax (dssl-defstruct stx)
  (syntax-parse stx
    [(_ name:id (formal-field:id ...))
     #:fail-when (check-duplicate-identifier
                   (syntax->list #'(formal-field ...)))
                 "duplicate field name"
     (let ([predicate (datum->syntax #'name
                        (string->symbol
                          (format "~a?" (syntax->datum #'name))))])
     #`(begin
         (define-simple-macro (name [field:id expr:expr] (... ...))
           #:fail-when (check-duplicate-identifier
                         (syntax->list #'(field (... ...))))
                       "duplicate field name"
           (dssl-make-struct 'name
                             '(formal-field ...)
                             (list (make-field 'field expr)
                                   (... ...))))
         (define (#,predicate value)
           (and (struct? value)
                (eq? 'name (struct-name value))))))]))

(define (dssl-make-struct name formals actuals)
  (define (get-value field)
    (or (struct-assq field actuals)
        (runtime-error
          "Constructor for ‘~a’ expects field ‘~a’"
          name field)))
  (for-each (λ (actual)
              (unless (memq (field-name actual) formals)
                (runtime-error
                  "Constructor for ‘~a’ does not expect field ‘~a’"
                  name (field-name actual))))
            actuals)
  (make-struct name (map get-value formals)))

(define-syntax-rule (dssl-struct-ref value field)
  (begin
    (when (not (struct? value))
      (runtime-error "Value ‘~a’ is not a struct" value))
    (cond
      [(struct-assq 'field (struct-fields value)) => field-value]
      [else
        (runtime-error "Struct ‘~a’ does not have field ‘~a’"
                       value 'field)])))

(define-syntax-rule (dssl-struct-set! value field rhs)
  (begin
    (when (not (struct? value))
      (runtime-error "Value ‘~a’ is not a struct" value))
    (cond
      [(struct-assq 'field (struct-fields value))
       =>
       (λ (field) (set-field-value! field rhs))]
      [else
        (runtime-error "Struct ‘~a’ does not have field ‘~a’"
                       value 'field)])))

(define-syntax-rule (dssl-assert expr)
  (unless expr
    (assertion-error "‘~a’ did not evaluate to true" 'expr)))

(define-syntax-rule (dssl-assert-eq e1 e2)
  (begin
    (define v1 e1)
    (define v2 e2)
    (unless (equal? v1 v2)
      (assertion-error "‘~a’ != ‘~a’" v1 v2))))

(define (dssl-!= a b)
  (not (equal? a b)))

(define (dssl-!== a b)
  (not (eq? a b)))

(define-syntax make-comparison
  (syntax-rules ()
    [(_ name string-cmp number-cmp)
     (define (name a b)
       (cond
         [(and (string? a) (string? b))
          (string-cmp a b)]
         [(and (number? a) (number? b))
          (number-cmp a b)]
         [else
           (runtime-error
             "Comparator ‘~a’ only applies to 2 strings or 2 numbers"
             'number-cmp)]))]))

(make-comparison dssl-< string<? <)
(make-comparison dssl-> string>? >)
(make-comparison dssl-<= string<=? <=)
(make-comparison dssl->= string>=? >=)

(define (dssl-print fmt . values)
  (cond
    [(string? fmt) (display (apply format fmt values))]
    [else          (for-each display (cons fmt values))]))

(define (dssl-println fmt . values)
  (apply dssl-print fmt values)
  (newline))

(define (dssl-<< n m)
  (arithmetic-shift n m))

(define (dssl->> n m)
  (arithmetic-shift n (- m)))

(define (dssl-explode s)
  (list->vector
    (map (λ (c) (list->string (list c)))
         (string->list s))))

(define (dssl-implode vec)
  (apply string-append (vector->list vec)))

(define (dssl-map f vec)
  (build-vector (vector-length vec)
                (λ (i) (f (vector-ref vec i)))))

(define (dssl-filter f vec)
  (list->vector (filter f (vector->list vec))))

(define-syntax-rule (dssl-error msg arg ...)
  (let ([fmt  msg]
        [args (list arg ...)])
    (cond
      [(string? fmt) (error (apply format fmt args))]
      [else          (apply error fmt args)])))

(define (runtime-error fmt . args)
  (error (apply format (string-append "Runtime error: " fmt) args)))

(define (assertion-error fmt . args)
  (error (apply format (string-append "Assertion failed: " fmt) args)))