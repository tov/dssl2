#lang racket

(provide #%app
         #%datum
         #%top
         #%top-interaction)
(provide (rename-out
           ; special syntax
           [dssl-module-begin   #%module-begin]
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
           [cond                cond]
           [if                  if]
           [else                else]
           [void                pass]
           [vector              vector]
           [dssl-True           True]
           [dssl-False          False]
           [dssl-assert         assert]
           [dssl-assert-eq      assert_eq]
           [dssl-break          break]
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
           [dssl-return         return]
           [dssl-setf!          setf!]
           [dssl-setf!          =]
           [dssl-struct-ref     struct-ref]
           [dssl-test           test]
           [dssl-vector-ref     vector-ref]
           [dssl-while          while])
         ; values
         ; * type predicates
         (rename-out
           [number?             num?]
           [exact-integer?      int?]
           [flonum?             float?]
           [string?             str?]
           [boolean?            bool?]
           [procedure?          proc?]
           [vector?             vec?])
         ; * numeric operations
         floor
         ceiling
         int
         float
         quotient
         random
         random_bits
         RAND_MAX
         remainder
         sqrt
         ; ** predicates
         zero?
         positive?
         negative?
         even?
         odd?
         ; * string operations
         chr
         explode
         format
         implode
         ord
         (rename-out
           [string-length       string_length])
         ; * vector operations
         build_vector
         len
         map
         filter
         ; * I/O operations
         print
         println
         ; * other functions
         identity)

(require dssl2/private/values)
(require racket/stxparam
         syntax/parse/define
         rackunit)
(require (prefix-in racket: racket))

(require (for-syntax syntax/parse))

(define dssl-True #t)
(define dssl-False #f)

(define-syntax-rule (dssl-module-begin expr ...)
  (#%module-begin
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
   (provide (all-defined-out))
   expr ...))

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
     #'(make-vec
         (for/vector ([i (in-naturals)]
                      [j (dssl-in-value v)])
           expr))]
    [(_ [j:id v:expr] expr:expr)
     #'(dssl-for/vector [(_ j) v] expr)]
    [(_ [(i:id j:id) v:expr] #:when when expr:expr)
     #:fail-when (and (bound-identifier=? #'i #'j) #'j)
                 "duplicate variable name"
     #'(make-vec
         (for/vector ([i (in-naturals)]
                      [j (dssl-in-value v)]
                      #:when when)
           expr))]
    [(_ [j:id v:expr] #:when when:expr expr:expr)
     #'(dssl-for/vector [(_ j) v] #:when when expr)]))

(define (dssl-in-value v)
  (cond
    [(vec? v)      (in-vector (unvec v))]
    [(natural? v)  (in-range v)]
    [(string? v)   (in-vector (unvec (explode v)))]
    [else          (type-error 'for v "something iterable")]))

; setf! is like Common Lisp setf, but it just recognizes three forms. We
; use this to translate assignments.
(define-syntax dssl-setf!
  (syntax-rules (dssl-vector-ref dssl-struct-ref)
    [(_ (dssl-vector-ref v i) rhs)
     (vector-set! (unvec v) i rhs)]
    [(_ (dssl-struct-ref s f) rhs)
     (dssl-struct-set! s f rhs)]
    [(_ i rhs)
     (set! i rhs)]))

(define (dssl-vector-ref v i)
  (vector-ref (unvec v) i))

(define-syntax (dssl-defstruct stx)
  (syntax-parse stx
    [(_ name:id (formal-field:id ...))
     #:fail-when (check-duplicate-identifier
                   (syntax->list #'(formal-field ...)))
                 "duplicate field name"
     (let ([predicate (datum->syntax #'name
                        (string->symbol
                          (format "~a?" (syntax->datum #'name))))]
           [constructor (datum->syntax #'name
                          (string->symbol
                            (format "make-~a" (syntax->datum #'name))))])
     #`(begin
         (define-syntax (#,constructor stx)
           (syntax-parse stx
             [(_ [field:id expr:expr] (... ...))
              #:fail-when (check-duplicate-identifier
                            (syntax->list #'(field (... ...))))
                          "duplicate field name"
              #'(dssl-make-struct/fields
                  'name
                  '(formal-field ...)
                  (list (make-field 'field expr) (... ...)))]))
         (define (name formal-field ...)
           (dssl-make-struct
             'name
             '(formal-field ...)
             (list formal-field ...)))
         (define (#,predicate value)
           (and (struct? value)
                (eq? 'name (struct-name value))))))]))

(define (dssl-make-struct name formals actuals)
  (unless (= (length formals) (length actuals))
    (runtime-error
      "Constructor ‘~a’ expects ~a arguments, got ~a"
      name (length formals) (length actuals)))
  (make-struct name (racket:map make-field formals actuals)))

(define (dssl-make-struct/fields name formals actuals)
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
  (make-struct name (racket:map get-value formals)))

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

(define-syntax (dssl-test stx)
  (syntax-parse stx
    [(_ name:expr body:expr ...+)
     #'(test-case (~a name) body ...)]))

(define (dssl-make-vector a b)
  (make-vec (make-vector a b)))

(define (vector . args)
  (make-vec (list->vector args)))

(define (vector? v)
  (vec? v))

(define (build_vector n f)
  (make-vec (racket:build-vector n f)))

(define (len v)
  (vector-length (unvec v)))

(define-syntax-rule (dssl-assert expr)
  (unless expr
    (assertion-error "‘~a’ did not evaluate to true" 'expr)))

(define-syntax-rule (dssl-assert-eq e1 e2)
  (begin
    (define v1 e1)
    (define v2 e2)
    (unless (equal? v1 v2)
      (assertion-error "‘~a’ ≠ ‘~a’" v1 v2))))

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

(define (print fmt . values)
  (cond
    [(string? fmt) (display (apply format fmt values))]
    [else          (for-each display (cons fmt values))]))

(define (println fmt . values)
  (apply print fmt values)
  (newline))

(define (dssl-<< n m)
  (arithmetic-shift n m))

(define (dssl->> n m)
  (arithmetic-shift n (- m)))

(define (chr i)
  (~a (integer->char i)))

(define (explode s)
  (make-vec
    (list->vector
      (racket:map (λ (c) (list->string (list c)))
                  (string->list s)))))

(define (implode vec)
  (apply string-append (vector->list (unvec vec))))

(define (ord c)
  (unless (= 1 (string-length c))
    (error 'ord "expect character (string of length 1)"))
  (char->integer (string-ref c 0)))

(define (map f vec)
  (make-vec
    (build-vector (vector-length (unvec vec))
                  (λ (i) (f (vector-ref (unvec vec) i))))))

(define (filter f vec)
  (make-vec (list->vector (racket:filter f (vector->list (unvec vec))))))

(define (floor n)
  (inexact->exact (racket:floor n)))

(define (ceiling n)
  (inexact->exact (racket:ceiling n)))

(define (int x)
  (cond
    [(number? x) (inexact->exact (truncate x))]
    [(string? x)
     (cond
       [(string->number x) => int]
       [else (runtime-error "int: could not convert to integer: ~s" x)])]
    [(eq? #t x)  1]
    [(eq? #f x)  0]
    [else (type-error 'int x "number, string, or Boolean")]))

(define (float x)
  (cond
    [(number? x) (exact->inexact x)]
    [(string? x)
     (cond
       [(string->number x) => float]
       [else (runtime-error "float: could not convert to float: ~s" x)])]
    [(eq? #t x)  1.0]
    [(eq? #f x)  0.0]
    [else (type-error 'int x "number, string, or Boolean")]))

(define random
  (case-lambda
    [() (racket:random)]
    [(limit) (racket:random limit)]
    [(low high) (racket:random low high)]))

; This is the largest argument that `random` can take.
(define RAND_MAX 4294967087)

(define (random_bits n)
  (define *RADIX* 16)
  (cond
    [(zero? n)      0]
    [else           (+ (* 2 (random_bits (sub1 n)))
                       (random 2))]))

(define-syntax-rule (dssl-error msg arg ...)
  (error (format msg arg ...)))

(define (type-error who got expected)
  (runtime-error "~a: got ~s where ~a expected" who got expected))

(define (runtime-error fmt . args)
  (error (apply format (string-append "Runtime error: " fmt) args)))

(define (assertion-error fmt . args)
  (error (apply format (string-append "Assertion failed: " fmt) args)))
