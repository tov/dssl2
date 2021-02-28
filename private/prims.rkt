#lang racket/base

(require (only-in racket/contract/base contract-out))

(provide ; helpers for other modules
         get-method-value
         get-method-value/or-else
         get-try-advance
         dssl-send
         dssl-equal?
         ; values
         ; * primitive interfaces
         ; ** ITERABLE
         ; ** ITERATOR
         ; * primitive classes
         ; ** boolean
         bool bool?
         ; ** character
         char char?
         ; ** integer
         int int?
         ; ** index_iterator
         ; ** float
         float float?
         ; ** proc
         proc proc?
         ; ** range_iterator
         range_iterator range_iterator?
         (contract-out [range (case->
                                (-> num? num? num? AnyC)
                                (-> num? num? AnyC)
                                (-> num? AnyC))])
         ; ** string
         str str?
         ; ** vector
         vec vec?
         ; * comparison functions
         cmp max min
         ; * length
         len
         ; * numeric predicates
         (contract-out
           [num?        (-> AnyC AnyC)]
           [nat?        (-> AnyC AnyC)]
           [nan?        (-> AnyC AnyC)]
           [zero?       (-> AnyC AnyC)]
           [pos?        (-> AnyC AnyC)]
           [neg?        (-> AnyC AnyC)]
           [even?       (-> AnyC AnyC)]
           [odd?        (-> AnyC AnyC)])
         ; * contracts
         contract?
         flat_contract?
         AnyC
         NoneC
         TupC
         VecC
         FunC
         (contract-out
           [SquareBracketC (-> str? proc? AnyC)])
         (contract-out
           [NotC (-> contract? contract?)]
           [OrC (-> contract? contract? ... contract?)]
           [AndC (-> contract? contract? ... contract?)]
           [IntInC (-> (OrC int? NoneC) (OrC int? NoneC) contract?)]
           [apply_contract (case-> (-> contract? AnyC AnyC)
                                   (-> contract? AnyC str? AnyC)
                                   (-> contract? AnyC str? str? AnyC))]
           [make_contract (-> str?
                              (OrC NoneC (-> AnyC AnyC))
                              (OrC NoneC (-> (-> str? NoneC) AnyC AnyC))
                              contract?)])
         ; * Randomness operations
         (contract-out
           [random (case->
                     (-> float?)
                     (-> (IntInC 1 RAND_MAX) nat?)
                     (-> int? int? int?))]
           [random_bits (-> nat? nat?)]
           [RAND_MAX nat?])
         ; * I/O operations
         (contract-out
           [print (-> str? AnyC ... NoneC)]
           [println (-> AnyC ... NoneC)]
           [eprint (-> str? AnyC ... NoneC)]
           [eprintln (-> AnyC ... NoneC)]
           [current_directory (case->
                               (-> str?)
                               (-> str? bool?))]
           [file_to_string (-> str? str?)]
           [string_to_file (-> str? str? NoneC)]
           [sleep (-> num? NoneC)])
         ; * other functions
         dir)

(require "class-system.rkt"
         "contract.rkt"
         "defines.rkt"
         "errors.rkt"
         "object.rkt"
         "struct.rkt"
         "generic.rkt"
         "printer.rkt"
         "singletons.rkt"
         syntax/parse/define
         (only-in racket/file
                  display-to-file
                  file->string)
         (only-in racket/list
                  first
                  rest)
         (only-in racket/contract
                  ->
                  and/c
                  case->
                  contract-out
                  dynamic->*
                  flat-named-contract
                  integer-in
                  make-contract
                  new-∀/c
                  new-∃/c
                  or/c
                  not/c
                  raise-blame-error
                  rename-contract)
         (only-in racket/format
                  ~a)
         (only-in racket/function
                  identity)
         (prefix-in r: racket/base)
         (prefix-in r: racket/contract/base)
         (prefix-in r: racket/math))
(require (for-syntax racket/base
                     (only-in racket/syntax syntax-local-eval)
                     syntax/parse))

;;; Basic type predicates

(def-conj int? exact-integer?)
(def-conj str? string?)
(def-conj vec? vector?)
(def-conj float? flonum?)
(def-conj bool? boolean?)
(def-conj proc? procedure?)
(def-conj contract? r:contract?)
(def-conj flat_contract? r:flat-contract?)

;; Comparison functions

(define (cmp a b)
  (dssl-send a '__cmp__ b
             #:or-else
             (flip-order
               (dssl-send b '__cmp__ a
                          #:or-else dssl-None))))

(define (flip-order order)
  (if (int? order)
    (- order)
    dssl-None))

(define ((build-find-extreme name prefer?) new old)
  (truthy-cond
    [(cmp new old)
     =>
     (λ (order) (if (prefer? order) new old))]
    [else
      (dssl-error "%s: not ordered: %p and %p" name old new)]))

(define (max x . xs)
  (foldl (build-find-extreme "max" positive?) x xs))

(define (min x . xs)
  (foldl (build-find-extreme "min" negative?) x xs))

;; length

(define (len x)
  (dssl-send x 'len))

;; Numeric predicates

(define (num? x)
  (or (int? x) (float? x)))

(def-conj nan?  num? r:nan?)
(def-conj zero? num? r:zero?)
(def-conj pos?  num? r:positive?)
(def-conj neg?  num? r:negative?)
(def-conj even? num? r:even?)
(def-conj odd?  num? r:odd?)

(def-conj nat?  int? (compose not r:negative?))

;; Contracts

(define NoneC (flat-named-contract 'NoneC void?))

(define (format-fun f x xs)
  (define port (open-output-string))
  (fprintf port "~a(~a" f (dssl-contract-name x))
  (for ([xi (in-list xs)])
    (fprintf port ", ~a" (dssl-contract-name xi)))
  (fprintf port ")")
  (get-output-string port))

(define VecC
  (special-square-bracket-contract
    VecC
    #:generic (c) (r:vectorof c)
    #:default vec?))

(define TupC
  (special-square-bracket-contract
    TupC
    #:generic cs (apply r:vector/c cs
                        #:flat? (andmap r:flat-contract? cs))
    #:default vec?))

(define (NotC c)
  (rename-contract
    (r:not/c c)
    (format-fun 'NotC c '())))

(define (OrC c . cs)
  (rename-contract
    (apply or/c c cs)
    (format-fun 'OrC c cs)))

(define (AndC c . cs)
  (rename-contract
    (apply and/c c cs)
    (format-fun 'AndC c cs)))

(define (FunC-helper all)
  (define rev-all (reverse all))
  (define args    (reverse (cdr rev-all)))
  (define res     (car rev-all))
  (dynamic->* #:mandatory-domain-contracts args
              #:range-contracts (list res)))

(define FunC
  (special-square-bracket-contract
    FunC
    #:generic (c . cs) (FunC-helper (cons c cs))
    #:default proc?))

(define (SquareBracketC name builder)
  (build-square-bracket-contract name builder))

(define (IntInC low high)
  (rename-contract
    (integer-in (falsy->false low)
                (falsy->false high))
    (format-fun 'IntInC low (list high))))

(define apply_contract
  (case-lambda
    [(contract value pos neg)
     (r:contract contract value pos neg)]
    [(contract value pos)
     (apply_contract contract value pos "the context")]
    [(contract value)
     (apply_contract contract value
                     "the contracted value")]))

(define (make_contract name first-order? projection)
  (define real-check
    (if (eq? dssl-None first-order?)
      (λ (_v) #t)
      first-order?))
  (cond
    [(NoneC projection)
     (flat-named-contract name first-order?)]
    [else
      (define real-projection
        (λ (blame)
           (λ (value party)
              (define (blame! message . argv)
                (apply raise-blame-error
                       blame
                       #:missing-party party
                       value
                       message
                       argv))
              (if (real-check value)
                (projection blame! value)
                (blame! '(given: "~a" expected: "~a")
                        value name)))))
      (make-contract #:name name
                     #:first-order real-check
                     #:late-neg-projection real-projection)]))

;; I/O operations

(define current_directory
  (case-lambda
    [()    (~a (current-directory))]
    [(dir) (current-directory dir)]))

(define (file_to_string filename)
  (file->string filename #:mode 'binary))

(define (string_to_file value filename)
  (display-to-file value filename
                   #:mode 'binary
                   #:exists 'truncate/replace))

(define (print arg0 . args)
  (apply dssl-printf arg0 args))

(define (eprint arg0 . args)
  (apply dssl-fprintf (current-error-port) arg0 args))

(define (fprintln port . args)
  (cond
    [(null? args)
     (newline port)]
    [(string? (car args))
     (apply dssl-fprintf port args)
     (newline port)]
    [else
      (dssl-fprintf port "%p" (car args))
      (for ([arg (in-list (cdr args))])
        (dssl-fprintf port ", %p" arg))
      (newline port)]))

(define (eprintln . args)
  (apply fprintln (current-error-port) args))

(define (println . args)
  (apply fprintln (current-output-port) args))

(define (sleep sec)
  (r:sleep sec))

;; Randomness

; This is the largest argument that `random` can take.
(define RAND_MAX 4294967087)

(define random
  (case-lambda
    [() (r:random)]
    [(limit) (r:random limit)]
    [(low high) (r:random low high)]))

(define (random_bits n)
  (cond
    [(zero? n)      0]
    [else           (+ (* 2 (random_bits (sub1 n)))
                       (random 2))]))

;; Primitive interfaces & classes

(define-dssl-interface ITERABLE () ()
  ([iterator () () AnyC]))

(define-dssl-interface ITERATOR () ((ITERABLE))
  ([try_advance () (AnyC) AnyC]))

(define (range-non-empty? current step limit)
  (cond
    [(positive? step) (< current limit)]
    [(negative? step) (> current limit)]
    [else             #false]))

(define-dssl-class range_iterator () (ITERATOR)
  ([_cur AnyC] [_step AnyC] [_lim AnyC])
  ([__init__ () self ([cur AnyC] [step AnyC] [lim AnyC]) AnyC
     (begin
       (dssl-self _cur cur)
       (dssl-self _step step)
       (dssl-self _lim lim))]
   [current () self () AnyC
     (dssl-self _cur)]
   [step () self () AnyC
     (dssl-self _step)]
   [limit () self () AnyC
     (dssl-self _lim)]
   [empty? () self () AnyC
     (not (range-non-empty? (dssl-self _cur)
                            (dssl-self _step)
                            (dssl-self _lim)))]
   [iterator () self () AnyC
     (range_iterator (dssl-self _cur) (dssl-self _step) (dssl-self _lim))]
   [try_advance () self ([visit AnyC]) AnyC
     (and
      (range-non-empty? (dssl-self _cur) (dssl-self _step) (dssl-self _lim))
      (begin
        (visit (dssl-self _cur))
        (dssl-self _cur (+ (dssl-self _cur) (dssl-self _step)))
        #true))]))

(define range
  (case-lambda
    [(start step limit) (range_iterator start step limit)]
    [(start limit)      (range_iterator start 1    limit)]
    [(limit)            (range_iterator 0     1    limit)]))

(define (index-ref indexable ix)
  (cond
    [(vector? indexable) (vector-ref indexable ix)]
    [(string? indexable) (string-ref indexable ix)]
    [(get-method-value/fun indexable '__index_ref__)
     =>
     (λ (__index_ref__) (__index_ref__ ix))]
    [else
      (dssl-error "index_iterator: source is not indexable")]))

(define-dssl-class index_iterator () (ITERATOR)
  ([_source AnyC] [_current AnyC] [_limit AnyC])
  ([__init__ () self ([source AnyC] [current AnyC] [limit AnyC]) AnyC
     (begin
       (dssl-self _source source)
       (dssl-self _current current)
       (dssl-self _limit limit))]
   [source () self () AnyC
     (dssl-self _source)]
   [current () self () AnyC
     (dssl-self _current)]
   [limit () self () AnyC
     (dssl-self _limit)]
   [iterator () self () AnyC
     (index_iterator (dssl-self _source)
                     (dssl-self _current)
                     (dssl-self _limit))]
   [try_advance () self ([visit AnyC]) AnyC
     (and (< (dssl-self _current)
             (dssl-self _limit))
          (begin
            (visit (index-ref (dssl-self _source) (dssl-self _current)))
            (dssl-self _current (add1 (dssl-self _current)))
            #true))]))

(define bool
  (case-lambda
    [() #f]
    [(x) (truthy? x)]))

(define-unwrapped-class bool-class bool bool?
  (; conversions
   [__float__   (λ (self) (if self 1.0 0.0))]
   [__num__     (λ (self) (if self 1 0))]
   [__int__     (λ (self) (if self 1 0))]
   ; unary operators
   [__invert__  (λ (self) (not self))]
   ; binary operators
   [__cmp__     (make-cmp boolean? bool<? bool>?)]
   [__and__     prim:and]
   [__rand__    prim:and]
   [__or__      prim:or]
   [__ror__     prim:or]
   [__xor__     prim:xor]
   [__rxor__    prim:xor]))

(define char
  (case-lambda
    [() (char 0)]
    [(val) (char/internal 'char val)]))

(define-unwrapped-class char-class char char?
  (; conversions
   [__int__     (λ (self) (char->integer self))]
   ; binary methods
   [__cmp__     (make-cmp char? char<? char>?)]))

(define (char/internal who val)
  (cond
    [(char? val) val]
    [(int? val) (integer->char val)]
    [(and (str? val) (= 1 (string-length val)))
     (string-ref val 0)]
    [else
      (raise-repr-error who val "int code point or singleton string")]))

(define int
  (case-lambda
    [() 0]
    [(x)
     (cond
       [(int? x) x]
       [(dssl-send x '__int__ #:and-then box #:or-else #f)
        => unbox]
       [else
         (raise-repr-error
           'int x
           "number, string, Boolean, or object responding to __int__")])]))

(define-unwrapped-class int-class int int?
  (; conversions
   [__int__     (λ (self) self)]
   [__float__   (λ (self) (exact->inexact self))]
   [__num__     (λ (self) self)]
   ; unary operators
   [__neg__     (λ (self) (- self))]
   [__pos__     (λ (self) self)]
   [__invert__  (λ (self) (bitwise-not self))]
   ; binary operators
   [__cmp__     (make-cmp num? < >)]
   [__add__     (num-binop __add__ r:+ '__radd__)]
   [__radd__    (num-binrop __radd__ r:+)]
   [__sub__     (num-binop __sub__ r:- '__rsub__)]
   [__rsub__    (num-binrop __rsub__ r:-)]
   [__mul__     (num-binop __mul__ r:* '__rmul__)]
   [__rmul__    (num-binrop __rmul__ r:*)]
   [__div__     (num-binop __div__ prim:div '__rdiv__)]
   [__rdiv__    (num-binrop __rdiv__ prim:div)]
   [__pow__     (num-binop __pow__ expt '__rpow__)]
   [__rpow__    (num-binrop __rpow__ expt)]
   [__mod__     (int-binop __mod__ modulo '__rmod__)]
   [__rmod__    (int-binrop __rmod__ modulo)]
   [__and__     (int-binop __and__ bitwise-and '__rand__)]
   [__rand__    (int-binrop __rand__ bitwise-and)]
   [__or__      (int-binop __or__ bitwise-ior '__ror__)]
   [__ror__     (int-binrop __ror__ bitwise-ior)]
   [__xor__     (int-binop __xor__ bitwise-xor '__rxor__)]
   [__rxor__    (int-binrop __rxor__ bitwise-xor)]
   [__lshift__  (int-binop __lshift__ arithmetic-shift '__rlshift__)]
   [__rlshift__ (int-binrop __rlshift__ arithmetic-shift)]
   [__rshift__  (int-binop __lshift__ right-shift '__rrshift__)]
   [__rrshift__ (int-binrop __rrshift__ right-shift)]
   ; public methods
   [abs         (λ (self) (r:abs self))]
   [floor       (λ (self) self)]
   [iterator    (λ (self) (range_iterator 0 1 self))]
   [ceiling     (λ (self) self)]
   [sqrt        (λ (self) (real-sqrt self))]
   [sin         (λ (self) (r:sin self))]
   [cos         (λ (self) (r:cos self))]
   [tan         (λ (self) (r:tan self))]
   [asin        (λ (self) (r:asin self))]
   [acos        (λ (self) (r:acos self))]
   [atan        (λ (self . other) (apply r:atan self other))]))

(define (real-sqrt n)
  (if (< n 0)
    (dssl-error "sqrt: cannot handle a negative")
    (sqrt n)))

(define float
  (case-lambda
    [() 0.0]
    [(x)
     (cond
       [(float? x) x]
       [(dssl-send x '__float__ #:and-then box #:or-else #f)
        => unbox]
       [else
         (raise-repr-error
           'float x
           "number, string, Boolean, or object responding to __float__")])]))

(define-unwrapped-class float-class float float?
  (; conversions
   [__float__   (λ (self) self)]
   [__num__     (λ (self) self)]
   [__int__     (λ (self) (inexact->exact (truncate self)))]
   ; unary operators
   [__neg__     (λ (self) (- self))]
   [__pos__     (λ (self) self)]
   [__invert__  (λ (self) (bitwise-not self))]
   ; binary operators
   [__cmp__     (make-cmp num? < >)]
   [__add__     (num-binop __add__ r:+ '__radd__)]
   [__radd__    (num-binrop __radd__ r:+)]
   [__sub__     (num-binop __sub__ r:- '__rsub__)]
   [__rsub__    (num-binrop __rsub__ r:-)]
   [__mul__     (num-binop __mul__ r:* '__rmul__)]
   [__rmul__    (num-binrop __rmul__ r:*)]
   [__div__     (num-binop __div__ r:/ '__rdiv__)]
   [__rdiv__    (num-binrop __rdiv__ r:/)]
   [__pow__     (num-binop __pow__ expt '__rpow__)]
   [__rpow__    (num-binrop __rpow__ expt)]
   ; public methods
   [abs         (λ (self) (r:abs self))]
   [floor       (λ (self) (inexact->exact (r:floor self)))]
   [ceiling     (λ (self) (inexact->exact (r:ceiling self)))]
   [sqrt        (λ (self) (real-sqrt self))]
   [sin         (λ (self) (r:sin self))]
   [cos         (λ (self) (r:cos self))]
   [tan         (λ (self) (r:tan self))]
   [asin        (λ (self) (r:asin self))]
   [acos        (λ (self) (r:acos self))]
   [atan        (λ (self . other) (apply r:atan self other))]))

(define proc
  (case-lambda
    [()    identity]
    [(val)
     (cond
       [(proc? val) val]
       [(dssl-send val '__proc__ #:and-then box #:or-else #f)
        => unbox]
       [else
         (raise-repr-error
           'proc val
           "proc or object responding to __proc__ method")])]))

(define-unwrapped-class proc-class proc proc?
  ([compose           (λ (self other)
                         (λ args
                            (self (apply other args))))]
   [vec_apply         (λ (self v)
                         (apply self (vector->list v)))]))

(define str
  (case-lambda
    [() ""]
    [(val)
     (cond
       [(str? val) val]
       [else       (dssl-format "%p" val)])]
    [(len c)
     (make-string len (char/internal 'str c))]))

(define-unwrapped-class str-class str str?
  (; conversions
   [__int__       (λ (self)
                     (cond
                       [(string->number self)
                        =>
                        (λ (self) (inexact->exact (truncate self)))]
                       [else
                         (raise-runtime-error
                           "str.__int__: bad int format in %p" self)]))]
   [__float__     (λ (self)
                     (cond
                       [(string->number self)
                        =>
                        exact->inexact]
                       [else
                         (raise-runtime-error
                           "str.__float__: bad float format in %p" self)]))]
   ; binary methods
   [__eq__        (-> str? AnyC)
                  (λ (self other) (string=? self other))]
   [__cmp__       (make-cmp string? string<? string>?)]
   [__add__       (λ (self other)
                     (if (str? other)
                       (~a self other)
                       (dssl-format "%s%p" self other)))]
   [__radd__      (λ (self other)
                     (if (str? other)
                       (~a other self)
                       (dssl-format "%p%s" other self)))]
   ; char indexing
   [__index_ref__ (-> nat? AnyC)
                  prim:str.__index_ref__]
   ; public methods
   [iterator      (λ (self)
                     (index_iterator self 0 (string-length self)))]
   [len           (λ (self) (string-length self))]
   [explode       (λ (self) (list->vector (string->list self)))]
   [format        (λ (self . args)
                     (apply dssl-format self args))]))

(define (bounds-check who index limit)
  (unless (< index limit)
    (dssl-error
      "%s: out of bounds:\n  got index: %p\n  expected: 0 ≤ index < %p"
      who index limit)))

(define (prim:str.__index_ref__ self ix)
  (bounds-check "str.__index_ref__" ix (string-length self))
  (string-ref self ix))

(define vec
  (case-lambda
    [() (vector)]
    [(size) (make-vector size dssl-None)]
    [(size init) (build-vector size init)]))

(define-unwrapped-class vec-class vec vec?
  ([__index_ref__ (-> nat? AnyC)
                  prim:vec.__index_ref__]
   [__index_set__ (-> nat? AnyC AnyC)
                  prim:vec.__index_set__]
   [__eq__        (-> vec? AnyC)
                  prim:vec.__eq__]
   [len           vector-length]
   [implode       prim:vec.implode]
   [iterator      (λ (self)
                     (index_iterator self 0 (vector-length self)))]
   [map           (-> (-> AnyC AnyC) AnyC)
                  prim:vec.map]
   [filter        (-> (-> AnyC AnyC) AnyC)
                  prim:vec.filter]))

(define (prim:vec.__index_ref__ self ix)
  (bounds-check "vec.__index_ref__" ix (vector-length self))
  (vector-ref self ix))

(define (prim:vec.__index_set__ self ix val)
  (bounds-check "vec.__index_set__" ix (vector-length self))
  (vector-set! self ix val))

(define (prim:vec.__eq__ self other)
  (define o-ref (get-method-value other '__index_ref__))
  (and (eq? (vector-length self) (dssl-send other 'len))
       (for/and ([element (in-vector self)]
                 [i       (in-naturals 0)])
         (dssl-equal? element (o-ref i)))))

(define (prim:vec.implode self)
  (define (convert c)
    (cond
      [(char? c)    c]
      [(integer? c) (integer->char c)]
      [else (raise-repr-error
              'vec.implode c "char or int code point")]))
  (list->string
    (r:map convert (vector->list self))))

(define (prim:vec.map self f)
  (build-vector
    (vector-length self)
    (λ (i) (f (vector-ref self i)))))

(define (prim:vec.filter self pred)
  (list->vector
    (r:filter pred (vector->list self))))

(define-values-for-syntax (*dir-table* *method-table*)
  (make-unwrapped-class-table
    bool-class
    char-class
    int-class
    float-class
    proc-class
    str-class
    vec-class))

;; Primitive class helpers

(define-syntax (int-binrop stx)
  (syntax-parse stx
    [(_ name:id op:expr)
     #'(λ (a b)
          (cond
            [(and (int? a) (int? b))
             (op b a)]
            [else
              (raise-repr-error 'name
                                (if (int? a) b a)
                                "int")]))]))

(define-syntax (int-binop stx)
  (syntax-parse stx #:literals (quote)
    [(_ name:id op:expr (quote rop:id))
     #'(λ (a b)
          (cond
            [(int? b) (op a b)]
            [(dssl-send b 'rop a #:and-then box #:or-else #f)
             => unbox]
            [else
              (raise-repr-error
                'name b
                (r:format "int or object responding to ~a method"
                          'rop))]))]))

(define-syntax (num-binrop stx)
  (syntax-parse stx
    [(_ name:id op:expr)
     #'(λ (a b)
          (cond
            [(and (num? a) (num? b))
             (op b a)]
            [else
              (raise-repr-error 'name
                                (if (num? a) b a)
                                "num")]))]))

(define-syntax (num-binop stx)
  (syntax-parse stx #:literals (quote)
    [(_ name:id op:expr (quote rop:id))
     #'(λ (a b)
          (cond
            [(num? b) (op a b)]
            [(dssl-send b 'rop a #:and-then box #:or-else #f)
             => unbox]
            [else
              (raise-repr-error
                'name b
                (r:format "num or object responding to ~a method"
                          'rop))]))]))

(define (bool<? a b)
  (and (not a) b))

(define (bool>? a b)
  (and a (not b)))

(define (make-cmp dom? <? >?)
  (λ (a b)
     (cond
       [(not (dom? b))  dssl-None]
       [(<? a b)        -1]
       [(>? a b)        1]
       [else            0])))

(define (right-shift a b)
  (arithmetic-shift a (- b)))

(define (prim:div a b)
  (cond
    [(eq? 0 b) (raise-runtime-error "/: division by zero")]
    [(and (int? a) (int? b)) (quotient a b)]
    [else                    (r:/ a b)]))

(define-simple-macro (logical-binop name:id bool-op:expr int-op:expr)
  (λ (a b)
     (cond
       [(and (bool? a) (bool? b))
        (bool-op a b)]
       [(and (int? a) (int? b))
        (int-op a b)]
       [(and (int? a) (bool? b))
        (int-op a (if b 1 0))]
       [(and (bool? a) (int? b))
        (int-op (if a 1 0) b)]
       [else
         (raise-repr-error 'name
                           (if (bool? a) b a)
                           "bool or int")])))

(define prim:and (logical-binop __and__ and bitwise-and))
(define prim:or (logical-binop __or__ or bitwise-ior))
(define prim:xor (logical-binop __xor__ (λ (a b) (not (eq? a b))) bitwise-xor))

;; Method fetching
;;
;; This all belongs in private/object.rkt, except that we can't
;; define it until after we defined *dir-table* and *method-table*,
;; and those happen in this file.

(define-syntax (get-method-list stx)
  (define dir-table (syntax-local-eval #'*dir-table*))
  (syntax-parse #`(#,stx #,dir-table) #:literals (quote)
    [((_ obj:expr) ((pred:expr sel:expr ...) ...))
     #'(let ([value obj])
         (cond
           [(object-base? value)
            (for/list ([method-info
                         (in-vector (object-info-method-infos
                                      (object-base-info value)))])
              (symbol->string (method-info-name method-info)))]
           [(pred value)
            (list (symbol->string sel) ...)]
           ...
           [else #f]))]))

(define-syntax (get-method-value stx)
  (syntax-parse stx #:literals (quote)
    [(_ receiver:expr (quote sel:id))
     (define method-table (syntax-local-eval *method-table*))
     (define candidates (hash-ref method-table (syntax-e #'sel) '()))
     (syntax-parse candidates
       [((pred . method) ...)
        #`(let ([value receiver])
            (cond
              [(get-method-value/fun value 'sel)
               => identity]
              [(pred value)
               (method value)]
              ...
              [else #f]))])]))

(define-syntax (get-method-value/or-else stx)
  (syntax-parse stx #:literals (quote)
    [(_ object:expr (quote method:id) context:expr)
     #'(let ([value object])
         (or (get-method-value value 'method)
             (raise-runtime-error "object %p does not have method %s"
                                  object 'method
                                  #:context context)))]))

(begin-for-syntax
  (define-splicing-syntax-class
    optional-and-then
    (pattern (~seq #:and-then expr:expr))
    (pattern (~seq)
             #:with expr #'identity))

  (define-splicing-syntax-class
    optional-or-else
    (pattern (~seq #:or-else expr:expr))
    (pattern (~seq)
             #:with expr #f)))

(define-syntax (dssl-send stx)
  (syntax-parse stx #:literals (quote)
    [(_ obj:id (quote sel:id) arg:expr ...
        and-then:optional-and-then
        or-else:optional-or-else)
     (with-syntax
       ([or-else-expr
          (syntax-parse #'or-else
            [(#:or-else expr:expr) #'expr]
            [() #'(raise-repr-error
                    'send obj
                    (r:format "object responding to ~a method" 'sel))])])
       #'(cond
           [(get-method-value obj 'sel)
            =>
            (λ (method) (and-then.expr (method arg ...)))]
           [else or-else-expr]))]))

(define (get-try-advance obj who context)
  (define iterator
    (dssl-send obj 'iterator
               #:or-else
               (raise-repr-error
                 who obj "object responding to .iterator()"
                 #:context context)))
  (get-method-value/or-else iterator 'try_advance context))

;; Listing the methods of an object.

(define (dir obj)
  (cond
    [(get-method-list obj)
     =>
     list->vector]
    [(struct-base? obj)
     (list->vector (get-field-list obj))]
    [(void? obj)
     (vector)]
    [else (raise-repr-error 'dir obj "a struct or object")]))

;; EQUALITY

; A Chain is one of:
;  - [Box Natural]
;  - [Box Chain]

; union-find!? : [EqHashTbl Any Chain] Any Any -> Boolean
; Associates `a` and `b` in the hash table, and returns whether they
; were associated already.
(define (union-find!? h x y)
  (define (find b)
    (define n (unbox b))
    (if (box? n)
        (let loop ([b b] [n n])
          (define nn (unbox n))
          (if (box? nn)
              (begin
                (set-box! b nn)
                (loop n nn))
              n))
        b))
  (define bx (hash-ref h x #f))
  (define by (hash-ref h y #f))
  (cond
    [(and bx by)
     (define rx (find bx))
     (define ry (find by))
     (cond
       [(eq? rx ry) #t]
       [else
         (define nx (unbox bx))
         (define ny (unbox by))
         (cond
           [(> nx ny)
            (set-box! ry rx)
            (set-box! rx (+ nx ny))]
           [else
            (set-box! rx ry)
            (set-box! ry (+ nx ny))])
         #f])]
    [bx
     (define rx (find bx))
     (hash-set! h y rx)
     #f]
    [by
     (define ry (find by))
     (hash-set! h x ry)
     #f]
    [else
     (define b (box 1))
     (hash-set! h x b)
     (hash-set! h y b)
     #f]))

(define current-equal-table (make-parameter #f))

(define-simple-macro (with-equal-table body:expr ...)
  (parameterize ([current-equal-table (or (current-equal-table)
                                          (make-hasheq))])
    body ...))

(define (seen!? a b)
  (union-find!? (current-equal-table) a b))

(define (dssl-equal? a0 b0)
  (with-equal-table
    (let compare ([a a0] [b b0])
      (cond
        ; We try number? before eq?, to get correct treatement of nan.
        [(number? a)            (and (number? b) (= a b))]
        ; This case covers equality for booleans, contracts, and
        ; procedures as well as physically equal pointers.
        [(eq? a b)              #true]
        [else
          (define a.__eq__    (get-method-value a '__eq__))
          (define a.__class__ (get-method-value a '__class__))
          (cond
            [(and a.__eq__ a.__class__
                  (eq? a.__class__ (get-method-value b '__class__)))
             (or (seen!? a b)
                 (a.__eq__ b))]
            [(eq? 0 (cmp a b))
             #t]
            [(struct-base? a)
             (or (seen!? a b)
                 (and (struct-base? b)
                      (struct-equal? a b compare)))]
            [(object-base? a)
             (or (seen!? a b)
                 (and (object-base? b)
                      a.__class__
                      (eq? a.__class__ (get-method-value b '__class__))
                      (object-equal? a b compare)))]
            [else #false])]))))

(define (struct-equal? a b compare)
  (define info (struct-base-struct-info a))
  (and (eq? info (struct-base-struct-info b))
       (for/and ([field-info (struct-info-field-infos info)])
         (define getter (field-info-getter field-info))
         (compare (getter a) (getter b)))))

(define (object-equal? a b compare)
  (for/and ([a-pair (in-vector ((object-base-reflect a)))]
            [b-pair (in-vector ((object-base-reflect b)))])
    (compare (cdr a-pair) (cdr b-pair))))
