#lang racket/base

(provide current-dssl-contract-name
         generic-base?
         generic-base-name
         generic-base-instantiate
         generic-proc
         generic-proc?
         generic-proc-tag
         square-bracket-class-lambda
         square-bracket-proc
         define-square-bracket-proc
         square-bracket-proc-contract
         square-bracket-contract
         build-square-bracket-contract
         special-square-bracket-contract)

(require "errors.rkt"
         (only-in "contract.rkt"
                  AnyC
                  ensure-contract))
(require (for-syntax syntax/stx
                     "errors.rkt"
                     "names.rkt"
                     "util.rkt"))
(require syntax/parse/define)
(require (only-in racket/function
                  arity-includes?)
         (only-in racket/contract/region
                  define/contract)
         (only-in racket/contract/base
                  contract
                  ->
                  ->i
                  contract-name
                  rename-contract
                  contract-late-neg-projection)
         (only-in racket/contract/combinator
                  make-contract
                  prop:contract
                  build-contract-property
                  raise-blame-error
                  blame-positive
                  contract-first-order)
         (only-in racket/list
                  make-list)
         (only-in racket/port
                  call-with-output-string))
(require (for-syntax racket/base
                     (only-in racket/syntax
                              generate-temporary
                              format-id
                              define/with-syntax)))

; The printer will change this to override how contract names are
; composed. This is to avoid a circular dependency between this
; module and "printer.rkt".
(define current-dssl-contract-name (make-parameter contract-name))

; This is for generic things that can be instantiated
; with square brackets. They also have names for printing.
(struct generic-base (name instantiate))

; Generic procs are functions with optional square-bracket
; arguments. That is, they can be instantiated or directly
; applied. The tag is 'class or 'proc, for printing.
(struct generic-proc generic-base (apply tag)
  #:property prop:procedure
  (struct-field-index apply))

; Generic contracts can be instantiated or used as contracts
; directly.
(struct generic-contract generic-base (first-order late-neg-projection)
  #:property prop:contract
  (build-contract-property
    #:name generic-base-name
    #:first-order (λ (v) (generic-contract-first-order v))
    #:late-neg-projection (λ (v) (generic-contract-late-neg-projection v))))

; format-generic : symbol? [List-of contract?] -> string?
; Formats the application of a generic procedure to its optional (square
; bracket) arguments.
(define (format-generic f xs)
  (define contract-name (current-dssl-contract-name))
  (call-with-output-string
    (λ (port)
       (display f port)
       (write-char #\[ port)
       (when (pair? xs)
         (let loop ([x0 (car xs)]
                    [xs (cdr xs)])
           (display (contract-name x0) port)
           (when (pair? xs)
             (write-string ", " port)
             (loop (car xs) (cdr xs)))))
       (write-char #\] port))))

(struct print-literally (string)
  #:methods
  gen:custom-write
  [(define (write-proc self port _mode)
     (write-string (print-literally-string self) port))])


; Creates a class, which may or may not have generic parameters.
; If there are generic parameters, they must be given defaults.
; Here we create a generic function that can be called like
; `Pair[T, U](fst, snd)` or `Pair(fst, snd)`. In the latter
; case, `T` and `U` default to `AnyC`:
;
;     (square-bracket-class-lambda
;       Pair
;       ([T AnyC] [U AnyC])
;       (fst snd)
;       ...body...)
(define-syntax (square-bracket-class-lambda stx)
  (syntax-parse stx
    [(_ name:id
        ()
        (formal:id ...)
        body:expr ...+)
     #'(λ (formal ...) body ...)]
    [(_ name:id
        ([opt-formal:id default:expr] ...+)
        (formal:id ...)
        body:expr ...+)
     #'(let ([instantiate
               (λ (opt-formal ...)
                  (procedure-rename
                    (λ (formal ...) body ...)
                    (string->symbol
                      (format-generic 'name (list opt-formal ...)))))])
         (generic-proc 'name
                       instantiate
                       (instantiate default ...)
                       'class))]))

; Creates a procedure that can be applied with square brackets or
; without.
;
;     (square-bracket-proc make_pair
;       #:generic (T U) ...body_1...
;       #:default ...body_2...
;
; In the above example, `make_pair[V, W](x)` means `...body_1...[V/T,
; W/U](x)`, whereas `make_pair(x)` means `...body_2...(x)`. That is,
; both bodies must evaluate to procedures, and `T` and `U` may be used
; in `...body_1...` only.
(define-syntax (square-bracket-proc stx)
  (syntax-parse stx
    [(_ name:id
        #:generic (opt-formal:id ...+) generic:expr
        #:default default:expr)
     #'(generic-proc
         'name
         (λ (opt-formal ...)
            (procedure-rename
              generic
              (string->symbol
                (format-generic 'name (list opt-formal ...)))))
         default
         'proc)]
    [(_ name:id
        #:generic () generic:expr
        #:default default:expr)
     #'default]))

; compose-neg-party : String Syntax -> String
; This is a helper for define-square-bracket-proc that is used
; for creating negative parties once they become known. It must
; be given a string describing what kind of negative party it is
; and a string whose source location describes where.
(define-for-syntax (compose-neg-party who where)
  (format "~a at ~a" who (srcloc->string (get-srcloc/fn where))))

(define (instantiate-square-bracket-proc name
                                         the-contract
                                         the-function
                                         neg-party
                                         srcloc)
  (contract the-contract
            (procedure-rename the-function name)
            (format "def ~a at ~a" name (srcloc->string srcloc))
            neg-party
            name
            srcloc))

; For defining a user square bracket proc, having optional contract
; params that default to `AnyC`. The syntax is
;
;     (define-square-bracket-proc
;       ((make_pair T U) [fst T] [snd U])
;       Pair?
;       ...body...)
;
; That is, the above defines `make_pair` as a a curried function that
; first takes two generic contract parameters `T` and `U`, and then
; takes two ordinary parameters, `fst` (which is protected by contract
; `T`) and `snd` (which is protected by conrract `U`). In DSSL2, `T` and
; `U` must be supplied as square bracket parameters, and default to
; `AnyC` if `make_pair` is applied with parentheses.
(define-syntax (define-square-bracket-proc stx)
  (syntax-parse stx
    [(_ ((name:id opt-formal:id ...) [formal:id formal-ctc:expr] ...)
        result-ctc:expr
        body:expr)
     (define no-opt-formals? (stx-null? #'(opt-formal ...)))
     ;; generate-temporary would be the "proper" solution, but the numbers
     ;; after the name may be confusing to students
     (define/with-syntax real-definition
       (format-id #'name "~a·" (syntax-e #'name)))
     #`(begin
         #,(if no-opt-formals?
             #'(define instantiate-contract
                 (let ([the-contract (-> formal-ctc ... result-ctc)])
                   (λ () the-contract)))
             #'(define (instantiate-contract opt-formal ...)
                 (-> formal-ctc ... result-ctc)))
         (define (instantiate neg-party opt-formal ...)
           (instantiate-square-bracket-proc
             'name
             (instantiate-contract opt-formal ...)
             (λ (formal ...) body)
             neg-party
             (get-srcloc name)))
         #,(if no-opt-formals?
             #'(define real-definition instantiate)
             #'(define (real-definition neg-party)
                 (square-bracket-proc
                   name
                   #:generic (opt-formal ...)
                             (instantiate neg-party opt-formal ...)
                   #:default (let ([opt-formal AnyC] ...)
                               (instantiate neg-party opt-formal ...)))))
         (define-syntax name
           (make-set!-transformer
             (λ (stx)
                (syntax-parse stx #:literals (set!)
                  [(set! _:id e:expr)
                   (syntax-error
                     stx
                     "cannot assign to def'd function or method")]
                  [(_:id . rest)
                   (with-syntax
                     ([app         (datum->syntax stx '#%app)]
                      [neg-party   (compose-neg-party "caller" stx)])
                     (syntax/loc stx
                       (app (real-definition neg-party) . rest)))]
                  [_:id
                    (with-syntax
                      ([neg-party  (compose-neg-party "usage" stx)])
                      (syntax/loc stx
                        (real-definition neg-party)))])))))]))

(define ((arities->sbp? contract-arity regular-arity) proc)
  (and (generic-proc? proc)
       (arity-includes?
         (procedure-arity (generic-proc-apply proc))
         regular-arity)
       (arity-includes?
         (procedure-arity (generic-base-instantiate proc))
         contract-arity)))

;; This helper is a HACK for allowing the name of an interface inside contracts
;; within its definition. For example, persistent data structures that have
;; methods that return instances of the data structure.
;; Currently, this detects (in a very crude way) when the interface name is used
;; *directly* as a contract for a parameter or a result (basically, a 1-level
;; very half-assed tree-walker). The proper solution would probably be to use
;; syntax parameters to change the meaning of the interface name in the body of
;; its definition, but I couldn't get it to work; my macro-fu is too weak.
;; Also, I couldn't get these references to do any actual checking. Not sure
;; why, but at least now it's possible to *write* these contracts...
;; -- stamourv, Dec 2021
;; See the recursive-interface test.
(define-for-syntax (fix-recursive-interface-contract name found)
  #`(ensure-contract 'def
                     #,(syntax-parse found
                         ;; Hack on top of hack: this should look for the actual
                         ;; `vec-ref` binding. But can't require it; cyclic
                         ;; dependency...
                         [(vec-ref maybe-name param)
                          (if (free-identifier=? #'maybe-name name)
                              ;; THIS DOES NOT WORK. it doesn't crash, but it
                              ;; doesn't actually check anything either
                              ;; eta, to avoid binding issues
                              ;; #`(lambda (x) (#,(interface-contract-name name) x))
                              #'AnyC ; HACK!
                              #'maybe-name)]
                         [_
                          found])))

; Creates a contract for a procedure that accepts optional, square
; bracket arguments.
;
; Example:
;
;     (square-bracket-proc-contract make_pair [T U] T U Pair?)
;
; Here, the optional parameters `T` and `U` give contracts for the
; required parameters, and default to `AnyC`.
; Observation (stamourv, Dec 2021): only used when defining interfaces
(define-syntax (square-bracket-proc-contract stx)
  (syntax-parse stx
    [(_ name:id interface-name [] formal:expr ... result:expr)
     #`(-> #,@(for/list ([f (syntax->list #'(formal ...))])
                (fix-recursive-interface-contract #'interface-name f))
           #,(fix-recursive-interface-contract #'interface-name #'result))]
    ;; This clause is for method prototypes within interface definitions which
    ;; introduce extra contract parameters (beyond those defined by the
    ;; interface itself). pretty niche, and was broken before. This code doesn't
    ;; crash anymore, but I'm not too confident in its correctness.
    ;; -- stamourv, Dec 2021
    [(_ name:id interface-name [opt-formal:id ...+] formal:expr ... result:expr)
     #`(let ()
         (define first-order?
           (arities->sbp? #,(syntax-length #'(opt-formal ...))
                          #,(syntax-length #'(formal ...))))
         (define (instantiate-contract opt-formal ...)
           (-> #,@(for/list ([f (syntax->list #'(formal ...))])
                    (fix-recursive-interface-contract #'interface-name f))
               #,(fix-recursive-interface-contract #'interface-name #'result)))
         (define ((late-neg-proj blame) value missing-party)
           (cond
             [(first-order? value)
              (generic-proc
                (generic-base-name value)
                (λ (opt-formal ...)
                   (contract
                     (instantiate-contract opt-formal ...)
                     ((generic-base-instantiate value) opt-formal ...)
                     (blame-positive blame)
                     missing-party
                     'name #f))
                (contract
                  (instantiate-contract
                    (make-list #,(syntax-length #'(opt-formal ...)) AnyC))
                  (generic-proc-apply value)
                  (blame-positive blame)
                  missing-party
                  'name #f)
                (generic-proc-tag value))]
             [else
               (raise-blame-error
                 blame #:missing-party missing-party value
                 "not a generic procedure")]))
         (make-contract #:first-order first-order?
                        #:late-neg-projection late-neg-proj))]))

; Creates a contract that accepts optional, square bracket parameters.
; The contract is specified by giving a sequence of optional parameters
; with defaults, and then the first-order check and late negative
; projection.
(define-syntax (square-bracket-contract stx)
  (syntax-parse stx
    [(_ name:id ()
        #:first-order first-order:expr
        #:late-neg-projection late-neg-projection:expr)
     #'(make-contract #:name 'name
                      #:first-order first-order
                      #:late-neg-projection late-neg-projection)]
    [(_ name:id ([opt-formal:id default:expr] ...+)
        #:first-order first-order:expr
        #:late-neg-projection late-neg-projection:expr)
     #'(let ([make-first-order (λ (opt-formal ...) first-order)]
             [make-projection  (λ (opt-formal ...) late-neg-projection)])
         (generic-contract
           'name
           (λ (opt-formal ...)
              (make-contract
                #:name (print-literally
                         (format-generic 'name (list opt-formal ...)))
                #:first-order (make-first-order opt-formal ...)
                #:late-neg-projection (make-projection opt-formal ...)))
           (make-first-order default ...)
           (make-projection default ...)))]))

; Creates a contract for a procedure that accepts optional, square
; bracket arguments. This is the function (non-macro) version of
; square-bracket-contract, and suitable for use from DSSL2, where it
; is exported as SquareBracketC.
;
; Example:
;
;     SquareBracketC("my_ctc", λ T, U: FunC[T, U, Pair?])
;
(define (build-square-bracket-contract name builder)
  (define opt-arity (procedure-arity builder))
  (unless (exact-integer? opt-arity)
    (dssl-error
      (string-append
        "SquareBracketC: builder procedure must have exact arity\n"
        "  builder:       %p\n"
        "  builder arity: %s")
      builder
      (format "~a" opt-arity)))
  (define (builder/name . ctcs)
    (rename-contract (apply builder ctcs)
                     (print-literally (format-generic name ctcs))))
  (define default-arguments (make-list opt-arity AnyC))
  (define default-contract (apply builder/name default-arguments))
  (generic-contract
    (print-literally name)
    builder/name
    (contract-first-order default-contract)
    (contract-late-neg-projection default-contract)))

; Creates a contract that takes optional, square bracket parameters.
; Unlike square-bracket-contract, this allows specifying different
; contracts for the case where optional parameters are given, and
; for the case where they aren't.
(define-syntax (special-square-bracket-contract stx)
  (syntax-parse stx
    [(_ name:id
        #:generic (opt-formal:id ...) generic:expr
        #:default default:expr)
     #'(let ([ctc default])
         (generic-contract
           'name
           (λ (opt-formal ...)
              (rename-contract
                generic
                (format-generic 'name (list opt-formal ...))))
           (contract-first-order ctc)
           (contract-late-neg-projection ctc)))]
    [(_ name:id
        #:generic (opt-formal:id ... . rest:id) generic:expr
        #:default default:expr)
     #'(let ([ctc default])
         (generic-contract
           'name
           (λ (opt-formal ... . rest)
              (rename-contract
                generic
                (format-generic 'name (list* opt-formal ... rest))))
           (contract-first-order ctc)
           (contract-late-neg-projection ctc)))]))

