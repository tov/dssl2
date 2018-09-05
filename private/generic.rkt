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
         special-square-bracket-contract)

(require "errors.rkt")
(require (for-syntax syntax/stx
                     "errors.rkt"))
(require syntax/parse/define)
(require (only-in racket/function
                  arity-includes?)
         (only-in racket/contract/region
                  define/contract)
         (only-in racket/contract/base
                  contract
                  any/c
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
                  contract-first-order))
(require (for-syntax racket/base
                     (only-in racket/syntax generate-temporary)))

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

; format-generic : symbol? [List-of contract?] -> symbol?
; Formats the application of a generic procedure to its optional (square
; bracket) arguments.
(define (format-generic f xs)
  (define contract-name (current-dssl-contract-name))
  (define port (open-output-string))
  (fprintf port "~a[~a" f (contract-name (car xs)))
  (for ([xi (in-list (cdr xs))])
    (fprintf port ", ~a" (contract-name xi)))
  (fprintf port "]")
  (string->symbol (get-output-string port)))

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
                    (λ (formal ...)
                       body ...)
                    (format-generic 'name (list opt-formal ...))))])
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
              (format-generic 'name (list opt-formal ...))))
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
     #`(begin
         #,(if no-opt-formals?
             #'(define instantiate-contract
                 (let ([the-contract (-> formal-ctc ... result-ctc)])
                   (λ () the-contract)))
             #'(define (instantiate-contract opt-formal ...)
                 (-> formal-ctc ... result-ctc)))
         (define (instantiate neg-party opt-formal ...)
           (contract (instantiate-contract opt-formal ...)
                     (procedure-rename
                       (λ (formal ...) body)
                       'name)
                     (format "def ~a at ~a" 'name
                             (srcloc->string (get-srcloc name)))
                     neg-party
                     'name (get-srcloc name)))
         #,(if no-opt-formals?
             #'(define real-definition instantiate)
             #'(define (real-definition neg-party)
                 (square-bracket-proc
                   name
                   #:generic (opt-formal ...)
                             (instantiate neg-party opt-formal ...)
                   #:default (let ([opt-formal any/c] ...)
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

; Creates a contract for a procedure that accepts optional, square
; bracket arguments.
;
; Example:
;
;     (square-bracket-proc-contract make_pair [T U] T U Pair?)
;
; Here, the optional parameters `T` and `U` give contracts for the
; required parameters, and default to `AnyC`.
(define-syntax (square-bracket-proc-contract stx)
  (syntax-parse stx
    [(_ name:id [] formal:expr ... result:expr)
     #'(-> formal ... result)]
    [(_ name:id [opt-formal:id ...+] formal:expr ... result:expr)
     #'(let ()
         (define (first-order? proc)
           (and (generic-proc? proc)
                (arity-includes?
                  (procedure-arity (generic-proc-apply proc))
                  (length (syntax->list #'(formal ...))))
                (arity-includes?
                  (procedure-arity (generic-base-instantiate proc))
                  (length (syntax->list #'(opt-formal ...))))))
         (define ((late-neg-proj blame) value missing-party)
           (cond
             [(first-order? value)
              (generic-proc
                (generic-base-name value)
                (λ (opt-formal ...)
                   (contract
                     (-> formal ... result)
                     ((generic-base-instantiate value) opt-formal ...)
                     (blame-positive blame)
                     missing-party
                     'name #f))
                (let ([opt-formal any/c] ...)
                  (contract
                    (-> formal ... result)
                    (generic-proc-apply value)
                    (blame-positive blame)
                    missing-party
                    'name #f))
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
                #:name (format-generic 'name (list opt-formal ...))
                #:first-order (make-first-order opt-formal ...)
                #:late-neg-projection (make-projection opt-formal ...)))
           (make-first-order default ...)
           (make-projection default ...)))]))

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

