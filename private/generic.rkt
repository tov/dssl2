#lang racket/base

(provide generic-base?
         generic-base-name
         generic-base-instantiate
         generic-proc
         generic-proc?
         generic-proc-tag
         square-bracket-class-lambda
         square-bracket-proc
         define-square-bracket-proc
         square-bracket-contract)

(require "errors.rkt")
(require (for-syntax "errors.rkt"))
(require syntax/parse/define)
(require (only-in racket/contract/region
                  define/contract)
         (only-in racket/contract/base
                  contract
                  any/c
                  ->i
                  ->
                  contract-name)
         (only-in racket/contract/combinator
                  make-contract
                  prop:contract
                  build-contract-property))
(require (for-syntax racket/base
                     (only-in racket/syntax generate-temporary)))

; This is for generic things that can be instantiated
; with square brackets.
(struct generic-base (name instantiate))

; Generic procs are functions with optional square-bracket
; arguments. That is, they can be instantiated or directly
; applied.
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

(define (format-generic f xs)
  (define port (open-output-string))
  (fprintf port "~a[~a" f (contract-name (car xs)))
  (for ([xi (in-list (cdr xs))])
    (fprintf port ", ~a" (contract-name xi)))
  (fprintf port "]")
  (string->symbol (get-output-string port)))

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

(define-for-syntax (compose-neg-party who where)
  (format "~a at ~a" who (srcloc->string (get-srcloc/fn where))))

(define-syntax (define-square-bracket-proc stx)
  (syntax-parse stx
    [(_ ((name:id opt-formal:id ...) [formal:id formal-ctc:expr] ...)
        result-ctc:expr
        body:expr)
     #`(begin
         (define (instantiate neg-party opt-formal ...)
           (contract (-> formal-ctc ... result-ctc)
                     (procedure-rename
                       (λ (formal ...) body)
                       'name)
                     (format "function ~a at ~a" 'name
                             (srcloc->string (get-srcloc name)))
                     neg-party
                     'name (get-srcloc name)))
         #,(if (null? (syntax->list #'(opt-formal ...)))
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
                   (syntax-error stx "cannot assign to def'd function")]
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

