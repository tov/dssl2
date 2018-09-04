#lang racket/base

(provide contract-params-match?
         define-dssl-interface
         (for-syntax lookup-interface))
(require (for-syntax racket/base
                     syntax/parse
                     (only-in racket/sequence in-syntax)
                     (only-in racket/string string-prefix?)
                     (only-in racket/syntax syntax-local-eval)
                     "errors.rkt"
                     "interface.rkt"
                     "names.rkt"
                     "util.rkt"))
(require (only-in racket/contract
                  blame-swap
                  contract
                  raise-blame-error)
         (only-in racket/vector
                  vector-memq)
         "contract.rkt"
         "errors.rkt"
         "generic.rkt"
         "object.rkt"
         "provide.rkt")

(define (contract-params-match? cs1 cs2)
  (for/and ([c1 (in-vector cs1)]
            [c2 (in-vector cs2)])
    (eq? c1 c2)))

(define-for-syntax (lookup-interface interface-name)
  (unless (identifier-binding interface-name 1)
    (syntax-error interface-name "undefined interface"))
  (syntax-local-eval interface-name))

(define (union-interfaces i1 i2)
  (define result (make-hasheq))
  (for ([(key value) (in-hash i1)])
    (hash-set! result key value))
  (for ([(key value) (in-hash i2)])
    (hash-set! result key
               (cond
                 [(hash-ref result key #f)
                  =>
                  (λ (value*) (λ (x) (value* (value x))))]
                 [else value])))
  (make-immutable-hasheq (hash->list result)))

(define (((apply-interface-contract interface-table
                                    contract-parameters
                                    interface-token
                                    first-order?)
          blame)
         value neg-party)
  (cond
    [(and (object-base? value)
          (assq interface-token
                (object-base-contract-paramses value)))
     =>
     (λ (pair)
        (if (contract-params-match?
              (cdr pair)
              contract-parameters)
          value
          (raise-blame-error
            blame #:missing-party neg-party value
            (string-append
              "cannot re-protect with with same interface"
              " (~a) and different contract parameters")
            'name)))]
    [(first-order? value)
     ((object-info-projector (object-base-info value))
      value
      (cons interface-token contract-parameters)
      (λ (sel method)
         (cond
           [(hash-ref interface-table sel #f)
            =>
            (λ (apply-contract) (apply-contract method))]
           [else
             (λ _
                (raise-blame-error
                  (blame-swap blame)
                  #:missing-party neg-party value
                  "interface ~a is protecting method ~a"
                  'name sel))])))]
    [(object-base? value)
     (raise-blame-error
       blame #:missing-party neg-party value
       "class ~a does not implement interface ~a"
       (object-info-name (object-base-info value))
       'name)]
    [else
      (raise-blame-error
        blame #:missing-party neg-party value
        "value ~e does not implement interface ~a"
        value 'name)]))

(define-syntax (define-dssl-interface stx)
  (syntax-parse stx
    [(_ name:id
        (cv:id ...)
        ((super-name:id super-param:expr ...) ...)
        ([method-name:id
           (method-cv:id ...)
           (method-param-contract:expr ...)
           method-result-contract:expr]
         ...))
     #:fail-when (check-duplicate-identifier
                   (syntax->list #'(method-name ...)))
                 "duplicate method name"
     ; Checking and metadata generation:
     (for ([method-name (in-syntax #'(method-name ...))])
       (unless (public-method-name? method-name)
         (syntax-error method-name "interface methods cannot be private")))
     (define super-infos
       (for/list ([super-name (in-syntax #'(super-name ...))])
         (lookup-interface super-name)))
     (define interface-token (gensym))
     (define interface-static-info
       (interface-info
         #'name
         interface-token
         super-infos
         (for/list ([name   (in-syntax #'(method-name ...))]
                    [m-cvs  (in-syntax #'((method-cv ...) ...))]
                    [params (in-syntax #'((method-param-contract ...) ...))])
           (with-syntax
             ([(param ...) params])
             (imethod-info
               name
               (syntax-length m-cvs)
               (syntax-length params))))))
     (check-interface-consistency interface-static-info)
     ; Code generation
     (define base-interface-table
       #'(make-immutable-hasheq
           (list (cons 'method-name
                       (let
                         ([the-contract
                            (square-bracket-proc-contract
                              method-name
                              [method-cv ...]
                              (ensure-contract 'def method-param-contract)
                              ...
                              (ensure-contract 'def method-result-contract))])
                         (λ (method)
                            (contract
                              the-contract
                              method
                              (format "method ~a of interface ~a"
                                      'method-name 'name)
                              "method caller"
                              #f
                              (get-srcloc method-name)))))
                 ...)))
     (define extended-interface-table
       (for/fold ([table base-interface-table])
         ([super-name   (in-syntax #'(super-name ...))]
          [super-params (in-syntax #'((super-param ...) ...))])
         #`(union-interfaces
             #,table
             (#,(interface-table-name super-name) #,@super-params))))
     #`(begin
         (define (make-interface-table cv ...)
           #,extended-interface-table)
         (define #,(interface-table-name #'name) make-interface-table)
         (dssl-provide (for-syntax name))
         (define-for-syntax name
           #,(reflect-interface interface-static-info))
         (define (first-order? obj)
           (and (object-base? obj)
                (vector-memq '#,interface-token
                             (object-info-interfaces
                               (object-base-info obj)))))
         (define (#,(struct-predicate-name #'name) obj)
           (and (first-order? obj) #t))
         (define (make-projection cv ...)
           (define interface-table (make-interface-table cv ...))
           (define contract-parameters (vector-immutable cv ...))
           (apply-interface-contract
             interface-table
             contract-parameters
             '#,interface-token
             first-order?))
         (dssl-provide #,(struct-predicate-name #'name))
         (dssl-provide #,(interface-contract-name #'name))
         (define #,(interface-contract-name #'name)
           (square-bracket-contract
             #,(interface-contract-name #'name)
             ([cv AnyC] ...)
             #:first-order first-order?
             #:late-neg-projection
             (make-projection cv ...))))]))

