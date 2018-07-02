#lang racket/base

(provide object-base
         object-base?
         object-base-object-info
         object-base-contract-params
         object-base-reflect
         make-object-info
         object-info-name
         object-info-interfaces
         object-info-method-infos
         make-method-info
         method-info?
         method-info-name
         method-info-getter
         write-object
         get-method-info
         get-method-value/fun
         define-primitive-class
         (for-syntax make-unwrapped-class
                     make-unwrapped-class-table))

(require "names.rkt")
(require "errors.rkt")
(require syntax/parse/define)
(require racket/contract/region)
(require (for-syntax racket/base
                     (only-in racket/sequence in-syntax)
                     (only-in racket/syntax format-id syntax-local-eval)
                     "names.rkt"))

(define-struct method-info (name getter))
(define-struct object-info (name interfaces method-infos))

(define-struct object-base (object-info contract-params reflect)
               #:transparent
               #:methods gen:custom-write
               [(define write-proc
                  (λ (obj port mode)
                     (cond
                       [(eq? #t mode) (write-object obj port)]
                       [else          (fprintf port "~e" obj)])))])

(define (write-object obj port [recur (λ (v) (fprintf port "~e" v))])
  (fprintf port "#<object:~a"
           (object-info-name (object-base-object-info obj)))
  (for ([field-pair (in-vector ((object-base-reflect obj)))])
    (fprintf port " ~a=" (car field-pair))
    (recur (cdr field-pair)))
  (display ">" port))

(define-syntax (define-primitive-class stx)
  (syntax-parse stx
    [(_ class-name:id (prim-ctor:id field:id ...)
        ([method-name:id method-ctc:expr method-val:expr]
         ...))
     #`(begin
         (define-struct (internal-name object-base)
            [__class__ method-name ...])
         (define (#,(struct-predicate-name #'class-name) value)
           (internal-name? value))
         (define object-info
           (make-object-info 'class-name
                             (vector-immutable)
                             (vector-immutable
                               (make-method-info
                                 '__class__
                                 (struct-getter-name internal-name
                                                     __class__))
                               (make-method-info
                                 'method-name
                                 (struct-getter-name internal-name
                                                     method-name))
                               ...)))
         (define contract-parameters (vector-immutable))
         (define (prim-ctor field ...)
           (define/contract method-name method-ctc method-val)
           ...
           (make-internal-name
             object-info
             contract-parameters
             (λ () (vector-immutable (cons 'field field) ...))
             class-name
             method-name
             ...)))]))

(begin-for-syntax
  (define *method-table* (make-hasheq))
  (define *class-table*  (make-hasheq '((directory . ()))))

  (require (for-syntax racket/base))
  (require syntax/parse/define)

  (define (register-method method-table selector-symbol type-pred method-value)
    (define old-methods-for-selector
      (hash-ref method-table selector-symbol '()))
    (hash-set! method-table selector-symbol
               (cons (cons type-pred method-value) old-methods-for-selector)))

  (define-syntax (make-unwrapped-class stx)
    (syntax-parse stx
      [(_ class-name:id pred:id
          ([sel:id method:expr] ...))
       #`(cons
           (list #'pred #''__class__ #''sel ...)
           (λ (method-table)
              (register-method method-table '__class__ #'pred
                               #'(λ (self) class-name))
              (for ([sel-v    (in-syntax #'(sel ...))]
                    [method-v (in-syntax #'(method ...))])
                (define method-name
                  (string->symbol
                    (format "~a.~a" 'class-name (syntax-e sel-v))))
                (register-method
                  method-table
                  (syntax-e sel-v)
                  #'pred
                  #`(λ (self)
                       (procedure-rename
                         (λ args (apply #,method-v self args))
                         '#,method-name))))))]))

  (define-simple-macro
    (make-unwrapped-class-table class:id ...)
    (let ([dir-table    '()]
          [method-table (make-hasheq)])
      (set! dir-table (cons (car (syntax-local-eval #'class))
                            dir-table))
      ...
      ((cdr (syntax-local-eval #'class)) method-table)
      ...
      (values dir-table method-table))))

(define (get-method-info obj sym)
  (let/ec return
    (define info-vector (object-info-method-infos
                          (object-base-object-info obj)))
    (for ([info (in-vector info-vector)])
      (when (eq? sym (method-info-name info))
        (return info)))
    #false))

(define (get-method-value/fun obj sym)
  (cond
    [(and (object-base? obj)
          (get-method-info obj sym))
     =>
     (λ (method-info) ((method-info-getter method-info) obj))]
    [else #f]))
