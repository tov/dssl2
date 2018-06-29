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
         get-method-vector
         get-method-info
         get-method-value
         define-primitive-class)

(require "names.rkt")
(require syntax/parse/define)
(require racket/contract/region)
(require (for-syntax racket/base
                     "names.rkt"))

(define-struct method-info (name getter))
(define-struct object-info (name interfaces method-infos))

(define-struct object-base (object-info contract-params reflect)
               #:transparent
               #:methods gen:custom-write
               [(define write-proc
                  (λ (obj port mode)
                     (if (eq? #t mode)
                       (write-object obj port)
                       (fprintf port "~e" obj))))])

(define (write-object obj port [recur (λ (v) (fprintf port "~e" v))])
  (fprintf port "#<object:~a"
           (object-info-name (object-base-object-info obj)))
  (for ([field-pair (in-vector ((object-base-reflect obj)))])
    (fprintf port " ~a=" (car field-pair))
    (recur (cdr field-pair)))
  (display ">" port))

(define (get-method-vector obj)
  (for/vector ([method-info (object-info-method-infos
                              (object-base-object-info obj))])
    (symbol->string (method-info-name method-info))))

(define (get-method-info obj sym)
  (let/ec return
    (define info-vector (object-info-method-infos
                          (object-base-object-info obj)))
    (for ([info (in-vector info-vector)])
      (when (eq? sym (method-info-name info))
        (return info)))
    #false))

(define (get-method-value obj sym)
  (cond
    [(get-method-info obj sym)
     =>
     (λ (method-info) ((method-info-getter method-info) obj))]
    [else #f]))

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
