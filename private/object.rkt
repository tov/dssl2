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
         define-unwrapped-class
         (for-syntax make-unwrapped-class-table))

(require "names.rkt")
(require syntax/parse/define)
(require (only-in racket/contract/base any/c -> contract))
(require (for-syntax racket/base
                     (only-in racket/sequence in-syntax)
                     (only-in racket/syntax format-id syntax-local-eval)
                     "names.rkt"))

(define-struct method-info (name getter))
(define-struct object-info (name interfaces method-infos))

(define-struct object-base (object-info contract-params reflect)
               #:transparent)

(define (write-object obj port recur)
  (fprintf port "#<object:~a"
           (object-info-name (object-base-object-info obj)))
  (for ([field-pair (in-vector ((object-base-reflect obj)))])
    (fprintf port " ~a=" (car field-pair))
    (recur (cdr field-pair)))
  (display ">" port))

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
          ([sel:id body:expr] ...))
       #'(cons
           (list #'pred #''__class__ #''sel ...)
           (λ (method-table)
              (register-method method-table '__class__ #'pred
                               #'(λ (self) class-name))
              (for ([sel-v  (in-syntax #'(sel ...))]
                    [body-v (in-syntax #'(body ...))])
                (register-method
                  method-table
                  (syntax-e sel-v)
                  #'pred
                  body-v))))]))

  (define-simple-macro
    (make-unwrapped-class-table class:id ...)
    (let ([dir-table    '()]
          [method-table (make-hasheq)])
      (set! dir-table (cons (car (syntax-local-eval #'class))
                            dir-table))
      ...
      ((cdr (syntax-local-eval #'class)) method-table)
      ...
      (values dir-table method-table)))

  (define-syntax-class unwrapped-method
    (pattern [sel:id ctc:expr body:expr])
    (pattern [sel:id body:expr] #:with ctc #'any/c)))

(define-syntax (define-unwrapped-class stx)
  (syntax-parse stx
    [(_ name:id class-name:id pred:id
        (method:unwrapped-method ...))
     (with-syntax
       ([(visible-method-name ...)
         (map (λ (sel) (format-id #'class-name "~a#~a" #'class-name sel))
              (syntax->list #'(method.sel ...)))])
       #'(begin
           (#%provide visible-method-name ...)
           (define (visible-method-name self)
             (procedure-rename
               (contract method.ctc
                         (λ args (apply method.body self args))
                         'visible-method-name
                         "method caller"
                         'visible-method-name
                         #f)
               'visible-method-name))
           ...
           (define-for-syntax name
             (make-unwrapped-class class-name pred
               ([method.sel visible-method-name] ...)))))]))

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
