#lang racket/base

(require (for-syntax racket/base
                     syntax-parse)
         (only-in racket/set
                  set
                  set-empty?
                  set-subtract
                  set->list)
         (for-template "../provide.rkt"
                       "../run-time/struct.rkt"))

(define-syntax (define-expander stx)
  (syntax-parse stx
    [(_ (name:id pat ...) body:expr ...+)
     (with-syntax
       ([(arg ...)   (generate-temporaries
                       (syntax->list #'(pat ...)))])
       #'(define (name arg ...)
           (with-syntax ([pat arg] ...)
             body ...)))]))

(define-expander
  (expand-dssl-struct/early name internal-constructor predicate)
  #'(begin
      (dssl-provide predicate)
      (define-values
        (_struct-type internal-constructor predicate _getter _setter)
        (make-struct-type 'name struct:struct-base 0 0))))

(define (check-set-difference have want)
  (define difference (set-subtract have want))
  (if (set-empty? difference)
    #f
    (set->list difference)))

; ident ident ident [list-of ident] [list-of expr] -> syntax
(define-expander (expand-dssl-struct/late name special-name s:cons
                                          (field-name ...) (ctc-expr ...))
  (define name-symbol      (syntax-e #'name))
  (define name-length      (string-length (symbol->string name-symbol)))
  (define field-name-list  (syntax->list #'(field-name ...)))
  (with-syntax
    ([(setter-name ...) (for/list ([field-name (in-list field-name-list)])
                          (format-id field "field ‘~a’ assignment" field))]
     [(ctc ...)         (generate-temporaries field-name-list)]
     [duplicate-field-message
       (format "duplicate field in struct ~a" (syntax-e #'name))]
     [extra-field-message
       (format "struct ~a does not have field(s)" (syntax-e #'name))]
     [missing-field-message
       (format "struct ~a requires field(s)" (syntax-e #'name))] )
    (syntax-property
      #`(begin
          (define ctc (ensure-contract 'struct ctc-expr))
          ...
          (define the-struct-info
            (struct-info
              'name
              (vector-immutable 'field-name ...)
              (vector-immutable ctc ...)))
          (dssl-provide name)
          (define/contract (name field-name ...)
                           (-> ctc ... AnyC)
            (define the-data (make-hash))
            (hash-set! the-data 'field-name field-name)
            ...
            (s:cons the-struct-info the-data))
          (dssl-provide special-name)
          (define-for-syntax expected-fields (set 'field-name ...))
          (define-syntax (special-name stx)
            (syntax-parse stx
              [(ctor [field:id expr:expr] (... ...))
               #:do [(define actual-field-list
                       (syntax->list #'(field (... ...))))
                     (define actual-fields
                         (set 'field (... ...)))     
               #:fail-when (check-duplicate-identifier actual-field-list)
               duplicate-field-message
               #:fail-when (check-set-difference actual-fields expected-fields)
               extra-field-message
               #:fail-when (check-set-difference expected-fields actual-fields)
               missing-field-message
               (with-syntax
                 ([(temp (... ...)) (generate-temporaries actual-field-list)])
               #'(let ()
                   (define


                     #`(name #,@exprs))])))
      'sub-range-binders
      (vector (syntax-local-introduce (struct-special-name #'name))
              0 name-length 0.5 0.5
              (syntax-local-introduce #'name)
              0 name-length 0.5 0.5)))]))

