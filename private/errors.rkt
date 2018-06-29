#lang racket/base

(provide exn:fail:dssl?
         get-srcloc
         get-srclocs
         dssl-error
         runtime-error
         type-error
         assertion-error
         syntax-error)

(require
  (only-in racket/format ~a)
  (for-syntax racket/base
              syntax/parse))

(define-syntax (get-srcloc stx)
  (syntax-parse stx
    [(_ expr:expr)
     #`(srcloc
         '#,(syntax-source #'expr)
         '#,(syntax-line #'expr)
         '#,(syntax-column #'expr)
         '#,(syntax-position #'expr)
         '#,(syntax-span #'expr))]))

(define-syntax (get-srclocs stx)
  (syntax-parse stx
    [(_ expr:expr ...)
     #'(list (get-srcloc expr) ...)]))

(define-struct (exn:fail:dssl exn:fail) (srclocs)
  #:transparent
  #:property prop:exn:srclocs
  (λ (a-struct) (exn:fail:dssl-srclocs a-struct)))

(define (error #:srclocs [srclocs '()] fmt . args)
  (raise (make-exn:fail:dssl
           (apply format fmt args)
           (current-continuation-marks)
           srclocs)))

(define dssl-error error)

(define (runtime-error #:srclocs [srclocs '()] fmt . args)
  (apply dssl-error #:srclocs srclocs
         (string-append "Runtime error: " fmt)
         args))

(define (type-error #:srclocs [srclocs '()] who got expected)
  (dssl-error #:srclocs srclocs
              "~a: type error\n  got: ~e\n  expected: ~a"
              who got expected))

(define (assertion-error #:srclocs [srclocs '()] who fmt . args)
  (apply dssl-error #:srclocs srclocs
         (string-append "Assertion failed: " (~a who) ";\n " fmt)
         args))

(define (syntax-error stx fmt . args)
  (raise-syntax-error #f (apply format fmt args) stx))
