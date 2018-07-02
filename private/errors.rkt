#lang racket/base

(provide exn:fail:dssl?
         get-srcloc
         get-srclocs
         current-dssl-error-format
         dssl-error
         runtime-error
         type-error
         assertion-error
         syntax-error)

(require
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

(define current-dssl-error-format
  (make-parameter
    (λ (fmt . args)
       (format "(apply dssl-format ~s ~s)" fmt args))))

(define (error #:srclocs [srclocs '()] fmt . args)
  (raise (make-exn:fail:dssl
           (apply (current-dssl-error-format) fmt args)
           (current-continuation-marks)
           srclocs)))

(define dssl-error error)

(define (runtime-error #:srclocs [srclocs '()] fmt . args)
  (apply dssl-error #:srclocs srclocs
         (string-append "Runtime error: " fmt)
         args))

(define (type-error #:srclocs [srclocs '()] who got expected)
  (dssl-error #:srclocs srclocs
              "%s: type error\n  got: %p\n  expected: %s"
              who got expected))

(define (assertion-error #:srclocs [srclocs '()] who fmt . args)
  (apply dssl-error #:srclocs srclocs
         (format "Assertion failed: ~a;\n ~a" who fmt)
         args))

(define (syntax-error stx fmt . args)
  (raise-syntax-error #f (apply format fmt args) stx))
