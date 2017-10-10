#lang racket

(provide exn:fail:dssl?
         get-srclocs
         dssl-error
         runtime-error
         type-error
         assertion-error)

(require (for-syntax syntax/parse))

(define-syntax (get-srclocs stx)
  (syntax-parse stx
    [(_)
     #`'()]
    [(_ expr:expr rest:expr ...)
     #`(cons (srcloc
               '#,(syntax-source #'expr)
               '#,(syntax-line #'expr)
               '#,(syntax-column #'expr)
               '#,(syntax-position #'expr)
               '#,(syntax-span #'expr))
             (get-srclocs rest ...))]))

(define-struct (exn:fail:dssl exn:fail) (srclocs)
  #:transparent
  #:property prop:exn:srclocs
  (λ (a-struct) (exn:fail:dssl-srclocs a-struct)))

(define (dssl-error #:srclocs [srclocs '()] fmt . args)
  (raise (make-exn:fail:dssl
           (apply format fmt args)
           (current-continuation-marks)
           srclocs)))

(define (runtime-error #:srclocs [srclocs '()] fmt . args)
  (apply dssl-error #:srclocs srclocs
         (string-append "Runtime error: " fmt)
         args))

(define (type-error #:srclocs [srclocs '()] who got expected)
  (dssl-error #:srclocs srclocs
              "~a: type error\n  got: ~e\n  expected: ~e"
              who got expected))

(define (assertion-error #:srclocs [srclocs '()] who fmt . args)
  (apply dssl-error #:srclocs srclocs
         (string-append "Assertion failed: " (~a who) ";\n " fmt)
         args))

