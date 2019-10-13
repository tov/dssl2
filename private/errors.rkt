#lang racket/base

(provide exn:fail:dssl?
         get-srcloc/fn
         get-srcloc
         get-srclocs
         current-dssl-error-format
         dssl-error
         runtime-error
         type-error
         assertion-error
         syntax-error)

(require
  (only-in racket/port with-output-to-string)
  (for-syntax racket/base
              syntax/parse))

(define (get-srcloc/fn expr)
  (srcloc (syntax-source expr)
          (syntax-line expr)
          (syntax-column expr)
          (syntax-position expr)
          (syntax-span expr)))

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

(define (dssl-format/error arg0 . args)
  (define dssl-format (current-dssl-error-format))
  (cond
    [(string? arg0)
     (apply dssl-format arg0 args)]
    [else
      (define fmt
        (with-output-to-string
          (λ ()
             (display "error(%p")
             (for ([_ (in-list args)]) (display ", %p"))
             (display ")"))))
      (apply dssl-format fmt arg0 args)]))

(define (error #:srclocs [srclocs '()] . args)
  (raise (make-exn:fail:dssl
           (if (pair? args)
             (apply dssl-format/error args)
             "error()")
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

(define (assertion-error #:srclocs [srclocs '()] fmt . args)
  (apply dssl-error #:srclocs srclocs
         (string-append "Assertion failed: " fmt)
         args))

(define (syntax-error stx fmt . args)
  (define message (apply format fmt args))
  (if (pair? stx)
    (raise-syntax-error #f message (car stx) #f (cdr stx))
    (raise-syntax-error #f message stx)))
