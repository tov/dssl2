#lang racket/base

(provide exn:fail:dssl?
         get-srcloc/fn
         get-srcloc
         get-srclocs
         capture-context
         with-error-context
         source-context-srcloc
         current-dssl-error-format
         dssl-error
         runtime-error
         type-error
         assertion-error
         assertion-error/loc
         syntax-error)

(require
  (only-in racket/port with-output-to-string)
  syntax/parse/define
  (for-syntax racket/base
              (only-in racket/sequence
                       in-syntax)))

(define (get-srcloc/fn expr)
  (srcloc (syntax-source expr)
          (syntax-line expr)
          (syntax-column expr)
          (syntax-position expr)
          (syntax-span expr)))

(define-syntax-parser get-srcloc
  [(_ expr:expr)
   #`(srcloc
       '#,(syntax-source #'expr)
       '#,(syntax-line #'expr)
       '#,(syntax-column #'expr)
       '#,(syntax-position #'expr)
       '#,(syntax-span #'expr))])

(begin-for-syntax
  (define (get-srclocs/helper exprs)
    (with-syntax
      ([(expr ...) (for/list ([expr (in-syntax exprs)]
                              #:when (syntax-line expr))
                     expr)])
      #'(list (get-srcloc expr) ...)))

  (define (get-first-srcloc/helper exprs)
    (with-syntax
      ([expr
         (or (for/first ([expr (in-syntax exprs)]
                         #:when (syntax-line expr))
                        expr)
             (datum->syntax #f #f))])
      #'(get-srcloc expr))))

(define-syntax-parser get-srclocs
  [(_ . exprs)
   (get-srclocs/helper #'exprs)])

(define-syntax-parser get-first-srcloc
  [(_ . exprs)
   (get-first-srcloc/helper #'exprs)])

(struct source-context (srcloc continuation-marks))

(define current-error-context
  (make-parameter (source-context '() #f)))

(define-simple-macro (capture-context . code)
  (source-context (get-first-srcloc . code)
                  (current-continuation-marks)))

(define-simple-macro (default-context)
  (let ([context (current-error-context)])
    (if (source-context-continuation-marks context)
      context
      (capture-context))))

(define-syntax-parser with-error-context
  [(_ #:context context:expr body:expr ...+)
   #'(parameterize ([current-error-context context])
       body ...)]
  [(_ (stx:expr ...) body:expr ...+)
   #'(parameterize ([current-error-context (capture-context stx ...)])
       body ...)])

(struct exn:fail:dssl exn:fail (srcloc)
  #:transparent
  #:property prop:exn:srclocs
  (λ (a-struct)
     (list (exn:fail:dssl-srcloc a-struct))))

(define current-dssl-error-format
  (make-parameter
    (λ (fmt . args)
       (format "(apply dssl-format ~s ~s)" fmt args))))

(define dssl-format/error
  (case-lambda
    [()
     "error()"]
    [(arg0 . args)
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
         (apply dssl-format fmt arg0 args)])]))

(define (error #:context [context (default-context)]
               . args)
  (raise (exn:fail:dssl
           (apply dssl-format/error args)
           (source-context-continuation-marks context)
           (source-context-srcloc context))))

(define dssl-error error)

(define (runtime-error #:context [context (default-context)]
                       fmt . args)
  (apply error #:context context
         (string-append "Runtime error: " fmt)
         args))

(define (type-error #:context [context (default-context)]
                    who got expected)
  (dssl-error #:context context
              "%s: type error\n  got: %p\n  expected: %s"
              who got expected))

(define (assertion-error #:context [context (default-context)]
                         fmt . args)
  (apply dssl-error #:context context
         (string-append "Assertion failed: " fmt)
         args))

(define-simple-macro (assertion-error/loc loc:expr args:expr ...+)
  (assertion-error #:context (capture-context loc)
                   args ...))

(define (syntax-error stx fmt . args)
  (define message (apply format fmt args))
  (if (pair? stx)
    (raise-syntax-error #f message (car stx) #f (cdr stx))
    (raise-syntax-error #f message stx)))
