#lang racket/base

(provide exn:fail:dssl?
         exn:fail:dssl:assert?  exn:fail:dssl:assert-condition
         exn:fail:dssl:timeout? exn:fail:dssl:timeout-seconds
         get-srcloc/fn
         get-srcloc
         get-srclocs
         capture-context
         with-error-context
         source-context-srcloc
         current-error-context-srcloc
         current-dssl-error-format
         dssl-error
         raise-runtime-error
         raise-repr-error
         raise-assertion-error raise-assertion-error/loc
         raise-timeout-error
         syntax-error)

(require
  (only-in racket/port with-output-to-string)
  syntax/parse/define
  (for-syntax racket/base
              (only-in racket/sequence
                       in-syntax)))

(struct exn:fail:dssl exn:fail
  (srcloc)
  #:transparent
  #:property prop:exn:srclocs
  (λ (a-struct)
     (list (exn:fail:dssl-srcloc a-struct))))

(struct exn:fail:dssl:assert exn:fail:dssl
  (condition)
  #:transparent)

(struct exn:fail:dssl:timeout exn:fail:dssl
  (seconds)
  #:transparent)

(struct source-context (srcloc continuation-marks))

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

(define current-error-context
  (make-parameter (source-context #f #f)))

(define (current-error-context-srcloc)
  (source-context-srcloc (current-error-context)))

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

(define-simple-macro
  (dssl-raise make-exn:id message:expr context:expr etc:expr ...)
  (let ()
    (define message-v message)
    (define context-v context)
    (raise (make-exn
             message-v
             (source-context-continuation-marks context-v)
             (source-context-srcloc context-v)
             etc ...))))

(define (error #:context [context (default-context)]
               . args)
  (dssl-raise exn:fail:dssl
              (apply dssl-format/error args)
              context))

(define dssl-error error)

(define (raise-runtime-error
          #:context [context (default-context)]
          fmt . args)
  (dssl-raise exn:fail:dssl
              (string-append "Runtime error: "
                             (apply dssl-format/error fmt args))
              context))

(define (raise-repr-error
          #:context [context (default-context)]
          who got expected)
  (dssl-raise exn:fail:dssl
              (dssl-format/error
                "%s: type error\n  got: %p\n  expected: %s"
                who got expected)
              context))

(define (raise-assertion-error
          #:context [context (default-context)]
          . args)
  (define condition (apply dssl-format/error args))
  (dssl-raise exn:fail:dssl:assert
              (string-append "Assertion failed: " condition)
              context
              condition))

(define-simple-macro
  (raise-assertion-error/loc loc:expr args:expr ...+)
  (raise-assertion-error #:context (capture-context loc)
                         args ...))

(define (raise-timeout-error
          #:context [context (default-context)]
          timeout)
  (dssl-raise exn:fail:dssl:timeout
              (format "Assertion timeout: exceeded ~a second limit"
                      timeout)
              context
              timeout))

(define (syntax-error stx fmt . args)
  (define message (apply format fmt args))
  (if (pair? stx)
    (raise-syntax-error #f message (car stx) #f (cdr stx))
    (raise-syntax-error #f message stx)))
