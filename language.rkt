#lang racket

(provide #%app
         #%datum
         #%module-begin
         #%top
         #%top-interaction)
(provide + - * /
         make-vector
         vector
         begin
         cond
         else
         (rename-out
           [modulo              %]
           [expt                **]
           [equal?              ==]
           [eq?                 ===]
           [dssl-!=             !=]
           [dssl-!==            !==]
           [dssl-<              <]
           [dssl->              >]
           [dssl-<=             <=]
           [dssl->=             >=]
           [dssl-break          break]
           [dssl-continue       continue]
           [dssl-define         define]
           [dssl-defstruct      defstruct]
           [dssl-lambda         lambda]
           [dssl-return         return]
           [dssl-setf!          setf!]
           [dssl-struct-ref     struct-ref]
           [dssl-vector-ref     vector-ref]
           [dssl-while          while]))
(require racket/stxparam)

(define-syntax-parameter
  dssl-return
  (lambda (stx)
    (raise-syntax-error #f "use of return keyword not in a function" stx)))

(define-syntax dssl-define
  (syntax-rules ()
    [(_ (f formals ...) expr ...)
     (define f (dssl-lambda (formals ...) expr ...))]
    [(_ name expr)
     (define name expr)]))

(define-syntax-rule (dssl-lambda (param ...) expr ...)
  (lambda (param ...)
    (let/ec return-f
       (syntax-parameterize
         ([dssl-return (syntax-rules ()
                         [(_ ?result) (return-f ?result)])])
         (begin expr ...)))))

(define-syntax-parameter
  dssl-break
  (lambda (stx)
    (raise-syntax-error #f "use of break keyword not in a loop" stx)))

(define-syntax-parameter
  dssl-continue
  (lambda (stx)
    (raise-syntax-error #f "use of continue keyword not in a loop" stx)))

(define-syntax-rule (dssl-while test expr ...)
  (let/ec break-f
    (let loop ()
      (define (continue-f) (loop) (break-f))
      (syntax-parameterize
        ([dssl-break (syntax-rules () [(_) (break-f)])]
         [dssl-continue (syntax-rules () [(_) (continue-f)])])
        (when test
          expr ...
          (loop))))))

(define-syntax dssl-setf!
  (syntax-rules (dssl-vector-ref dssl-struct-ref)
    [(_ (dssl-vector-ref v i) rhs)
     (vector-set! v i rhs)]
    [(_ (dssl-struct-ref s f) rhs)
     (dssl-struct-set! s f rhs)]
    [(_ i rhs)
     (set! i rhs)]))

(define dssl-vector-ref vector-ref)

(define-struct struct [name fields]
               #:transparent)
(define-struct field [name value]
               #:transparent
               #:mutable)

(define (struct-assq name fields)
  (cond
    [(memf (λ (field) (eq? (field-name field) name)) fields)
     => first]
    [else #false]))

(define-syntax (dssl-defstruct stx)
  (syntax-case stx ()
    [(_ name (formal-field ...))
     #'(define-syntax-rule (name [field expr] (... ...))
         (dssl-make-struct 'name
                           '(formal-field ...)
                           (list (make-field 'field expr) (... ...))))]))


(define (dssl-make-struct name formals actuals)
  (define (get-value field)
    (or (struct-assq field actuals)
        (runtime-error
          "Error: constructor for ~a expects field ~a"
          name field)))
  (for-each (λ (actual)
              (unless (memq (field-name actual) formals)
                (runtime-error
                  "Error: constructor for ~a does not expect field ~a"
                  name (first actual))))
            actuals)
  (make-struct name (map get-value formals)))

(define-syntax-rule (dssl-struct-ref value field)
  (begin
    (when (not (struct? value))
      (runtime-error "Error: value ~a is not a struct" value))
    (cond
      [(struct-assq 'field (struct-fields value)) => field-value]
      [else
        (runtime-error "Error: struct ~a does not have field ~a"
                       value 'field)])))

(define-syntax-rule (dssl-struct-set! value field rhs)
  (begin
    (when (not (struct? value))
      (runtime-error "Error: value ~a is not a struct" value))
    (cond
      [(struct-assq 'field (struct-fields value))
       =>
       (λ (field) (set-field-value! field rhs))]
      [else
        (runtime-error "Error: struct ~a does not have field ~a"
                       value 'field)])))

(define (dssl-!= a b)
  (not (equal? a b)))

(define (dssl-!== a b)
  (not (eq? a b)))

(define-syntax make-comparison
  (syntax-rules ()
    [(_ name string-cmp number-cmp)
     (define (name a b)
       (cond
         [(and (string? a) (string? b))
          (string-cmp a b)]
         [(and (number? a) (number? b))
          (number-cmp a b)]
         [else
           (error "Comparators only apply to pairs of strings or numbers")]))]))

(make-comparison dssl-< string<? <)
(make-comparison dssl-> string>? >)
(make-comparison dssl-<= string<=? <=)
(make-comparison dssl->= string>=? >=)

(define (runtime-error fmt . args)
  (error (apply format (string-append "Runtime error: " fmt) args)))

