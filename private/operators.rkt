#lang racket/base

(provide #; comparisons
         ==
         !=
         <
         >
         <=
         >=
         is
         |is not|
         ;; arithmetic
         +
         -
         *
         /
         //
         %
         **
         ;; bitwise logic
         &
         \|
         ^
         <<
         >>
         ~
         ;; Boolean logic
         not
         and    ; (syntax)
         or)    ; (syntax)

(provide (rename-out [!=        ≠]
                     [<=        ≤]
                     [>=        ≥]))

(require (prefix-in p: "prims.rkt")
         "errors.rkt"
         "singletons.rkt")
(require (prefix-in racket: racket/base))
(require syntax/parse/define)
(require (for-syntax racket/base (only-in racket/syntax format-id)))

(define (not v)
  (falsy? v))

(define-syntax-parser and
  [(_ e:expr es:expr ...+)
   #'(let ([v e])
       (if (falsy? v)
         v
         (and es ...)))]
  [(_ e:expr)   #'e]
  [(_)          #'#true])

(define-syntax-parser or
  [(_ e:expr es:expr ...+)
   #'(let ([v e])
       (if (truthy? v)
         v
         (or es ...)))]
  [(_ e:expr)   #'e]
  [(_)          #'#false])

(define-syntax (define-generic-binop stx)
  (syntax-parse stx #:literals (quote)
    [(_ name:id (quote lop:id) (quote rop:id) msg:str)
     #'(define (name a b)
         (cond
           [(p:dssl-send a 'lop b #:and-then box #:or-else #f)
            => unbox]
           [(p:dssl-send b 'rop a #:and-then box #:or-else #f)
            => unbox]
           [else
             (raise-repr-error
               'name (vector a b)
               (format "~a or object responding to ~a method"
                       msg 'lop))]))]))

(define-syntax (define-generic-unop stx)
  (syntax-parse stx #:literals (quote)
    [(_ name:id (quote op:id) msg:str)
     #'(define (name a)
         (cond
           [(p:dssl-send a 'op #:and-then box #:or-else #f)
            => unbox]
           [else
             (raise-repr-error
               'name a
               (format "~a or object responding to ~a method"
                       msg 'op))]))]))

(define-syntax (define-generic-un/binop stx)
  (syntax-parse stx #:literals (quote)
    [(_ name:id [(quote op:id) msg1:str]
                [(quote lop:id) (quote rop:id) msg2:str])
     (with-syntax
       ([unop  (format-id #f "unary ~a" #'name)]
        [binop (format-id #f "binary ~a" #'name)])
       #'(begin
           (define-generic-unop unop 'op msg1)
           (define-generic-binop binop 'lop 'rop msg2)
           (define name
             (case-lambda
               [(a) (unop a)]
               [(a b) (binop a b)]))))]))

(define-generic-binop // '__idiv__ '__ridiv__ "ints")
(define-generic-binop %  '__mod__ '__rmod__ "ints")
(define-generic-binop ** '__pow__ '__rpow__ "nums")
(define-generic-binop &  '__and__ '__rand__ "ints")
(define-generic-binop \| '__or__  '__ror__  "ints")
(define-generic-binop ^  '__xor__ '__rxor__ "ints")
(define-generic-binop << '__lshift__ '__rlshift__ "ints")
(define-generic-binop >> '__rshift__ '__rrshift__ "ints")

(define-generic-unop  ~  '__invert__ "int or bool")

(define-generic-un/binop + ['__pos__ "num"]
                           ['__add__ '__radd__ "nums"])
(define-generic-un/binop - ['__neg__ "num"]
                           ['__sub__ '__rsub__ "nums"])
(define-generic-binop *  '__mul__ '__rmul__ "nums")
(define-generic-binop /  '__div__ '__rdiv__ "nums")

(define (== a b)
  (p:dssl-equal? a b))

(define (!= a b)
  (racket:not (== a b)))

(define (is a b)
  (eq? a b))

(define (|is not| a b)
  (racket:not (is a b)))

(define (< a b)
  (truthy-cond
    [(p:cmp a b)
     =>
     (λ (order) (racket:< order 0))]
    [else (raise-repr-error '< (vector a b) "comparable objects")]))

(define (<= a b)
  (truthy-cond
    [(p:cmp a b)
     =>
     (λ (order) (racket:<= order 0))]
    [else (raise-repr-error '<= (vector a b) "comparable objects")]))

(define (> a b)
  (truthy-cond
   [(p:cmp b a)
    =>
    (λ (order) (racket:< order 0))]
   [else (raise-repr-error '> (vector a b) "comparable objects")]))

(define (>= a b)
  (truthy-cond
   [(p:cmp b a)
    =>
    (λ (order) (racket:<= order 0))]
   [else (raise-repr-error '>= (vector a b) "comparable objects")]))
