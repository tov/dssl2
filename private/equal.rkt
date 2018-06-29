#lang racket/base

(provide dssl-equal?)

(require "struct.rkt")
(require "object.rkt")
(require syntax/parse/define)

; A Chain is one of:
;  - [Box Natural]
;  - [Box Chain]

; union-find!? : [EqHashTbl Any Chain] Any Any -> Boolean
; Associates `a` and `b` in the hash table, and returns whether they
; were associated already.
(define (union-find!? h x y)
  (define (find b)
    (define n (unbox b))
    (if (box? n)
        (let loop ([b b] [n n])
          (define nn (unbox n))
          (if (box? nn)
              (begin
                (set-box! b nn)
                (loop n nn))
              n))
        b))
  (define bx (hash-ref h x #f))
  (define by (hash-ref h y #f))
  (cond
    [(and bx by)
     (define rx (find bx))
     (define ry (find by))
     (cond
       [(eq? rx ry) #t]
       [else
         (define nx (unbox bx))
         (define ny (unbox by))
         (cond
           [(> nx ny)
            (set-box! ry rx)
            (set-box! rx (+ nx ny))]
           [else
            (set-box! rx ry)
            (set-box! ry (+ nx ny))])
         #f])]
    [bx
     (define rx (find bx))
     (hash-set! h y rx)
     #f]
    [by
     (define ry (find by))
     (hash-set! h x ry)
     #f]
    [else
     (define b (box 1))
     (hash-set! h x b)
     (hash-set! h y b)
     #f]))

(define current-equal-table (make-parameter #f))

(define-simple-macro (with-equal-table body:expr ...)
  (parameterize ([current-equal-table (or (current-equal-table)
                                          (make-hasheq))])
    body ...))

(define (seen!? a b)
  (union-find!? (current-equal-table) a b))

(define (dssl-equal? a0 b0)
  (with-equal-table
    (let compare ([a a0] [b b0])
      (cond
        ; We try number? before eq?, to get correct treatement of nan.
        [(number? a)            (and (number? b) (= a b))]
        ; This case covers equality for booleans, contracts, and
        ; procedures as well as physically equal pointers.
        [(eq? a b)              #true]
        [(string? a)            (and (string? b) (string=? a b))]
        [(vector? a)
         (or (seen!? a b)
             (and (vector? b)
                  (= (vector-length a) (vector-length b))
                  (for/and ([x (in-vector a)]
                            [y (in-vector b)])
                    (compare x y))))]
        [(struct-base? a)
         (or (seen!? a b)
             (and (struct-base? b)
                  (struct-equal? a b compare)))]
        [(object-base? a)
         (or (seen!? a b)
             (and (object-base? b)
                  (object-equal? a b compare)))]
        [else #false]))))

(define (struct-equal? a b compare)
  (define info (struct-base-struct-info a))
  (and (eq? info (struct-base-struct-info b))
       (for/and ([field-info (struct-info-field-infos info)])
         (define getter (field-info-getter field-info))
         (compare (getter a) (getter b)))))

(define (object-equal? a b compare)
  (and (same-class? a b)
       (cond
         [(get-method-value a '__eq__) => (λ (eq) (eq b))]
         [else
           (for/and ([a-pair (in-vector ((object-base-reflect a)))]
                     [b-pair (in-vector ((object-base-reflect b)))])
             (compare (cdr a-pair) (cdr b-pair)))])))

(define (same-class? a b)
  (define class-a (get-method-value a '__class__))
  (and class-a
       (eq? class-a (get-method-value b '__class__))))
