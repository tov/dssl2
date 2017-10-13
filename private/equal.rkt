#lang racket/base

(provide dssl-equal?)

(require "struct.rkt")

; A HashPair is (make-hash-pair fixnum? fixnum?)
(struct hash-pair [left right])

(define (make-key a b)
  (hash-pair (eq-hash-code a) (eq-hash-code b)))

(define (dssl-equal? a0 b0)
  (define table #false)

  (define (see! a b)
    (unless table (set! table (make-hash)))
    (printf "(see! ~e ~e)\n" a b)
    (hash-update! table
                  (make-key a b)
                  (λ (pairs) (cons (cons a b) pairs))
                  '()))

  (define (seen? a b)
    (printf "(seen ~e ~e)\n" a b)
    (and
      table
      (for/or ([pair (hash-ref table (make-key a b) '())])
        (and (eq? a (car pair))
             (eq? b (cdr pair))))))

  (define (compare a b)
    (let-syntax ([both (syntax-rules () [(_ p?) (and (p? a) (p? b))])])
      (cond
        ; This case covers equality for booleans, contracts, and
        ; procedures as well as physically equal pointers.
        [(eq? a b)              #true]
        [(both number?)         (= a b)]
        [(both string?)         (string=? a b)]
        [(seen? a b)            #true]
        [(and (both vector?) (= (vector-length a) (vector-length b)))
         (see! a b)
         (for/and ([x (in-vector a)]
                   [y (in-vector b)])
           (compare x y))]
        [(both struct-base?)    #false]
        [else #false])))

  (compare a0 b0))
