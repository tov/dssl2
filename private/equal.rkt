#lang racket/base

(provide dssl-equal?)

(require "struct.rkt")

; A HashPair is (make-hash-pair fixnum? fixnum?)
(struct hash-pair [left right] #:transparent)

(define (make-key a b)
  (hash-pair (eq-hash-code a) (eq-hash-code b)))

(define (dssl-equal? a0 b0)
  (define table #false)

  (define (see! a b)
    (unless table (set! table (make-hash)))
    (hash-update! table
                  (make-key a b)
                  (λ (pairs) (cons (cons a b) pairs))
                  '()))

  (define (seen? a b)
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
        [(and (both vector?) (= (vector-length a) (vector-length b)))
         (cond
           [(seen? a b) #true]
           [else
             (see! a b)
             (for/and ([x (in-vector a)]
                       [y (in-vector b)])
               (compare x y))])]
        [(and (both struct-base?)
              (eq? (struct-base-struct-info a) (struct-base-struct-info b)))
         (cond
           [(seen? a b) #true]
           [else
             (see! a b)
             (for/and ([field-info (struct-info-field-infos
                                     (struct-base-struct-info a))])
               (compare ((field-info-getter field-info) a)
                        ((field-info-getter field-info) b)))])]
        [else #false])))

  (compare a0 b0))
