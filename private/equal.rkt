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
    (cond
      ; This case covers equality for booleans, contracts, and
      ; procedures as well as physically equal pointers.
      [(eq? a b)              #true]
      [(number? a)            (and (number? b)
                                   (= a b))]
      [(string? a)            (and (string? b)
                                   (string=? a b))]
      [(vector? a)
       (and (vector? b)
            (= (vector-length a) (vector-length b))
            (or (seen? a b)
                (begin
                  (see! a b)
                  (for/and ([x (in-vector a)]
                            [y (in-vector b)])
                    (compare x y)))))]
      [(struct-base? a)
       (and (struct-base? b)
            (let ([info (struct-base-struct-info a)])
              (and (eq? info (struct-base-struct-info b))
                   (or (seen? a b)
                       (begin
                         (see! a b)
                         (for/and ([field-info
                                     (struct-info-field-infos info)])
                           (compare ((field-info-getter field-info) a)
                                    ((field-info-getter field-info) b))))))))]
      [else #false]))

  (compare a0 b0))
