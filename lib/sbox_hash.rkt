#lang racket/base

(require dssl2/private/class-system
         (only-in dssl2/private/prims
                  AnyC
                  str))

(provide HASHER
         SboxHash64
         SboxHash64?
         SboxHash
         SboxHash?
         make_sbox_hash)


;;;
;;; Constants
;;;

(define N-CHARS       256)
(define CHAR-MASK     (sub1 N-CHARS))
(define WORD-MASK     (sub1 (arithmetic-shift 1 64)))
(define UINT16-LIMIT  (arithmetic-shift 1 16))


;;;
;;; Random generation of 64-bit unsigned integers
;;; (and vectors thereof)
;;;

(define (random-uint16)
  (random UINT16-LIMIT))

(define (random-uint64)
  (bitwise-ior
   (random-uint16)
   (arithmetic-shift (random-uint16) 16)
   (arithmetic-shift (random-uint16) 32)
   (arithmetic-shift (random-uint16) 48)))

(define (random-sbox)
  (build-vector N-CHARS (λ (_) (random-uint64))))


;;;
;;; The hash function
;;;

(define (do-hash key start sbox)
  (for/fold ([hash-code start])
            ([c (in-string (key->str key))])
    (mix (bitwise-xor hash-code (substitute sbox c)))))

(define (key->str key)
  (if (string? key)
      key
      (str key)))

(define (substitute sbox c)
  (vector-ref sbox
              (bitwise-and
               CHAR-MASK
               (char->integer c))))

(define (mix i)
  (bitwise-and WORD-MASK (* 3 i)))

;;;
;;; HASHER Interface
;;;

(define-dssl-interface HASHER () ()
  ([hash () (AnyC) AnyC]))


;;;
;;; SboxHash64 class
;;;

(define-dssl-class SboxHash64 () (HASHER)
  ([start AnyC] [sbox AnyC])
  ([__init__ () self () AnyC
     (begin
       (dssl-self start (random-uint64))
       (dssl-self sbox (random-sbox)))]
   [hash () self ([key AnyC]) AnyC
     (do-hash key (dssl-self start) (dssl-self sbox))]
   [__eq__ () self ([other AnyC]) AnyC
     (eq? self other)]
   [__print__ () self ([print AnyC]) AnyC
     (print "#<object:SboxHash _start=%p _sbox=...>"
            (dssl-self start))]))


;;;
;;; Deprecated APIs
;;;

(define (SboxHash)
  (SboxHash64))

(define (SboxHash? o)
  (SboxHash64? o))

(define (make_sbox_hash)
  (define start (random-uint64))
  (define sbox  (random-sbox))
  (define (hash key)
    (do-hash key start sbox))
  hash)
