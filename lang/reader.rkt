#lang s-exp syntax/module-reader
dssl2/language
#:read my-read
#:read-syntax my-read-syntax

(require dssl2/parser)

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (parse-dssl2 src in #f))
