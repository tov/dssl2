#lang s-exp syntax/module-reader
dssl2/language
#:read my-read
#:read-syntax my-read-syntax
#:info info

(require dssl2/parser)

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (parse-dssl2 src in #f))

(define (info key defval default)
  (case key
    [(color-lexer)
     (dynamic-require 'dssl2/syntax-color 'get-syntax-token)]
    [(drracket:default-extension) "ds2"]
    [else
      (default key defval)]))

