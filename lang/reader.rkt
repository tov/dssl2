#lang s-exp syntax/module-reader
dssl2/language
#:read my-read
#:read-syntax my-read-syntax
#:info info

(require dssl2/private/parser
         dssl2/private/indent
         dssl2/private/syntax-color)
(require (only-in racket send))

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (parse-dssl2 src in #f))

(define (info key defval default)
  (case key
    [(color-lexer)
     get-syntax-token]
    [(drracket:opt-out-toolbar-buttons)
     '(debug-tool drracket:syncheck)] ; macro-stepper?
    [(drracket:indentation)
     find-current-indent]
    [(drracket:keystrokes)
     `(["["             ,(λ (text e) (send text insert #\[))]
       ["tab"           ,(λ (text e) (go-to-indent text #f))]
       ["s:tab"         ,(λ (text e) (go-to-indent text #t))]
       ["enter"         ,(λ (text e) (enter-and-indent text))])]
    [(drracket:submit-predicate)
     (λ (port space) #t)]
    [else
      (default key defval)]))
