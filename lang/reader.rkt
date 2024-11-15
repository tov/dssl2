#lang s-exp syntax/module-reader
dssl2/language
#:read my-read
#:read-syntax my-read-syntax
#:info info

(require "../private/parser.rkt"
         "../private/drracket/comment.rkt"
         "../private/drracket/indent.rkt"
         "../private/drracket/color-lexer.rkt")
(require (only-in racket/class send))

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (parse-dssl2 src in #f))

(define (info key defval default)
  (case key
    [(color-lexer)
     get-syntax-token]
    [(drracket:opt-out-toolbar-buttons)
     '(debug-tool drracket:syncheck macro-stepper)]
    [(drracket:indentation)
     find-current-indent]
    [(drracket:keystrokes)
     `(["["             ,(λ (text e) (send text insert #\[))]
       ["c:semicolon"   ,(λ (text e) (do-toggle-comment text))]
       ["d:semicolon"   ,(λ (text e) (do-toggle-comment text))]
       ["tab"           ,(λ (text e) (do-indent text))]
       ["s:tab"         ,(λ (text e) (do-dedent text))]
       ["backspace"     ,(λ (text e) (backspace-and-align text))]
       ["enter"         ,(λ (text e) (enter-and-indent text))])]
    [(drracket:submit-predicate)
     (λ (port space) #t)]
    [else
      (default key defval)]))
