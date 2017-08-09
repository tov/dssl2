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
     '(debug-tool macro-stepper drracket:syncheck)]
    [(drracket:indentation)
     find-current-indent]
    [(drracket:keystrokes)
     `(["["             ,handle-keystroke]
       ["tab"           ,handle-keystroke]
       ["s:tab"         ,handle-keystroke]
       ["enter"         ,handle-keystroke])]
    [(drracket:submit-predicate)
     (λ (port space) #t)]
    [else
      (default key defval)]))

(define (handle-keystroke text event)
  (case (send event get-key-code)
    [(#\[)      (send text insert #\[)]
    [(#\tab)    (go-to-indent text (send event get-shift-down))]
    [(#\return) (enter-and-indent text)]))
