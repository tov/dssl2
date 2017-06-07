#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-empty-tokens dssl2-empty-tokens
  (EOF
   INDENT
   DEDENT
   NEWLINE
   LPAREN
   RPAREN
   LBRACK
   RBRACK
   LBRACE
   RBRACE
   COMMA
   PERIOD
   COLON
   SEMICOLON
   EQUALS
   PLUS         ; two different precedences
   MINUS        ; two different precedences
   IF
   ELSE
   WHILE
   RETURN
   LAMBDA
   DEF
   DEFSTRUCT))

(define-tokens dssl2-tokens
  (IDENT
   OP0  ; ||
   OP1  ; &&
   OP2  ; == <= >= != < > === !==
   OP3  ; |
   OP4  ; ^
   OP5  ; &
   OP6  ; << >>
   OP7  ; (+ -)
   OP8  ; * / %
   OP9  ; unary ~ ! (+ -)
   OP10 ; **
   LITERAL))

(define-lex-abbrevs
  [natural     (:+ numeric)]
  [integer     (:: (:? #\-) natural)]
  [exponent    (:: (:or #\e #\E) integer)]
  [pointfloat  (:or (:: integer #\. (:* numeric))
                    (:: (:? #\-) (:* numeric) #\. natural))]
  [float       (:or (:: pointfloat (:? exponent))
                    (:: integer exponent))]
  [hexdigit    (:or numeric (char-range #\a #\f) (char-range #\A #\F))]
  [hexadecimal (:: (:? #\-) (:or "0x" "0X") (:+ hexdigit))]
  [octdigit    (char-range #\0 #\7)]
  [octal       (:: (:? #\-) (:or "0o" "0O") (:+ octdigit))]
  [binary      (:: (:? #\-) (:or "0b" "0B") (:+ (:or #\0 #\1)))])

; new-dssl2-lexer : input-port? -> [ -> Token]
(define (new-dssl2-lexer port)
  (define stack '(0))
  (define queue '())

  (define (push item)
    (set! stack (cons item stack)))

  (define (pop)
    (define result (first stack))
    (set! stack (rest stack))
    result)

  (define (enq . items)
    (set! queue (append queue items)))

  (define (deq)
    (define result (first queue))
    (set! queue (rest queue))
    result)

  (define (closing closer token)
    (cond
      [(number? (first stack))
       (pop)
       (enq (token-DEDENT))
       (closing closer token)]
      [(eq? (first stack) closer)
       (pop)
       (enq token)
       (deq)]
      [else
        (error "Lexical error"
               "Expected" (first stack)
               "got" closer)]))

  (define (top-number)
    (let loop [(stack stack)]
      (if (number? (first stack))
        (first stack)
        (loop (rest stack)))))

  (define (handle-indent indent)
    (cond
      [(number? (first stack))
       (let loop []
         (cond
           [(> indent (first stack))
            (push indent)
            (enq (token-INDENT))]
           [(= indent (first stack))
            (enq (token-NEWLINE))]
           [else
             (enq (token-DEDENT))
             (pop)
             (loop)]))
       (deq)]
      [else (the-lexer port)]))

  (define the-lexer
    (lexer
      [(eof)                    (token-EOF)]
      [#\(
       (begin
         (push #\))
         (token-LPAREN))]
      [#\[
       (begin
         (push #\])
         (token-LBRACK))]
      [#\{
       (begin
         (push #\})
         (token-LBRACE))]
      [#\)                      (closing #\) (token-RPAREN))]
      [#\]                      (closing #\] (token-RBRACK))]
      [#\}                      (closing #\} (token-RBRACE))]
      [#\,                      (token-COMMA)]
      [#\.                      (token-PERIOD)]
      [#\:                      (token-COLON)]
      [#\;                      (token-SEMICOLON)]
      [#\=                      (token-EQUALS)]
      [#\+                      (token-PLUS)]
      [#\-                      (token-MINUS)]
      ["if"                     (token-IF)]
      ["else"                   (token-ELSE)]
      ["while"                  (token-WHILE)]
      ["return"                 (token-RETURN)]
      ["lambda"                 (token-LAMBDA)]
      [#\λ                      (token-LAMBDA)]
      ["def"                    (token-DEF)]
      ["defstruct"              (token-DEFSTRUCT)]
      [(:: alphabetic (:* (:or alphabetic numeric #\_)) (:? (:or #\! #\?)))
       (token-IDENT lexeme)]
      ["||"                     (token-OP0 lexeme)]
      ["&&"                     (token-OP1 lexeme)]
      [(:or "==" #\< #\> "<=" ">=" "!=" "===" "!==")
       (token-OP2 lexeme)]
      [#\|                      (token-OP3 lexeme)]
      [#\^                      (token-OP4 lexeme)]
      [#\&                      (token-OP5 lexeme)]
      [(:or "<<" ">>")          (token-OP6 lexeme)]
      [(:or #\* #\/ #\%)        (token-OP8 lexeme)]
      [(:or #\! #\~)            (token-OP9 lexeme)]
      ["**"                     (token-OP10 lexeme)]
      [(:: #\"
           (:*
             (:or
               (:- any-char (:or #\\ #\" #\newline))
               (:: #\\ any-char)))
           #\")
       (token-LITERAL
         (interpret-string (remove-first-and-last lexeme)))]
      [(:: #\'
           (:*
             (:or
               (:- any-char (:or #\\ #\' #\newline))
               (:: #\\ any-char)))
           #\')
       (token-LITERAL
         (interpret-string (remove-first-and-last lexeme)))]
      [integer                  (token-LITERAL (read-string lexeme))]
      [float                    (token-LITERAL (read-string lexeme))]
      [hexadecimal              (token-LITERAL (interpret-non-dec lexeme))]
      [octal                    (token-LITERAL (interpret-non-dec lexeme))]
      [binary                   (token-LITERAL (interpret-non-dec lexeme))]
      ["-inf.0"                 (token-LITERAL -inf.0)]
      ["+inf.0"                 (token-LITERAL +inf.0)]
      ["-nan.0"                 (token-LITERAL -nan.0)]
      ["+nan.0"                 (token-LITERAL +nan.0)]
      [(:or #\space #\tab)
       (the-lexer port)]
      [(:: #\# (:* (:- any-char #\newline)))
       (the-lexer port)]
      [(:+ (:: #\newline (:* #\space)))
       (handle-indent (last-spaces lexeme))]))
  (λ ()
     (cond
       [(cons? queue)   (deq)]
       [else            (the-lexer port)])))

; string? -> string?
; Removes the first and last characters of a string.
(define (remove-first-and-last str)
  (substring str 1 (sub1 (string-length str))))

; string? -> string?
; Interprets the escapes in a string literal.
(define (interpret-string lit)
  (define (loop chars)
    (cond
      [(empty? chars)    '()]
      [(eq? #\\ (first chars))
       (define (the-rest) (loop (rest (rest chars))))
       (case (second chars)
         [(#\a)         (cons #\001 (the-rest))]
         [(#\b)         (cons #\backspace (the-rest))]
         [(#\f)         (cons #\006 (the-rest))]
         [(#\n)         (cons #\newline (the-rest))]
         [(#\r)         (cons #\return (the-rest))]
         [(#\t)         (cons #\tab (the-rest))]
         [(#\v)         (cons #\vtab (the-rest))]
         [(#\newline)   (the-rest)]
         [(#\x)         (cons (hex->char (third chars) (fourth chars))
                              (loop (list-tail chars 4)))]
         [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
                        (cons
                          (oct->char (second chars)
                                     (third chars)
                                     (fourth chars))
                          (loop (list-tail chars 4)))]
         [else          (cons (second chars) (the-rest))])]
      [else
        (cons (first chars)
              (loop (rest chars)))]))
  (list->string (loop (string->list lit))))

; char? char? -> char?
; Converts two hex digits to the represented character.
(define (hex->char digit1 digit2)
  (integer->char
    (read (open-input-string
            (list->string (list #\# #\x digit1 digit2))))))

; char? char? char? -> char?
; Converts three octal digits to the represented character.
(define (oct->char digit1 digit2 digit3)
  (integer->char
    (read-string (list->string (list #\# #\o digit1 digit2 digit3)))))

; string? -> number?
; Interprets a Python non-decimal number.
(define (interpret-non-dec str)
  (read-string (string-append "#" (substring str 1))))

; string? -> number?
; Counts the spaces on the last line of the string.
(define (last-spaces str)
  (string-length (regexp-replace #rx".*\n" str "")))

; string? -> any?
; Reads from a string
(define (read-string str)
  (read (open-input-string str)))

(define a-lexer (new-dssl2-lexer (current-input-port)))

(let loop ()
  (define token (a-lexer))
  (unless (eq? token 'EOF)
    (displayln token)
    (loop)))
