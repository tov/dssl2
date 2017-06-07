#lang racket

(provide dssl2-empty-tokens dssl2-tokens new-dssl2-lexer
         syntax-error)
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
   LAMBDA
   ASSERT
   ASSERT-EQ
   LET
   IF
   ELIF
   ELSE
   WHILE
   FOR
   IN
   RETURN
   PASS
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
  [exponent    (:: (:or #\e #\E) (:? #\-) natural)]
  [pointfloat  (:or (:: natural #\. (:* numeric))
                    (:: (:* numeric) #\. natural))]
  [float       (:or (:: pointfloat (:? exponent))
                    (:: natural exponent))]
  [hexdigit    (:or numeric (char-range #\a #\f) (char-range #\A #\F))]
  [hexadecimal (:: (:? #\-) (:or "0x" "0X") (:+ hexdigit))]
  [octdigit    (char-range #\0 #\7)]
  [octal       (:: (:? #\-) (:or "0o" "0O") (:+ octdigit))]
  [binary      (:: (:? #\-) (:or "0b" "0B") (:+ (:or #\0 #\1)))]
  [sq-str-char (:or (:- any-char (:or #\\ #\' #\newline))
                    (:: #\\ any-char))]
  [dq-str-char (:or (:- any-char (:or #\\ #\" #\newline))
                    (:: #\\ any-char))])

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

  (define (enq item start end)
    (set! queue (append queue (list (position-token item start end)))))

  (define (deq)
    (define result (first queue))
    (set! queue (rest queue))
    result)

  (define (closing closer token pos)
    (cond
      [(number? (first stack))
       (pop)
       (enq (token-DEDENT) pos pos)
       (closing closer token)]
      [(eq? (first stack) closer)
       (pop)
       (enq token pos pos)
       (deq)]
      [else
        (syntax-error pos "Expected ~a but got ~a"
                      (first stack) closer)]))

  (define the-lexer
    (lexer-src-pos
      [(eof)                    (begin
                                  (let loop []
                                    (when (not (eqv? 0 (first stack)))
                                      (enq (token-NEWLINE) start-pos end-pos)
                                      (enq (token-DEDENT) start-pos end-pos)
                                      (pop)
                                      (loop)))
                                  (enq (token-EOF) start-pos end-pos)
                                  (return-without-pos (deq)))]
      [#\(
                                (begin
                                  (push #\))
                                  (token-LPAREN))]
      [#\[
       (begin
         (push #\])
         (token-LBRACK))]
      [#\{                      (begin
                                  (push #\})
                                  (token-LBRACE))]
      [#\)                      (return-without-pos
                                  (closing #\) (token-RPAREN) start-pos))]
      [#\]                      (return-without-pos
                                  (closing #\] (token-RBRACK) start-pos))]
      [#\}                      (return-without-pos
                                  (closing #\} (token-RBRACE) start-pos))]
      [#\,                      (token-COMMA)]
      [#\.                      (token-PERIOD)]
      [#\:                      (token-COLON)]
      [#\;                      (token-SEMICOLON)]
      [#\=                      (token-EQUALS)]
      [#\+                      (token-PLUS)]
      [#\-                      (token-MINUS)]
      ["let"                    (token-LET)]
      ["assert"                 (token-ASSERT)]
      ["assert_eq"              (token-ASSERT-EQ)]
      ["if"                     (token-IF)]
      ["elif"                   (token-ELIF)]
      ["else"                   (token-ELSE)]
      ["while"                  (token-WHILE)]
      ["for"                    (token-FOR)]
      ["in"                     (token-IN)]
      ["return"                 (token-RETURN)]
      ["pass"                   (token-PASS)]
      ["lambda"                 (token-LAMBDA)]
      [#\λ                      (token-LAMBDA)]
      ["True"                   (token-LITERAL #t)]
      ["False"                  (token-LITERAL #f)]
      ["def"                    (token-DEF)]
      ["defstruct"              (token-DEFSTRUCT)]
      [(:: alphabetic (:* (:or alphabetic numeric #\_)) (:? (:or #\! #\?)))
                                (token-IDENT (string->symbol lexeme))]
      ["||"                     (token-OP0 (string->symbol lexeme))]
      ["&&"                     (token-OP1 (string->symbol lexeme))]
      [(:or "==" #\< #\> "<=" ">=" "!=" "===" "!==")
                                (token-OP2 (string->symbol lexeme))]
      [#\|                      (token-OP3 (string->symbol lexeme))]
      [#\^                      (token-OP4 (string->symbol lexeme))]
      [#\&                      (token-OP5 (string->symbol lexeme))]
      [(:or "<<" ">>")          (token-OP6 (string->symbol lexeme))]
      [(:or #\* #\/ #\%)        (token-OP8 (string->symbol lexeme))]
      [(:or #\! #\~)            (token-OP9 (string->symbol lexeme))]
      ["**"                     (token-OP10 (string->symbol lexeme))]
      [(:: #\" (:* dq-str-char) #\")
       (token-LITERAL
         (interpret-string (remove-first-and-last lexeme)))]
      [(:: #\' (:* sq-str-char) #\')
       (token-LITERAL
         (interpret-string (remove-first-and-last lexeme)))]
      [(:: #\" (:* dq-str-char))
       (syntax-error end-pos "Unterminated string")]
      [(:: #\' (:* sq-str-char))
       (syntax-error end-pos "Unterminated string")]
      [natural                  (token-LITERAL (read-string lexeme))]
      [float                    (token-LITERAL (read-string lexeme))]
      [hexadecimal              (token-LITERAL (interpret-non-dec lexeme))]
      [octal                    (token-LITERAL (interpret-non-dec lexeme))]
      [binary                   (token-LITERAL (interpret-non-dec lexeme))]
      ["-inf.0"                 (token-LITERAL -inf.0)]
      ["+inf.0"                 (token-LITERAL +inf.0)]
      ["-nan.0"                 (token-LITERAL -nan.0)]
      ["+nan.0"                 (token-LITERAL +nan.0)]
      [(:or #\space)
       (return-without-pos (the-lexer port))]
      [(:: #\# (:* (:- any-char #\newline)))
       (return-without-pos (the-lexer port))]
      [(:+ (:: #\newline (:* #\space)))
       (let [(indent (last-spaces lexeme))]
         (enq (token-NEWLINE) start-pos end-pos)
         (cond
           [(number? (first stack))
            (cond
              [(> indent (first stack))
               (push indent)
               (enq (token-INDENT) start-pos end-pos)]
              [(= indent (first stack))
               (void)]
              [else
                (let loop []
                  (cond
                    [(< indent (first stack))
                     (enq (token-DEDENT) start-pos end-pos)
                     (pop)
                     (loop)]
                    [(= indent (first stack))
                     (void)]
                    [else
                      (syntax-error start-pos
                                    "Inconsistent dedent")]))])
            (return-without-pos (deq))]
           [else (return-without-pos (the-lexer port))]))]
      [(:: #\\ #\newline)
       (return-without-pos (the-lexer port))]
      [#\tab
       (syntax-error start-pos "Tabs are not allowed in DSSL2")]
      [any-char
        (syntax-error start-pos "Unexpected character ‘~a’" lexeme)]))

  (port-count-lines! port)

  (λ ()
     (define result
       (cond
         [(cons? queue)   (deq)]
         [else            (the-lexer port)]))
     ; (displayln (format "Token: ~a" result))
     result))

; format-string position? any? ... -> !
; Calls error with a nice syntax error message.
(define (syntax-error pos msg . args)
  (error
    (apply format
           (string-append "Syntax error (line ~a column ~a): " msg)
           (position-line pos)
           (add1 (position-col pos))
           args)))

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
    (read-string (list->string (list #\# #\x digit1 digit2)))))

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

(module+ test
  (define a-lexer (new-dssl2-lexer (current-input-port)))

  (let loop ()
    (define token (a-lexer))
    (unless (eq? (position-token-token token) 'EOF)
      (displayln token)
      (loop))))
