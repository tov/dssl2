#lang racket/base

(provide dssl2-empty-tokens dssl2-tokens new-dssl2-lexer
         natural float hexadecimal octal binary
         comment identifier
         sq-str-char dq-str-char
         lsq-str-contents ldq-str-contents)
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         racket/list
         racket/match
         syntax/readerr
         (for-syntax racket/base))

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
   IS
   NOT
   NOT-IN       ; ∉
   PLUS         ; two different precedences
   MINUS        ; two different precedences
   ARROW
   LAMBDA
   ASSERT
   ASSERT-ERROR
   LET
   IF
   ELIF
   ELSE
   WHILE
   FOR
   IN
   BREAK
   CONTINUE
   IMPORT
   RETURN
   PASS
   TEST
   TIME
   DEF
   STRUCT
   INTERFACE
   CLASS))

(define-tokens dssl2-tokens
  (IDENT
   OP0  ; or
   OP1  ; and
   OP2  ; not
   OP3  ; == != ≠ <= ≤ >= ≥ > (< is "is not")
   OP4  ; (|)
   OP5  ; ^
   OP6  ; &
   OP7  ; << >>
   OP8  ; (+ -)
   OP9  ; * / // %
   OP10 ; (unary) ~ (+ -)
   OP11 ; **
   OP-LESS
   LITERAL
   STRING-LITERAL))

(define-lex-abbrevs
  [line-break  (:: (:? #\return) #\newline)]
  [space       (:or #\space #\uA0)]
  [not-eol     (:- any-char (:or #\return #\newline))]
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
  [comment     (:: #\# (:* not-eol))]
  [sq-str-char (:or (:- not-eol (:or #\\ #\'))
                    (:: #\\ (:or not-eol line-break)))]
  [dq-str-char (:or (:- not-eol (:or #\\ #\"))
                    (:: #\\ (:or not-eol line-break)))]
  [lsq-str-contents
               (:- (:* (:or (:- any-char #\\)
                            (:: #\\ any-char)))
                   (:: (:* any-char)
                       #\' #\' #\'
                       (:* any-char))
                   (:: (:* any-char)
                       #\'))]
  [ldq-str-contents
               (:- (:* (:or (:- any-char #\\)
                            (:: #\\ any-char)))
                   (:: (:* any-char)
                       #\" #\" #\"
                       (:* any-char))
                   (:: (:* any-char)
                       #\"))]
  [identifier  (:: (:or alphabetic #\_)
                   (:* (:or alphabetic numeric #\_))
                   (:? (:or #\! #\?)))])

; new-dssl2-lexer : string? input-port? boolean? -> [ -> Token]
(define (new-dssl2-lexer src port interactive?)
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

  (define (lexical-error pos msg . args)
    (define offset (position-offset pos))
    (raise-read-error (apply format msg args)
                      src
                      (position-line pos)
                      (position-col pos)
                      offset
                      (and offset (max 1 (- (file-position port) offset)))))

  (define (lexical-unexpected pos descr culprit)
    (cond
      [culprit (lexical-error pos "Unexpected ~a ‘~a’" descr culprit)]
      [else    (lexical-error pos "Unexpected ~a"      descr)]))

  (define (closing closer token pos)
    (cond
      [(number? (first stack))
       (lexical-unexpected pos "closing delimeter" closer)]
      [(eq? (first stack) closer)
       (pop)
       (enq token pos pos)
       (deq)]
      [else
        (lexical-error pos "Expected ‘~a’ but got ‘~a’"
                       (first stack) closer)]))

  (define (on-eof start-pos end-pos)
    (when (not (eqv? 0 (first stack)))
      (enq (token-NEWLINE) start-pos end-pos))
    (let loop []
      (when (not (eqv? 0 (first stack)))
        (enq (token-DEDENT) start-pos end-pos)
        (pop)
        (loop)))
    (enq (token-NEWLINE) start-pos end-pos)
    (enq (token-EOF) start-pos end-pos)
    (deq))

  (define (on-indent indent start-pos end-pos)
    (cond
      [(number? (first stack))
       (enq (token-NEWLINE) start-pos end-pos)
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
                 (lexical-error start-pos "Inconsistent dedent")]))])
       (deq)]
      [else (the-lexer port)]))

  (define the-lexer
    (lexer-src-pos
      [(eof)                    (return-without-pos
                                  (on-eof start-pos end-pos))]
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
      ["is"                     (token-IS)]
      ["not"                    (token-NOT)]
      [#\∉                      (token-NOT-IN)]
      [#\+                      (token-PLUS)]
      [#\-                      (token-MINUS)]
      ["->"                     (token-ARROW)]
      ["let"                    (token-LET)]
      ["assert"                 (token-ASSERT)]
      ["assert_error"           (token-ASSERT-ERROR)]
      ["if"                     (token-IF)]
      ["elif"                   (token-ELIF)]
      ["else"                   (token-ELSE)]
      ["while"                  (token-WHILE)]
      ["for"                    (token-FOR)]
      [(:or "in" "∈")           (token-IN)]
      ["break"                  (token-BREAK)]
      ["continue"               (token-CONTINUE)]
      ["import"                 (token-IMPORT)]
      ["return"                 (token-RETURN)]
      ["pass"                   (token-PASS)]
      ["lambda"                 (token-LAMBDA)]
      [#\λ                      (token-LAMBDA)]
      ["True"                   (token-LITERAL #t)]
      ["False"                  (token-LITERAL #f)]
      ["None"                   (token-LITERAL (void))]
      ["def"                    (token-DEF)]
      ["struct"                 (token-STRUCT)]
      ["interface"              (token-INTERFACE)]
      ["class"                  (token-CLASS)]
      ["test"                   (token-TEST)]
      ["time"                   (token-TIME)]
      ["or"                     (token-OP0 (string->symbol lexeme))]
      ["and"                    (token-OP1 (string->symbol lexeme))]
      [(:or "==" "<=" #\≤ #\> ">=" #\≥ "!=" #\≠)
                                (token-OP3 (string->symbol lexeme))]
      [#\<                      (token-OP-LESS (string->symbol lexeme))]
      [#\|                      (token-OP4 (string->symbol lexeme))]
      [#\^                      (token-OP5 (string->symbol lexeme))]
      [#\&                      (token-OP6 (string->symbol lexeme))]
      [(:or "<<" ">>")          (token-OP7 (string->symbol lexeme))]
      [(:or #\* #\/ "//" #\%)   (token-OP9 (string->symbol lexeme))]
      [#\~                      (token-OP10 (string->symbol lexeme))]
      ["**"                     (token-OP11 (string->symbol lexeme))]
      [(:: "'''" lsq-str-contents "'''")
       (token-STRING-LITERAL
         (interpret-string (remove-first-and-last 3 lexeme)))]
      [(:: "\"\"\"" ldq-str-contents "\"\"\"")
       (token-STRING-LITERAL
         (interpret-string (remove-first-and-last 3 lexeme)))]
      [(:: "'''" lsq-str-contents)
       (lexical-error start-pos "Unterminated long string")]
      [(:: "\"\"\"" ldq-str-contents)
       (lexical-error start-pos "Unterminated long string")]
      [(:: #\" (:* dq-str-char) #\")
       (token-STRING-LITERAL
         (interpret-string (remove-first-and-last 1 lexeme)))]
      [(:: #\' (:* sq-str-char) #\')
       (token-STRING-LITERAL
         (interpret-string (remove-first-and-last 1 lexeme)))]
      [(:: #\" (:* dq-str-char))
       (lexical-error start-pos "Unterminated string")]
      [(:: #\' (:* sq-str-char))
       (lexical-error start-pos "Unterminated string")]
      [natural                  (token-LITERAL (read-string lexeme))]
      [float                    (token-LITERAL (read-string lexeme))]
      [hexadecimal              (token-LITERAL (interpret-non-dec lexeme))]
      [octal                    (token-LITERAL (interpret-non-dec lexeme))]
      [binary                   (token-LITERAL (interpret-non-dec lexeme))]
      ["inf"                    (token-LITERAL +inf.0)]
      ["nan"                    (token-LITERAL +nan.0)]
      [identifier               (token-IDENT (string->symbol lexeme))]
      [space
       (return-without-pos (the-lexer port))]
      [comment
       (return-without-pos (the-lexer port))]
      [(:: #\\ (:* space) (:? comment) line-break)
       (return-without-pos (the-lexer port))]
      [(:: (:* (:: line-break (:* space) (:? comment)))
           line-break (:* space))
       (return-without-pos
         (on-indent (last-spaces lexeme) start-pos end-pos))]
      [#\tab
       (lexical-error start-pos "Tabs are not allowed in DSSL2")]
      [any-char
       (lexical-error start-pos "Unexpected character ‘~a’ (~a)"
                      lexeme (char->integer (string-ref lexeme 0)))]
      [(special)
       (lexical-unexpected start-pos "special" lexeme)]
      [(special-comment)
       (lexical-unexpected start-pos "special comment" lexeme)]))

  (port-count-lines! port)

  (λ ()
     (define result
           (cond
             [(cons? queue)   (deq)]
             [else            (the-lexer port)]))
     ; (displayln (format "Token: ~a" result))
     result))

; string? -> string?
; Removes the first `n` and last `n` characters of a string.
(define (remove-first-and-last n str)
  (substring str n (- (string-length str) n)))

; Expands to a match pattern that matches an octal digit.
(define-match-expander digit/8
  (λ (stx)
     (syntax-case stx ()
       [(_ pat ...)
        #'(and (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
               pat
               ...)])))
; Expands to a match pattern that matches a hex digit.
(define-match-expander digit/16
  (λ (stx)
     (syntax-case stx ()
       [(_ pat ...)
        #'(and (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                   #\A #\B #\C #\D #\E #\F
                   #\a #\b #\c #\d #\e #\f)
               pat
               ...)])))

; string? -> string?
; Interprets the escapes in a string literal.
(define (interpret-string lit)
  (define (unescape chars)
    (match chars
      ; C-style escapes
      [(cons #\a rst)   (values rst #\007)]
      [(cons #\b rst)   (values rst #\backspace)]
      [(cons #\f rst)   (values rst #\page)]
      [(cons #\n rst)   (values rst #\newline)]
      [(cons #\r rst)   (values rst #\return)]
      [(cons #\t rst)   (values rst #\tab)]
      [(cons #\v rst)   (values rst #\vtab)]
      ; hexadecimal character escapes
      [(list* #\x (digit/16 c1) (digit/16 c2) rst)
       (values rst (hex->char c1 c2))]
      [(list* #\x (digit/16 c1) rst)
       (values rst (hex->char c1))]
      ; octal character escapes
      [(list* (digit/8 c1) (digit/8 c2) (digit/8 c3) rst)
       (values rst (oct->char c1 c2 c3))]
      [(list* (digit/8 c1) (digit/8 c2) rst)
       (values rst (oct->char c1 c2))]
      [(list* (digit/8 c1) rst)
       (values rst (oct->char c1))]
      ; escaped line breaks
      [(or (list* #\return #\newline rst)
           (list* #\newline rst))
       (values rst #f)]
      ; otherwise
      [(cons c1 rst)
       (values rst c1)]))
  (define (loop chars acc)
    (match chars
      ['()
       (reverse acc)]
      [(cons #\\ escaped)
        (define-values (rst c) (unescape escaped))
        (if (eq? c #false)
          (loop rst acc)
          (loop rst (cons c acc)))]
      [(list* #\return #\newline rst)
       (loop rst (cons #\newline acc))]
      [(cons c1 rst)
       (loop rst (cons c1 acc))]))
  (list->string (loop (string->list lit) '())))

; char? ... -> char?
; Converts two hex digits to the represented character.
(define (hex->char . digits)
  (integer->char
    (read-string (list->string (list* #\# #\x digits)))))

; char? ... -> char?
; Converts octal digits to the represented character.
(define (oct->char . digits)
  (integer->char
    (read-string (list->string (list* #\# #\o digits)))))

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

