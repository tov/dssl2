#lang racket/base

(provide get-syntax-token)

(require "../lexer.rkt"
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         syntax/parse/define
         (for-syntax racket/base
                     (only-in racket/syntax
                              format-id)
                     syntax/parse))

(define-syntax (token stx)
  (syntax-parse stx
    [(_ type data:string)
     (with-syntax
       ([data-sym  (format-id stx "~a" (syntax-e #'data))])
       #'(token type data-sym))]
    [(_ type data)
     (with-syntax
       ([lexeme    (datum->syntax stx 'lexeme)]
        [start-pos (datum->syntax stx 'start-pos)]
        [end-pos   (datum->syntax stx 'end-pos)])
       #'(values lexeme 'type 'data
                 (position-offset start-pos)
                 (position-offset end-pos)))]
    [(_ type)
     #'(token type #f)]))

(define get-syntax-token
  (lexer
    [(eof)                      (token eof)]
    [(:+ whitespace)            (token white-space)]
    [(:: #\\ (:* whitespace) (:? comment) #\newline)
                                (token comment)]
    [(:or "λ" "lambda" "let" "assert" "assert_eq" "assert_error"
          "if" "elif" "else"
          "class" "interface" "break" "continue"
          "while" "for" "in" "∈" "∉" "return" "pass" "def" "struct"
          "test" "time" "import")
                                (token keyword)]
    [comment                    (token comment)]
    [(:or float hexadecimal octal binary natural "inf" "nan")
                                (token constant)]
    [(:: "'''" lsq-str-contents "'''")          (token string)]
    [(:: "'''" lsq-str-contents)                (token error)]
    [(:: "\"\"\"" ldq-str-contents "\"\"\"")    (token string)]
    [(:: "\"\"\"" ldq-str-contents)             (token error)]
    [(:: #\' (:* sq-str-char) #\')              (token string)]
    [(:: #\' (:* sq-str-char))                  (token error)]
    [(:: #\" (:* dq-str-char) #\")              (token string)]
    [(:: #\" (:* dq-str-char))                  (token error)]
    [#\(                        (token parenthesis "(")]
    [#\)                        (token parenthesis ")")]
    [#\[                        (token parenthesis "[")]
    [#\]                        (token parenthesis "]")]
    [#\{                        (token parenthesis "{")]
    [#\}                        (token parenthesis "}")]
    ["is"                       (token keyword)]
    [(:or "or" "and" "not")     (token keyword)]
    [(:or "==" #\< #\> "<=" #\≤ ">=" #\≥ "!=" #\≠
          #\| #\^ #\&
          "<<" ">>" #\* #\/ #\% #\~ "**" #\+ #\-)
                                (token hash-colon-keyword)]
    [(:or #\, #\: #\; #\=)      (token parenthesis)]
    [(:or "True" "False")       (token constant)]
    [(:or "None")               (token constant)]
    [#\.                        (token hash-colon-keyword)]
    [identifier                 (token symbol)]
    [any-char                   (token error)]
    [(special)                  (token error)]
    [(special-comment)          (token error)]))

