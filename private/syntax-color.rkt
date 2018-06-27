#lang racket/base

(provide get-syntax-token)

(require "lexer.rkt"
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         (for-syntax racket/base))

(define-syntax (token stx)
  (syntax-case stx ()
    [(_ type data)
     (let
       ([lexeme         (datum->syntax stx 'lexeme)]
        [start-pos      (datum->syntax stx 'start-pos)]
        [end-pos        (datum->syntax stx 'end-pos)])
       #`(values #,lexeme 'type 'data
                 (position-offset #,start-pos)
                 (position-offset #,end-pos)))]
    [(_ type)
     #'(token type #f)]))

(define get-syntax-token
  (lexer
    [(eof)                      (token eof)]
    [(:+ whitespace)            (token white-space)]
    [(:: #\\ #\newline)         (token comment)]
    [(:or "λ" "lambda" "let" "assert" "assert_eq" "assert_error"
          "if" "elif" "else"
          "class" "interface"
          "while" "for" "in" "return" "pass" "def" "struct" "test"
          "time" "import")
                                (token keyword)]
    [comment                    (token comment)]
    [(:or float hexadecimal octal binary natural "inf" "nan")
                                (token constant)]
    [(:: #\' (:* sq-str-char) #\')      (token string)]
    [(:: #\' (:* sq-str-char))          (token error)]
    [(:: #\" (:* dq-str-char) #\")      (token string)]
    [(:: #\" (:* dq-str-char))          (token error)]
    [#\(                        (token parenthesis |(|)]
    [#\)                        (token parenthesis |)|)]
    [#\[                        (token parenthesis |[|)]
    [#\]                        (token parenthesis |]|)]
    [#\{                        (token parenthesis |{|)]
    [#\}                        (token parenthesis |}|)]
    ["is"                       (token keyword)]
    [(:or "or" "and" "not")     (token keyword)]
    [(:or "==" #\< #\> "<=" ">=" "!=" #\| #\^ #\&
          "<<" ">>" #\* #\/ #\% #\~ "**" #\+ #\-)
                                (token hash-colon-keyword)]
    [(:or #\, #\: #\; #\=)      (token parenthesis)]
    [(:or "True" "False")       (token constant)]
    [#\.                        (token hash-colon-keyword)]
    [identifier                 (token symbol)]
    [any-char                   (token error)]
    [(special)                  (token error)]
    [(special-comment)          (token error)]))

