#lang racket

(provide get-syntax-token)

(require dssl2/private/lexer
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

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
    [(:or "λ" "lambda" "let" "assert" "assert_eq" "if" "elif" "else"
          "while" "for" "in" "return" "pass" "def" "defstruct")
                                (token keyword)]
    [comment                    (token comment)]
    [(:or float hexadecimal octal binary natural fp-special)
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
    [(:or "||" "&&")            (token keyword)]
    [(:or "==" #\< #\> "<=" ">=" "!=" "===" "!--" #\| #\^ #\&
          "<<" ">>" #\* #\/ #\% #\! #\~ "**" #\+ #\-)
                                (token hash-colon-keyword)]
    [(:or #\, #\: #\; #\=)      (token parenthesis)]
    [(:or "True" "False")       (token constant)]
    [#\.                        (token hash-colon-keyword)]
    [identifier                 (token symbol)]
    [any-char                   (token error)]
    [(special)                  (token error)]
    [(special-comment)          (token error)]))

