#lang racket/base

(provide parse-dssl2)
(require "lexer.rkt"
         (only-in parser-tools/lex
                  position-line
                  position-col
                  position-offset)
         (only-in racket/syntax
                  format-id)
         parser-tools/yacc
         syntax/readerr)
(require (for-syntax racket/base))

(define (parse-dssl2 src port interactive?)
  ((dssl2-parser src)
   (new-dssl2-lexer src port interactive?)))


(define (dssl2-parser src)
  (define (parser-error tok-ok? tok-name tok-value start-pos end-pos)
    (raise-read-error (format "Syntax error: unexpected token ‘~a’"
                              (or tok-value tok-name))
                      src
                      (position-line start-pos)
                      (position-col start-pos)
                      (position-offset start-pos)
                      (max 1
                           (- (position-offset end-pos)
                              (position-offset start-pos)))))

  (define (locate start end sexp)
    (datum->syntax #false
                   sexp
                   (list src
                         (position-line start)
                         (position-col start)
                         (position-offset start)
                         (- (position-offset end)
                            (position-offset start)))))

  (define-syntax (loc stx)
    (syntax-case stx ()
      [(_ sexp)
       (with-syntax [(start (datum->syntax stx '$1-start-pos))
                     (end   (datum->syntax stx '$n-end-pos))]
         #'(locate start end sexp))]))

  (define-syntax (sym stx)
    (syntax-case stx ()
      [(_ sexp)
       (with-syntax [(start (datum->syntax stx '$1-start-pos))
                     (end   (datum->syntax stx '$n-end-pos))]
         (syntax-property #'(locate start end sexp)
                          'original-for-check-syntax #t))]))

  (define (symbol/pos sym pos)
    (let ([port (open-input-string (format "~s" sym))])
      (port-count-lines! port)
      (set-port-next-location! port
                               (position-line pos)
                               (position-col pos)
                               (position-offset pos))
      (read-syntax src port)))

  (define-syntax (symbol stx)
    (syntax-case stx ()
      [(_ sym)
       (with-syntax [(start (datum->syntax stx '$1-start-pos))]
         #'(symbol/pos sym start))]))

  (parser
    (tokens dssl2-empty-tokens dssl2-tokens)
    (src-pos)
    (suppress)
    (error parser-error)
    (start <program>)
    (end EOF)
    (grammar

      (<program>
        [(<whitespace>)
         eof]
        [(<whitespace> <statements> <whitespace>)
         (loc `(begin ,@$2))])

      (<whitespace>
        [()
         #true]
        [(INDENT <whitespace> DEDENT <whitespace>)
         #true]
        [(NEWLINE <whitespace>)
         #true])

      (<statements>
        [(<statement> <newlines>)
         $1]
        [(<statement> <newlines> <statements>)
         (append $1 $3)])

      (<newlines>
        [()
         #true]
        [(NEWLINE <newlines>)
         #true])

      (<statement>
        [(<simple-statement>)
         $1]
        [(<compound-statement>)
         (list $1)])

      (<compound-statement>
        [(IF <expr0> COLON <suite> <elifs> <maybe-else>)
         (loc `(,(symbol 'if)
                 [,$2 ,@$4]
                 ,@$5
                 ,$6))]
        [(WHILE <expr0> COLON <suite>)
         (loc `(,(symbol 'while) ,$2 ,@$4))]
        [(FOR <ident> IN <expr> COLON <suite>)
         (loc `(,(symbol 'for) [,$2 ,$4] ,@$6))]
        [(FOR <ident> COMMA <ident> IN <expr> COLON <suite>)
         (loc `(,(symbol 'for) [(,$2 ,$4) ,$6] ,@$8))]
        [(DEF <ident> <foralls> LPAREN <contract-formals> RPAREN <result>
              COLON <suite>)
         (loc `(,(symbol 'def) (,$2 ,@$3 ,@$5) ,@$7 ,@$9))]
        [(TEST <expr> COLON <suite>)
         (loc `(,(symbol 'test) ,$2 ,@$4))]
        [(TEST COLON <suite>)
         (loc `(,(symbol 'test) "<anonymous-test>" ,@$3))]
        [(TIME <expr> COLON <suite>)
         (loc `(,(symbol 'time) ,$2 ,@$4))]
        [(TIME COLON <suite>)
         (loc `(,(symbol 'time) "<anonymous-time>" ,@$3))])

      (<elifs>
        [()
         `()]
        [(<elif> <elifs>)
         (cons $1 $2)])

      (<elif>
        [(ELIF <expr0> COLON <suite>)
         (loc `[,$2 ,@$4])])

      (<maybe-else>
        [()
         `[else (pass)]]
        [(ELSE COLON <suite>)
         (loc `[,(symbol 'else) ,@$3])])

      (<result>
        [(ARROW <expr>)
         `(#:-> ,$2)]
        [()
         `()])

      (<suite>
        [(<simple-statement>)
         $1]
        [(NEWLINE INDENT <statements> DEDENT)
         $3])

      (<simple-statement>
        [(<single-line-statement> NEWLINE)
         $1])

      (<single-line-statement>
        [(<small-statement> <more-small-statements>)
         (cons $1 $2)])

      (<more-small-statements>
        [()
         `()]
        [(SEMICOLON <small-statement> <more-small-statements>)
         (cons $2 $3)])

      (<small-statement>
        [(<expr>)
         $1]
        [(LET <contract-formal>)
         (loc `(,(symbol 'let) ,$2))]
        [(LET <contract-formal> EQUALS <expr>)
         (loc `(,(symbol 'let) ,$2 ,$4))]
        [(DEFSTRUCT <ident> LPAREN <contract-formals> RPAREN)
         (loc `(,(symbol 'defstruct) ,$2 ,$4))]
        [(BREAK)
         (loc `(,(symbol 'break)))]
        [(CONTINUE)
         (loc `(,(symbol 'continue)))]
        [(IMPORT <ident>)
         (loc `(,(symbol 'import) ,$2))]
        [(IMPORT STRING-LITERAL)
         (loc `(,(symbol 'import) ,$2))]
        [(RETURN <expr>)
         (loc `(,(symbol 'return) ,$2))]
        [(RETURN)
         (loc `(,(symbol 'return)))]
        [(<lvalue> EQUALS <expr>)
         (loc `(,(symbol/pos '= $2-start-pos) ,$1 ,$3))]
        [(ASSERT <expr>)
         (loc `(,(symbol 'assert) ,$2))]
        [(ASSERT-EQ <expr> COMMA <expr>)
         (loc `(,(symbol 'assert_eq) ,$2 ,$4))]
        [(ASSERT-ERROR <expr>)
         (loc `(,(symbol 'assert_error) ,$2))]
        [(ASSERT-ERROR <expr> COMMA STRING-LITERAL)
         (loc `(,(symbol 'assert_error) ,$2 ,$4))]
        [(PASS)
         (loc `(pass))])

      (<foralls>
        [()
         `()]
        [(LBRACK <formals> RBRACK)
         `(#:forall ,$2)])

      (<contract-formals>
        [()
         `()]
        [(<contract-formal>)
         (loc (list $1))]
        [(<contract-formal> COMMA <contract-formals>)
         (loc (cons $1 $3))])

      (<contract-formal>
        [(<ident> COLON <expr>)
         (loc (list $1 $3))]
        [(<ident>)
         $1])

      (<formals>
        [()
         `()]
        [(<ident>)
         (loc (list $1))]
        [(<ident> COMMA <formals>)
         (loc (cons $1 $3))])

      (<ident>
        [(IDENT)
         (symbol $1)])

      (<lvalue>
        [(<ident>)
         $1]
        [(<atom> PERIOD <ident>)
         (loc `(struct-ref ,$1 ,$3))]
        [(<atom> LBRACK <expr> RBRACK)
         (loc `(vec-ref ,$1 ,$3))])

      (<atom>
        [(<lvalue>)
         $1]
        [(STRING-LITERAL)
         (loc $1)]
        [(LITERAL)
         (loc $1)]
        [(<atom> LPAREN <actuals> RPAREN)
         (loc `(,$1 ,@$3))]
        [(LBRACK <actuals> RBRACK)
         (loc `(vec ,@$2))]
        [(LBRACK <expr> SEMICOLON <expr> RBRACK)
         (loc `(make-vec ,$4 ,$2))]
        [(LBRACK <expr> FOR <ident> IN <expr0> RBRACK)
         (loc `(for/vec [,$4 ,$6] ,$2))]
        [(LBRACK <expr> FOR <ident> COMMA <ident> IN <expr0> RBRACK)
         (loc `(for/vec [(,$4 ,$6) ,$8] ,$2))]
        [(LBRACK <expr> FOR <ident> IN <expr0> IF <expr> RBRACK)
         (loc `(for/vec [,$4 ,$6] #:when ,$8 ,$2))]
        [(LBRACK <expr> FOR <ident> COMMA <ident> IN <expr0> IF <expr> RBRACK)
         (loc `(for/vec [(,$4 ,$6) ,$8] #:when ,$10 ,$2))]
        [(<ident> LBRACE <fields> RBRACE)
         (loc `(,(format-id #f "m:~a" $1 #:source $1) ,@$3))]
        [(OBJECT <ident> LBRACE <fields> RBRACE)
         (loc `(,(symbol 'object) ,$2 ,@$4))]
        [(LPAREN <expr> RPAREN)
         (loc $2)])

      (<actuals>
        [()
         `()]
        [(<expr>)
         (list $1)]
        [(<expr> COMMA <actuals>)
         (cons $1 $3)])

      (<fields>
        [()
         `()]
        [(<field>)
         (list $1)]
        [(<field> COMMA <fields>)
         (cons $1 $3)])

      (<field>
        [(<ident> COLON <expr>)
         (loc `[,$1 ,$3])]
        [(<ident>)
         (loc `[,$1 ,$1])])

      (<expr>
        [(LAMBDA <formals> COLON <single-line-statement>)
         (loc `(,(symbol 'lambda) ,$2 ,@$4))]
        [(<expr0> IF <expr0> ELSE <expr>)
         (loc `(if-e ,$3 ,$1 ,$5))]
        [(<expr0>)
         $1])

      (<expr0>
        [(<expr0> OP0 <expr1>)
         (loc `(,(symbol/pos $2 $2-start-pos) ,$1 ,$3))]
        [(<expr1>)
         $1])

      (<expr1>
        [(<expr1> OP1 <expr2>)
         (loc `(,(symbol/pos $2 $2-start-pos) ,$1 ,$3))]
        [(<expr2>)
         $1])

      (<expr2>
        [(<expr3> OP2 <expr3>)
         (loc `(,(symbol/pos $2 $2-start-pos) ,$1 ,$3))]
        [(<expr3>)
         $1])

      (<expr3>
        [(<expr3> OP3 <expr4>)
         (loc `(,(symbol/pos $2 $2-start-pos) ,$1 ,$3))]
        [(<expr4>)
         $1])

      (<expr4>
        [(<expr4> OP4 <expr5>)
         (loc `(,(symbol/pos $2 $2-start-pos) ,$1 ,$3))]
        [(<expr5>)
         $1])

      (<expr5>
        [(<expr5> OP5 <expr6>)
         (loc `(,(symbol/pos $2 $2-start-pos) ,$1 ,$3))]
        [(<expr6>)
         $1])

      (<expr6>
        [(<expr6> OP6 <expr7>)
         (loc `(,(symbol/pos $2 $2-start-pos) ,$1 ,$3))]
        [(<expr7>)
         $1])

      (<expr7>
        [(<expr7> PLUS <expr8>)
         (loc `(,(symbol/pos '+ $2-start-pos) ,$1 ,$3))]
        [(<expr7> MINUS <expr8>)
         (loc `(,(symbol/pos '- $2-start-pos) ,$1 ,$3))]
        [(<expr7> OP7 <expr8>)
         (loc `(,(symbol/pos $2 $2-start-pos) ,$1 ,$3))]
        [(<expr8>)
         $1])

      (<expr8>
        [(<expr8> OP8 <expr9>)
         (loc `(,(symbol/pos $2 $2-start-pos) ,$1 ,$3))]
        [(<expr9>)
         $1])

      (<expr9>
        [(OP9 <expr9>)
         (loc `(,(symbol $1) ,$2))]
        [(PLUS <expr9>)
         (loc `(,(symbol '+) ,$2))]
        [(MINUS <expr9>)
         (loc `(,(symbol '-) ,$2))]
        [(<expr10>)
         $1])

      (<expr10>
        [(<atom> OP10 <expr10>)
         (loc `(,(symbol/pos $2 $2-start-pos) ,$1 ,$3))]
        [(<atom>)
         $1]))))
