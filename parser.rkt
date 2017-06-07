#lang racket

(provide parse-dssl2)
(require dssl2/lexer
         (only-in parser-tools/lex
                  position-line
                  position-col
                  position-offset)
         parser-tools/yacc)

(define (parse-dssl2 port)
  ((dssl2-parser port) (new-dssl2-lexer port)))

(define (parser-error tok-ok? tok-name tok-value start-pos end-pos)
  (syntax-error start-pos "Unexpected token ‘~a’"
                (or tok-value tok-name)))

(define (dssl2-parser port)
  (define (locate start end sexp)
    (datum->syntax #false
                   sexp
                   (list port
                         (position-line start)
                         (position-col start)
                         (position-offset start)
                         (- (position-offset end)
                            (position-offset start)))))

  (define-syntax (loc stx)
    (syntax-case stx ()
      [(_ sexp)
       (let [(start (datum->syntax stx '$1-start-pos))
             (end   (datum->syntax stx '$n-end-pos))]
         #`(locate #,start #,end sexp))]))

  (parser
    (tokens dssl2-empty-tokens dssl2-tokens)
    (src-pos)
    (error parser-error)
    (start program)
    (end EOF)
    (grammar

      (program
        [(statements)
         (loc `(begin ,@$1))])

      (statements
        [()
         `()]
        [(statement statements)
         (append $1 $2)])

      (statement
        [(simple-statement)
         $1]
        [(compound-statement)
         (list $1)])

      (compound-statement
        [(IF expr0 COLON suite elifs maybe-else)
         (loc `(cond
                 [,$2 ,@$4]
                 ,@$5
                 ,$6))]
        [(WHILE expr0 COLON suite)
         (loc `(while ,$2 ,@$4))]
        [(FOR IDENT IN expr COLON suite)
         (loc `(for [,$2 ,$4] ,@$6))]
        [(FOR IDENT COMMA IDENT IN expr COLON suite)
         (loc `(for [(,$2 ,$4) ,$6] ,@$8))]
        [(DEF IDENT LPAREN formals RPAREN COLON suite)
         (loc `(define (,$2 ,@$4) ,@$7))])

      (elifs
        [()
         `()]
        [(elif elifs)
         (cons $1 $2)])

      (elif
        [(ELIF expr0 COLON suite)
         (loc `[,$2 ,@$4])])

      (maybe-else
        [()
         `[else (pass)]]
        [(ELSE COLON suite)
         (loc `[else ,@$3])])

      (suite
        [(simple-statement)
         $1]
        [(NEWLINE INDENT statements DEDENT)
         $3])

      (simple-statement
        [(small-statement more-small-statements NEWLINE)
         (cons $1 $2)])

      (more-small-statements
        [()
         `()]
        [(SEMICOLON small-statement more-small-statements)
         (cons $2 $3)])

      (small-statement
        [(expr)
         $1]
        [(LET IDENT)
         (loc `(define ,$2 #f))]
        [(LET IDENT EQUALS expr)
         (loc `(define ,$2 ,$4))]
        [(DEFSTRUCT IDENT LPAREN formals RPAREN)
         (loc `(define-struct ,$2 ,$4))]
        [(RETURN expr)
         (loc `(return ,$2))]
        [(lvalue EQUALS expr)
         (loc `(setf! ,$1 ,$3))]
        [(ASSERT expr)
         (loc `(assert ,$2))]
        [(ASSERT-EQ expr COMMA expr)
         (loc `(assert-eq ,$2 ,$4))]
        [(PASS)
         (loc `(pass))])

      (formals
        [()
         `()]
        [(IDENT)
         (loc (list $1))]
        [(IDENT COMMA formals)
         (loc (cons $1 $3))])

      (lvalue
        [(IDENT)
         $1]
        [(atom PERIOD IDENT)
         (loc `(struct-ref ,$1 ,$3))]
        [(atom LBRACK expr RBRACK)
         (loc `(vector-ref ,$1 ,$3))])

      (atom
        [(lvalue)
         $1]
        [(LITERAL)
         (loc $1)]
        [(atom LPAREN actuals RPAREN)
         (loc `(,$1 ,@$3))]
        [(LBRACK actuals RBRACK)
         (loc `(vector ,@$2))]
        [(LBRACK expr SEMICOLON expr RBRACK)
         (loc `(make-vector ,$4 ,$2))]
        [(LPAREN expr RPAREN)
         (loc $2)])

      (actuals
        [()
         `()]
        [(expr)
         (list $1)]
        [(expr COMMA actuals)
         (cons $1 $3)])

      (expr
        [(LAMBDA formals COLON expr)
         (loc `(lambda ,$2 ,$4))]
        [(expr0)
         $1])

      (expr0
        [(expr0 OP0 atom)
         (loc `(,$2 ,$1 ,$3))]
        [(expr1)
         $1])

      (expr1
        [(expr1 OP1 expr2)
         (loc `(,$2 ,$1 ,$3))]
        [(expr2)
         $1])

      (expr2
        [(expr3 OP2 expr3)
         (loc `(,$2 ,$1 ,$3))]
        [(expr3)
         $1])

      (expr3
        [(expr3 OP3 expr4)
         (loc `(,$2 ,$1 ,$3))]
        [(expr4)
         $1])

      (expr4
        [(expr4 OP4 expr5)
         (loc `(,$2 ,$1 ,$3))]
        [(expr5)
         $1])

      (expr5
        [(expr5 OP5 expr6)
         (loc `(,$2 ,$1 ,$3))]
        [(expr6)
         $1])

      (expr6
        [(expr6 OP6 expr7)
         (loc `(,$2 ,$1 ,$3))]
        [(expr7)
         $1])

      (expr7
        [(expr7 PLUS expr8)
         (loc `(+ ,$1 ,$3))]
        [(expr7 MINUS expr8)
         (loc `(- ,$1 ,$3))]
        [(expr8)
         $1])

      (expr8
        [(expr8 OP8 expr9)
         (loc `(,$2 ,$1 ,$3))]
        [(expr9)
         $1])

      (expr9
        [(OP9 expr10)
         (loc `(,$1 ,$2))]
        [(PLUS expr10)
         (loc `(+ ,$2))]
        [(MINUS expr10)
         (loc `(- ,$2))]
        [(expr10)
         $1])

      (expr10
        [(atom OP10 expr10)
         (loc `(,$2 ,$1 ,$3))]
        [(atom)
         $1]))))

