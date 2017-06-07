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
                 ,$6))])

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
        [(expr0)
         $1]
        [(LET IDENT)
         (loc `(define ,$2 #f))]
        [(LET IDENT EQUALS expr0)
         (loc `(define ,$2 ,$4))]
        [(DEFSTRUCT IDENT LPAREN params RPAREN)
         (loc `(define-struct ,$2 ,$4))]
        [(lvalue EQUALS expr0)
         (loc `(setf! ,$1 ,$3))]
        [(PASS)
         (loc `(pass))])

      (params
        [()
         `()]
        [(IDENT)
         (loc (list $1))]
        [(IDENT COMMA params)
         (loc (cons $1 $3))])

      (lvalue
        [(IDENT)
         $1]
        [(atom PERIOD IDENT)
         (loc `(struct-ref ,$1 ,$3))]
        [(atom LBRACK expr0 RBRACK)
         (loc `(vector-ref ,$1 ,$3))])

      (atom
        [(lvalue)
         $1]
        [(LITERAL)
         (loc $1)]
        [(LPAREN expr0 RPAREN)
         (loc $2)])

      (expr0
        [(expr0 PLUS atom)
         (loc `(+ ,$1 ,$3))]
        [(atom)
         $1]))))

