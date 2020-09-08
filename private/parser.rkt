#lang racket/base

(provide parse-dssl2)
(require "lexer.rkt"
         "names.rkt"
         (only-in parser-tools/lex
                  position-line
                  position-col
                  position-offset)
         (only-in racket/syntax)
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
       (with-syntax [(start (datum->syntax #'sexp '$1-start-pos))
                     (end   (datum->syntax #'sexp '$n-end-pos))]
         #'(locate start end sexp))]))

  (define (locate/symbol sym pos)
    (let ([port (open-input-string (format "~s" sym))])
      (port-count-lines! port)
      (set-port-next-location! port
                               (position-line pos)
                               (position-col pos)
                               (position-offset pos))
      (read-syntax src port)))

  (define-syntax (loc/head stx)
    (syntax-case stx ()
      [(_ sexp pos)
       (with-syntax [(start (datum->syntax #'sexp '$1-start-pos))
                     (end   (datum->syntax #'sexp '$n-end-pos))
                     (head  (datum->syntax #'sexp (syntax-e #'pos)))]
         #'(let ([svalue sexp])
             (locate start end (cons (locate/symbol (car svalue) head)
                                     (cdr svalue)))))]))

  (define-syntax-rule (loc/1 sexp)
    (loc/head sexp $1-start-pos))

  (define-syntax-rule (loc/2 sexp)
    (loc/head sexp $2-start-pos))

  (define-syntax-rule (loc/3 sexp)
    (loc/head sexp $3-start-pos))

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

      (<newlines+>
        [(NEWLINE <newlines>)
         #true])

      (<statement>
        [(<simple-statements> NEWLINE)
         $1]
        [(<compound-statement>)
         (list $1)])

      (<compound-statement>
        [(<mix-statement/large>)
         $1]
        [(WHILE <expr0> COLON <suite>)
         (loc/1 `(while ,$2 ,@$4))]
        [(FOR <ident> IN <expr> COLON <suite>)
         (loc/1 `(for [,$2 ,$4] ,@$6))]
        [(FOR <ident> COMMA <ident> IN <expr> COLON <suite>)
         (loc/1 `(for [(,$2 ,$4) ,$6] ,@$8))]
        [(DEF <ident> <foralls> LPAREN <contract-formals> RPAREN <result>
              COLON <suite>)
         (loc/1 `(def (,$2 ,@$3 ,@$5) ,@$7 ,@$9))]
        [(STRUCT <ident> COLON <struct-suite>)
         (loc/1 `(struct ,$2 ,@$4))]
        [(CLASS <ident> <foralls> <implemented-interfaces> COLON <class-suite>)
         (loc/1 `(class ,$2 ,@$3 ,@$4 ,@$6))]
        [(INTERFACE <ident> <foralls> <extended-interfaces> COLON
                    <interface-suite>)
         (loc/1 `(interface ,$2 ,@$3 ,$4 ,@$6))]
        [(TEST <timeout>)
         (loc/1 `(test ,@$2))]
        [(TEST <expr> <opt-timeout> COLON <suite>)
         (loc/1 `(test ,$2 ,@$3 ,@$5))]
        [(TEST <opt-timeout> COLON <suite>)
         (loc/1 `(test ,(anonymous-block 'test $1-start-pos) ,@$2 ,@$4))])

      (<simple-statements>
        [(<simple-statement> <more-simple-statements>)
         (cons $1 $2)])

      (<more-simple-statements>
        [()
         `()]
        [(SEMICOLON <simple-statement> <more-simple-statements>)
         (cons $2 $3)])

      (<simple-statement>
        [(<mix-statement/small>)
         $1]
        [(LET <contract-formal>)
         (loc/1 `(let ,$2))]
        [(BREAK)
         (loc/1 `(break))]
        [(CONTINUE)
         (loc/1 `(continue))]
        [(IMPORT <ident>)
         (loc/1 `(import ,$2))]
        [(IMPORT STRING-LITERAL)
         (loc/1 `(import ,$2))]
        [(RETURN)
         (loc/1 `(return))]
        [(ASSERT <timeout>)
         (loc/1 `(assert ,@$2))]
        [(ASSERT <expr> <opt-timeout>)
         (loc/1 `(assert ,$2 ,@$3))]
        [(ASSERT-EQ <expr> COMMA <expr> <opt-timeout>)
         (loc/1 `(assert ,(loc/3 `(== ,$2 ,$4)) ,@$5))]
        [(ASSERT-ERROR <expr> <opt-timeout>)
         (loc/1 `(assert_error ,$2 ,@$3))]
        [(ASSERT-ERROR <expr> COMMA STRING-LITERAL <opt-timeout>)
         (loc/1 `(assert_error ,$2 ,$4 ,@$5))]
        [(PASS)
         (loc/1 `(pass))])

      ; These statements are large if they contain large expressions…
      (<mix-statement/large>
        [(<large-expression>)
         $1]
        [(LET <contract-formal> EQUALS <large-expression>)
         (loc/1 `(let ,$2 ,$4))]
        [(RETURN <large-expression>)
         (loc/1 `(return ,$2))]
        [(<lvalue> EQUALS <large-expression>)
         (loc/2 `(= ,$1 ,$3))])

      ; …but small if they contain small expressions.
      (<mix-statement/small>
        [(<expr>)
         $1]
        [(LET <contract-formal> EQUALS <expr>)
         (loc/1 `(let ,$2 ,$4))]
        [(RETURN <expr>)
         (loc/1 `(return ,$2))]
        [(<lvalue> EQUALS <expr>)
         (loc/2 `(= ,$1 ,$3))])

      (<elifs>
        [()
         `()]
        [(<elif> <elifs>)
         (cons $1 $2)])

      (<elif>
        [(ELIF <expr0> COLON <suite>)
         (loc/1 `[elif ,$2 ,@$4])])

      (<maybe-else>
        [()
         `[else (pass)]]
        [(ELSE COLON <suite>)
         (loc/1 `[else ,@$3])])

      (<result>
        [(ARROW <expr>)
         `(#:-> ,$2)]
        [()
         `()])

      (<implemented-interfaces>
        [(LPAREN <formals> RPAREN) `(#:implements ,$2)]
        [()                        `()])

      (<extended-interfaces>
        [(LPAREN <instantiated-interface-list> RPAREN) $2]
        [()                        '()])

      (<instantiated-interface-list>
        [()                            '()]
        [(<instantiated-interface>)    (loc (list $1))]
        [(<instantiated-interface> COMMA <instantiated-interface-list>)
                                       (loc (cons $1 $3))])

      (<instantiated-interface>
        [(<ident>)                 $1]
        [(<ident> LBRACK <formals> RBRACK)
                                   (loc (cons $1 $3))])

      (<suite>
        [(<simple-statements> NEWLINE)
         $1]
        [(NEWLINE INDENT <statements> DEDENT)
         $3])

      (<struct-suite>
        [(NEWLINE INDENT PASS NEWLINE DEDENT)
         '()]
        [(PASS NEWLINE)
         '()]
        [(NEWLINE INDENT <class-or-struct-fields> DEDENT)
         $3])

      (<class-suite>
        [(NEWLINE INDENT <class-statements> DEDENT)
         $3])

      (<class-statements>
        [(<class-or-struct-fields> <class-methods>)
         (append $1 $2)])

      (<class-or-struct-fields>
        [() '()]
        [(<class-or-struct-field> SEMICOLON <class-or-struct-fields>)
         (cons $1 $3)]
        [(<class-or-struct-field> <newlines+> <class-or-struct-fields>)
         (cons $1 $3)])

      (<class-or-struct-field>
        [(LET <contract-formal>)
         (loc/1 `(let ,$2))])

      (<class-methods>
        [(<class-method>)
         (list $1)]
        [(<class-method> <newlines> <class-methods>)
         (cons $1 $3)])

      (<class-method>
        [(DEF <ident> <foralls> LPAREN <method-formals> RPAREN <result> COLON
              <suite>)
         (loc/1 `(def (,$2 ,@$3 ,@$5) ,@$7 ,@$9))])

      (<method-formals>
        [(<ident>)
         (list $1)]
        [(<ident> COMMA <contract-formals>)
         (cons $1 $3)])

      (<interface-suite>
        [(NEWLINE INDENT PASS NEWLINE DEDENT)
         '()]
        [(PASS NEWLINE)
         '()]
        [(NEWLINE INDENT <interface-methods> DEDENT)
         $3])

      (<interface-methods>
        [(<interface-method> <newlines+>)
         (list $1)]
        [(<interface-method> SEMICOLON <interface-methods>)
         (cons $1 $3)]
        [(<interface-method> <newlines+> <interface-methods>)
         (cons $1 $3)])

      (<interface-method>
        [(DEF <ident> <foralls> LPAREN <method-formals> RPAREN <result>)
         (loc/1 `(def (,$2 ,@$3 ,@$5) ,@$7))])

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
         (locate/symbol $1 $1-start-pos)])

      (<lvalue>
        [(<ident>)
         $1]
        [(<atom> PERIOD <ident>)
         (loc `(struct-ref ,$1 ,$3))]
        [(<atom> LBRACK <non-empty-actuals> RBRACK)
         (loc `(vec-ref ,$1 ,@$3))])

      (<string-literal>
        [(STRING-LITERAL)
           $1]
        [(STRING-LITERAL <string-literal>)
           (string-append $1 $2)])

      (<atom>
        [(<lvalue>)
         $1]
        [(<string-literal>)
         (loc $1)]
        [(LITERAL)
         (loc $1)]
        [(<atom> LPAREN <actuals> RPAREN)
         (loc `(,$1 ,@$3))]
        [(LBRACK <actuals> RBRACK)
         (loc `(vec-lit ,@$2))]
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
         (loc `(,(struct-special-name/located $1) ,@$3))]
        [(LPAREN <expr> RPAREN)
         (loc $2)])

      (<non-empty-actuals>
        [(<expr>)
         (list $1)]
        [(<expr> COMMA <non-empty-actuals>)
         (cons $1 $3)])

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

      (<opt-timeout>
        [()                     '()]
        [(COMMA <timeout>)      $2])

      (<timeout>
        [(TIME OP-LESS <expr>)
         (list '#:timeout $3)])

      (<op2>
        [(OP2)          $1]
        [(NOT)          'not])

      (<op3>
        [(OP3)          $1]
        [(OP-LESS)      $1]
        [(IN)           'in]
        [(IS)           'is]
        [(NOT-IN)       '∉]
        [(IS NOT)       '|is not|]
        [(NOT IN)       '|not in|])

      (<op8>
        [(OP8)          $1]
        [(PLUS)         '+]
        [(MINUS)        '-])

      (<op10>
        [(OP10)         $1]
        [(PLUS)         '+]
        [(MINUS)        '-])

      (<large-expression>
        [(TIME <expr> COLON <suite>)
         (loc/1 `(time ,$2 ,@$4))]
        [(TIME COLON <suite>)
         (loc/1 `(time ,(anonymous-block 'time $1-start-pos) ,@$3))]
        [(LAMBDA <formals> COLON <suite>)
         (loc/1 `(lambda ,$2 ,@$4))]
        [(IF <expr0> COLON <suite> <elifs> <maybe-else>)
         (loc/1 `(if [,$2 ,@$4] ,@$5 ,$6))])

      (<expr>
        [(TIME <expr> COLON <simple-statements>)
         (loc/1 `(time ,$2 ,@$4))]
        [(TIME COLON <simple-statements>)
         (loc/1 `(time ,(anonymous-block 'time $1-start-pos) ,@$3))]
        [(LAMBDA <formals> COLON <simple-statements>)
         (loc/1 `(lambda ,$2 ,@$4))]
        [(<expr0> IF <expr0> ELSE <expr>)
         (loc `(if-e ,$3 ,$1 ,$5))]
        [(<expr0>)
         $1])

      (<expr0>
        [(<expr0> OP0 <expr1>)
         (loc/2 `(,$2 ,$1 ,$3))]
        [(<expr1>)
         $1])

      (<expr1>
        [(<expr1> OP1 <expr2>)
         (loc/2 `(,$2 ,$1 ,$3))]
        [(<expr2>)
         $1])

      (<expr2>
        [(<op2> <expr2>)
         (loc/1 `(,$1 ,$2))]
        [(<expr3>)
          $1])

      (<expr3>
        [(<expr4> <op3> <expr4>)
         (loc/2 `(,$2 ,$1 ,$3))]
        [(<expr4>)
         $1])

      (<expr4>
        [(<expr4> OP4 <expr5>)
         (loc/2 `(,$2 ,$1 ,$3))]
        [(<expr5>)
         $1])

      (<expr5>
        [(<expr5> OP5 <expr6>)
         (loc/2 `(,$2 ,$1 ,$3))]
        [(<expr6>)
         $1])

      (<expr6>
        [(<expr6> OP6 <expr7>)
         (loc/2 `(,$2 ,$1 ,$3))]
        [(<expr7>)
         $1])

      (<expr7>
        [(<expr7> OP7 <expr8>)
         (loc/2 `(,$2 ,$1 ,$3))]
        [(<expr8>)
         $1])

      (<expr8>
        [(<expr8> <op8> <expr9>)
         (loc/2 `(,$2 ,$1 ,$3))]
        [(<expr9>)
         $1])

      (<expr9>
        [(<expr9> OP9 <expr10>)
         (loc/2 `(,$2 ,$1 ,$3))]
        [(<expr10>)
         $1])

      (<expr10>
        [(<op10> <expr10>)
         (loc/1 `(,$1 ,$2))]
        [(<expr11>)
         $1])

      (<expr11>
        [(<atom> OP11 <expr11>)
         (loc/2 `(,$2 ,$1 ,$3))]
        [(<atom>)
         $1]))))

(define (anonymous-block kind pos)
  (format "<~a@~a>" kind (position-line pos)))
