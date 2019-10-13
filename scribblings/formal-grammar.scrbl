#lang scribble/manual

@require["common.rkt"]

@title{Formal grammar}

The DSSL2 language has a number of statement and expression forms, which
are described in more depth below. Here they are summarized in
@hyperlink["https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form"]{
    Extended Backus-Naur Form}.

Non-terminal symbols are written in ⟨@emph{italic_with_pointies}⟩, whereas
terminal symbols are in @term[|colored typewriter|]. Conventions include:

@itemlist[
 @item{@~many{@term[x]} for repetition 0 or more times}
 @item{@~many1{@term[x]} for repetition 1 or more times}
 @item{@~many-comma{@term[x]} for repetition 0 or more times with commas in
 between}
 @item{@~opt{@term[x]} for optional}
]

The grammar begins by saying that a program is a sequence of zero or
more statements, where a statement is either a simple statement followed
by a newline, or a compound statement.

@grammar[
  [program   (~many statement)]
  [statement (simple 'NEWLINE)
             compound]
  [expr     'number
            'string
            True
            False
            None
            lvalue
            (expr "if" expr "else" expr)
            (expr "(" (~many-comma expr) ")")
            (lambda (~many-comma 'name) ":" simple)
            (λ (~many-comma 'name) ":" simple)
            ('struct_name "{" (~many-comma 'name ":" expr) "}")
            ("[" (~many-comma expr) "]")
            ("[" expr ";" expr "]")
            ("[" expr "for" (~opt 'name ",") 'name "in" expr (~opt "if" expr) "]")
            (expr 'binop expr)
            ('unop expr)]
  [simple    (assert expr opt_timeout)
             (assert_error expr (~opt "," expr) opt_timeout)
             break
             continue
             (lvalue = expr)
             expr
             (import mod_spec)
             (let 'name opt_ctc (~opt "=" expr))
             pass
             (return (~opt expr))
             (simple ";" simple)]
  [lvalue    'name
             (expr "." 'name)
             (expr "[" expr "]")]
  [compound  (class 'name opt_cvars opt_implements : class_block)
             (def 'name opt_cvars "(" (~many-comma 'name opt_ctc) ")"
               opt_res_ctc : block)
             (if expr ":" block
                 (~many elif expr ":" block)
                 (~opt else ":" block))
             (interface 'name opt_cvars ":" interface_block)
             (for (~opt 'name ",") 'name "in" expr ":" block)
             (struct 'name ":" struct_block)
             (test (~opt expr) ":" block)
             (time (~opt expr) ":" block)
             (while expr ":" block)]
  [block     (simple 'NEWLINE)
             ('NEWLINE 'INDENT (~many1 statement) 'DEDENT)]
  [class_block
             ('NEWLINE 'INDENT (~many field_def) (~many1 meth_proto ":" block) 'DEDENT)]
  ; [class_fields ]
  ; [class_methods ]
  [interface_block
            pass
            ('NEWLINE 'INDENT (~many1 meth_proto 'NEWLINE) 'DEDENT)]
  [struct_block
            pass
            ('NEWLINE 'INDENT (~many1 field_def) 'DEDENT)]
  [meth_proto
            (def 'name opt_cvars "(" 'name (~many "," 'name opt_ctc) ")" opt_res_ctc)]
  [field_def
            (let 'name opt_ctc 'NEWLINE)]
  [opt_timeout
            (~opt "," "time" "<" expr)]
  [opt_implements
            (~opt "(" (~many-comma 'name) ")")]
  [opt_ctc
            (~opt ":" ctc)]
  [opt_res_ctc
            (~opt "->" ctc)]
  [opt_cvars
            (~opt "[" (~many-comma 'name) "]")]
  [ctc      expr]
  [mod_spec 'mod_name
            'mod_string]
]

@t{binop}s are, from tightest to loosest precedence:

@itemlist[
 @item{@racket[**],}
 @item{@racket[*], @racket[/], and @racket[%],}
 @item{@racket[+] and @racket[-],}
 @item{@racket[>>] and @racket[<<],}
 @item{@racket[&],}
 @item{@racket[^],}
 @item{@racket[\|] (not written with the backslash),}
 @item{@racket[==], @racket[<], @racket[>], @racket[<=], @racket[>=],
 @racket[!=], @racket[is], and @racket[|is not|] (not written
 with the vertical bars),}
 @item{@racket[and], and}
 @item{@racket[or].}
]

@t{unop}s are @racket[~], @racket[+], @racket[-], and @racket[not].

