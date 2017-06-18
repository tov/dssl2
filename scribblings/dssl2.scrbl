#lang scribble/manual
@require["common.rkt" "std-grammar.rkt" "prim-ops.rkt"
         @for-label[dssl]]

@title{DSSL: Data Structures Student Language}
@author{Jesse A. Tov <jesse@"@"eecs.northwestern.edu>}

@defmodulelang[dssl]

The DSSL language is substantially similar to
@hyperlink["https://docs.racket-lang.org/htdp-langs/advanced.html"]{Advanced
Student Language}. In particular, it provides the same functions and
values (except for hash tables).

In addition to the special forms documented below, DSSL includes
@hyperlink["https://docs.racket-lang.org/reference/for.html"]{@racket[for]}
(and friends).

@racketgrammar*[
#:literals (define define-struct define-datatype lambda λ cond else if and or
            local let let* recur shared letrec time begin begin0 set!
            delay when case match unless while until
            _ cons list list* struct vector box
            check-expect check-random check-satisfied check-within
            check-member-of check-range check-error)
[program (code:line def-or-expr ...)]
[def-or-expr definition
             expr
             test-case]
[definition (define (name variable ...) expr)
            (define name expr)
            (define-struct name (name ...))]
[expr (code:line (expr expr ...))
      (begin expr expr ...)
      (begin0 expr expr ...)
      (lambda (variable ...) expr ...)
      (λ (variable ...) expr ...)
      (local [definition ...] expr ...)
      (let ([name expr] ...) expr ...)
      (let* ([name expr] ...) expr ...)
      (recur name ([name expr] ...) expr ...)
      (letrec ([name expr] ...) expr ...)
      (shared ([name expr] ...) expr ...)
      (set! name expr)
      (cond [expr expr ...] ... [expr expr ...])
      (cond [expr expr ...] ... [else expr ...])
      (case expr [(choice choice ...) expr ...] ...
                 [(choice choice ...) expr ...])
      (case expr [(choice choice ...) expr ...] ...
                 [else expr ...])
      (match expr [pattern expr ...] ...)
      (if expr expr expr)
      (when expr expr ...)
      (unless expr expr ...)
      (and expr expr expr ...)
      (or expr expr expr ...)
      (while expr expr ...)
      (until expr expr ...)
      for-loop
      (time expr ...)
      (delay expr)
      (code:line name)
      (code:line @#,elem{@racketvalfont{'}@racket[_quoted]})
      (code:line @#,elem{@racketvalfont{`}@racket[_quasiquoted]})
      (code:line @#,elem{@racketvalfont{'}@racket[()]})
      number
      boolean
      string
      character]
[choice (code:line name)
        number]
[pattern _
         name
         number
         true
         false
         string
         character
         @#,elem{@racketvalfont{'}@racket[_quoted]}
         @#,elem{@racketvalfont{`}@racket[_quasiquoted-pattern]}
         (cons pattern pattern)
         (list pattern ...)
         (list* pattern ...)
         (struct id (pattern ...))
         (vector pattern ...)
         (box pattern)]
[quasiquoted-pattern name
                     number
                     string
                     character
                     (quasiquoted-pattern ...)
                     @#,elem{@racketvalfont{'}@racket[_quasiquoted-pattern]}
                     @#,elem{@racketvalfont{`}@racket[_quasiquoted-pattern]}
                     @#,elem{@racketfont[","]@racket[_pattern]}
                     @#,elem{@racketfont[",@"]@racket[_pattern]}]
[test-case (check-expect expr expr)
           (check-error expr expr ...)
           (check-within expr expr expr)
           (check-random expr expr)
           (check-satisfied expr expr)
           (check-member-of expr expr ...)
           (check-range expr expr expr)]
]

@prim-nonterms[("dssl") define define-struct]

@prim-variables[("dssl") empty true false]

@section[#:tag "dssl-syntax"]{Syntax for DSSL}

@subsection[#:tag "def-forms"]{Definition Forms}

@defform[(define (name variable ...) expression ...)]{

Defines a function named @racket[name]. The @racket[expression]s are the
body of the function. When the function is called, the values of the
arguments are inserted into the body in place of the @racket[variable]s. The
function returns the value of the last expression in the sequence.

The function name’s cannot be the same as that of another function or
variable.}

@defform[#:link-target? #f (define name expression)]{

Defines a variable called @racket[name] with the the value of
@racket[expression]. The variable’s name cannot be the same as that of
another function or variable, and @racket[name] itself must not appear in
@racket[expression].}

@subsection[#:tag "expr-forms"]{Expression Forms}

@defform/none[(expression expression ...)]{

Calls the function that results from evaluating the first
@racket[expression]. The value of the call is the value of function's body when
every instance of @racket[name]'s variables are replaced by the values of the
corresponding @racket[expression]s.

The function being called must come from either a definition appearing
before th○e function call, or from a @racket[lambda] expression. The
number of argument @racket[expression]s must be the same as the number
of arguments expected by the function.}

@defform[(begin expression expression ...)]{

Evaluates the @racket[expression]s in order from left to right. The value of
the @racket[begin] expression is the value of the last @racket[expression].}


@defform[(begin0 expression expression ...)]{

Evaluates the @racket[expression]s in order from left to right. The value of
the @racket[begin] expression is the value of the first @racket[expression].}


@defform[(set! variable expression)]{

Evaluates @racket[expression], and then mutates the @racket[variable]
to have @racket[expression]'s value. The @racket[variable] must be defined 
by @racket[define], @racket[letrec], @racket[let*], or @racket[let].}


@defform[(lambda (variable ...) expression ...)]{

Creates a function that takes as many arguments as given @racket[variable]s,
and whose body is a sequence of @racket[expression]s. The result of the function is the result of the last @racket[expression].}

@defform[(λ (variable ...) expression ...)]{

The Greek letter @racket[λ] is a synonym for @racket[lambda].}

@defform[(case expression [(choice ...) expression ...]
                          ...
                          [(choice ...) expression ...])]{

A @racket[case] form contains one or more clauses. Each clause contains a
choices (in parentheses)---either numbers or names---and an answer
@racket[expression]. The initial @racket[expression] is evaluated, and its
value is compared to the choices in each clause, where the lines are considered
in order. The first line that contains a matching choice provides an answer
@racket[expression] whose value is the result of the whole @racket[case]
expression. Numbers match with the numbers in the choices, and symbols match
with the names. If none of the lines contains a matching choice, it is an
error.}

@defform/none[#:literals (case else)
              (case expression [(choice ...) expression ...]
                               ...
                               [else expression ...])]{

This form of @racket[case] is similar to the prior one, except that the final
@racket[else] clause is taken if no clause contains a choice matching the value
of the initial @racket[expression].}

@; ----------------------------------------------------------------------


@defform[(match expression [pattern expression ...] ...)]{

A @racket[match] form contains one or more clauses that are surrounded by
square brackets. Each clause contains a pattern---a description of a value---and
an answer @racket[expression].  The initial @racket[expression] is evaluated,
and its value is matched against the pattern in each clause, where the clauses are
considered in order. The first clause that contains a matching pattern provides
an answer @racket[expression] whose value is the result of the whole
@racket[match] expression. This @racket[expression] may reference identifiers
defined in the matching pattern. If none of the clauses contains a matching
pattern, it is an error.}

@; ----------------------------------------------------------------------

@defform[(when test-expression body-expression ...)]{

If @racket[test-expression] evaluates to @racket[true], the
@racket[body-expression]s are evaluated in order, an the result is the result
of the last @racket[body-expression]. Otherwise the result is @racket[(void)]
and the @racket[body-expression] is not evaluated. If the result of evaluating
the @racket[test-expression] is neither @racket[true] nor @racket[false], it is
an error.}

@defform[(unless test-expression body-expression ...)]{

Like @racket[when], but the @racket[body-expression]s are evaluated when the
@racket[test-expression] produces @racket[false] instead of @racket[true].}

@defform[(delay expression)]{

Produces a “promise” to evaluate @racket[expression]. The
@racket[expression] is not evaluated until the promise is forced with
@racket[force]; when the promise is forced, the result is recorded, so
that any further @racket[force] of the promise immediately produces the
remembered value.}

@(intermediate-forms lambda
                     local
                     letrec
                     let*
                     let
                     time
                     define
                     define-struct)

@defform[(shared ([name expr-for-shared] ...) expression)]{

Like @racket[letrec], but when an @racket[expression] next to an @racket[id]
is a @racket[cons], @racket[list], @racket[vector], quasiquoted
expression, or @racketidfont{make-}@racket[_struct-name] from a
@racket[define-struct], the @racket[expression] can refer directly to any
@racket[name], not just @racket[name]s defined earlier. Thus,
@racket[shared] can be used to create cyclic data structures.}

@defform[(recur loop-name ([name expr-for-recur] ...) expression)]{

A short-hand syntax for recursive loops. The first @racket[name] corresponds to
the name of the recursive function. The @racket[name]s in the parenthesis are
the function's arguments, and each corresponding @racket[expression] is a
value supplied for that argument in an initial starting call of the
function. The last @racket[expression] is the body of the function.

More precisely, the following @racket[recur]:·

@racketblock[
(recur func-name ([arg-name arg-expression] (unsyntax @racketidfont{...}))
  body-expression)
]

is equivalent to:

@racketblock[
(local [(define (func-name arg-name (unsyntax @racketidfont{...})) body-expressi○on)]
  (func-name arg-expression (unsyntax @racketidfont{...})))
]}

@defform[(while test-expression body-expression ...)]{

If @racket[test-expression] evaluates to @racket[true], the
@racket[body-expression]s are evaluated in order, and then the loop starts over
by testing @racket[test-expression] again. When @racket[test-expression] is
false, the loop terminates and returns @racket[(void)]. If the result of
evaluating the @racket[test-expression] is neither @racket[true] nor
@racket[false], it is an error.}

@defform[(until test-expression body-expression ...)]{

Like @racket[while], except the loop continutes so long as
@racket[test-expression] evalutes to false.}

@(prim-forms ("dssl")
             define
             lambda
             define-struct
             @{In DSSL, @racket[define-struct] introduces one additional function:
              @itemize[
               @item{@racketidfont{set-}@racket[_structure-name]@racketidfont{-}@racket[_field-name]@racketidfont{!}
                : takes an instance of the structure and a value, and
                mutates the instance's field to the given value.}]}
             define-wish
             cond
             else
             if
             and 
             or
             check-expect
             check-random
	     check-satisfied
             check-within
             check-error
             check-member-of
             check-range
             require
             true false
             #:with-beginner-function-call #f)

@section[#:tag "dssl-pre-defined"]{Pre-Defined Functions}

The remaining subsections list those functions that are built into the
programming language. All other functions must be defined in the program.

@(require (submod lang/htdp-advanced procedures))
@(render-sections (docs) #'here "htdp-advanced")

