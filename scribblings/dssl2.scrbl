#lang scribble/manual

@(require
        "util.rkt"
        (for-label dssl2))

@title{DSSL2: Data Structures Student Language 2}
@author{Jesse A. Tov <jesse@"@"eecs.northwestern.edu>}

@defmodulelang[dssl2]

The DSSL2 language has the following statement and expression forms:

@racketgrammar*[
#:literals (def defstruct let lambda λ else if elif while for in
            break continue : True False =
            assert assert_eq pass return NEWLINE INDENT DEDENT)
[program (code:line statement ...)]
[statement   (code:line simple NEWLINE)
             compound]
[simple
            (code:line assert expr)
            (code:line assert_eq expr @#,q{,} expr)
            break
            continue
            (code:line defstruct name ( field @#,q{,} ... @#,q{ }))
            (code:line lvalue = expr)
            expr
            (code:line let var @#,q{=} expr)
            (code:line let var)
            (code:line pass)
            (code:line return expr)
            (code:line simple @#,q{;} simple)]
[lvalue var
        (code:line expr @#,q{.} field)
        (code:line expr @#,q["["] expr @#,q["]"])]
[compound
            (code:line def name ( var @#,q{,} ... @#,q{ }) : block)
            (code:line if expr : block @#,m["{"] elif expr : block @#,m["}*"] @#,m["["] else expr : block @#,m["]"])
            (code:line for var in expr : block)
            (code:line for var @#,q{,} var in expr : block)
            (code:line while expr : block)
            ]
[block
        (code:line simple NEWLINE)
        (code:line NEWLINE INDENT statement ... DEDENT)]
[expr lvalue
      number
      string
      True
      False
      (code:line expr ( expr @#,q{,} ... @#,q{ }))
      (code:line lambda var @#,q{,} ... : expr)
      (code:line λ var @#,q{,} ... : expr)
      (code:line expr if expr else expr)
      (code:line expr BINOP expr)
      (code:line UNOP expr)
      (code:line structname @#,q["{"] fieldname : expr @#,q{,} ... @#,q[" }"])
      (code:line @#,q{[} expr @#,q{,} ... @#,q{]})
      (code:line @#,q{[} expr @#,q{;} expr @#,q{]})
      (code:line @#,q{[} expr for var in expr @#,q{]})
      (code:line @#,q{[} expr for var @#,q{,} var in expr @#,q{]})
      (code:line @#,q{[} expr for var in expr if expr @#,q{]})
      (code:line @#,q{[} expr for var @#,q{,} var in expr if expr @#,q{]})
      ]
]

@italic{BINOP}s are, from tightest to loosest precedence:

@itemlist[
 @item{@racket[**]}
 @item{@racket[*], @racket[/], and @racket[%]}
 @item{@racket[+] and @racket[-]}
 @item{@racket[>>] and @racket[<<]}
 @item{@racket[&]}
 @item{@racket[^]}
 @item{@racket[\|] (not written with the backslash)}
 @item{@racket[==], @racket[<], @racket[>], @racket[<=], @racket[>=],
 @racket[!=], @racket[===], and @racket[!==]}
 @item{@racket[&&]}
 @item{@racket[\|\|] (not written with the backslashes)}
]

@italic{UNOP}s are @racket[!], @racket[+], @racket[-].

@section[#:tag "dssl-syntax"]{Syntax for DSSL}

@subsection{Compound statements and blocks}

DSSL2 uses alignment and indentation to delimit blocks. In particular,
compound statements such as @racket[if]-@racket[elif]-@racket[else] take
@syn[block]s for each condition, where a @syn[block] can be either one
simple statement followed by a newline, or a sequence of statements on
subsequent lines that are all indented by four additional spaces. Here
is an example of a decision tree written using indentation:

@verbatim[#:indent 4 #<<END
def organization_discount(otype, osize):
    if otype === BOOKSTORE:
        if osize >= 50:
            return 0.25
        else:
            return 0
    else if otype === LIBRARY:
        if osize >= 50:
            return 0.15
        elif osize >= 20:
            return 0.1
        elif osize >= 5:
            return 0.05
        else:
            return 0
    else:
        return 0
END
]

Each block follows a colon and newline, and is indented 4 spaces more
than the previous line. (Extranous space is an error.)

@subsection[#:tag "stm-forms"]{Statement Forms}

@defdsslform{@defidform/inline[assert] @syn[expr]}

Asserts that the given @syn[expr] evaluates to non-false. If the
expression evaluates false, signals an error.

@defdsslform{@defidform/inline[assert_eq] @syn[expr], @syn[expr]}

Asserts that the given @syn[expr]s evaluates to physically equal values.
If they are not equal, signals an error.

@defdsslform{@defidform/inline[break]}

When in a @racket[for] or @racket[while] loop, ends the (inner-most)
loop immediately.

@defdsslform{@defidform/inline[continue]}

When in a @racket[for] or @racket[while] loop, ends the current
iteration of the (inner-most) loop and begins the next iteration.

@defdsslform{@defidform/inline[def] @syn[name](@syn[var], ...): @syn[block]}

Defines @syn[name] to be a function with formal parameters @syn[var],
@code{...}, and with body @syn[block].

For example,

@verbatim[#:indent 4 #<<END
def fact(n):
    if n < 2:
        return 1
    else:
        return n * fact(n - 1)
END
]

A function may have zero arguments, as in @racket[greet]:

@verbatim[#:indent 4 #<<END
def greet(): println("Hello, world!")
END
]

The body of a function is defined to be a block, which means it can be
an indented sequence of statements, or a single simple statement on the
same line as the @racket[def].

@defdsslform{@defidform/inline[defstruct] @syn[structname](@syn[fieldname], ...)}

Defines a new structure type @syn[structname] with fields given by
@syn[fieldname], @code{...}. For example, to define a struct
@racket[posn] with fields @racket[x] and @racket[y], we write:

@verbatim[#:indent 4 #<<END
defstruct posn(x, y)
END
]

Then we can create a @racket[posn] using struct construction syntax and
select out the fields using dotted selection syntax:

@verbatim[#:indent 4 #<<END
let p = posn { x: 3, y: 4 }

def magnitude(q):
    sqrt(q.x * q.x + q.y * q.y)
END
]

It also possible to construct the struct by giving the fields in order
using function syntax:

@verbatim[#:indent 4 #<<END
assert_eq magnitude(posn(3, 4)), 5
END
]

@defdsslform{@syn[lvalue] @defidform/inline[=] @syn[expr]}

Assignment. The assigned @syn[lvalue] can be in one of three forms:

@itemlist[
 @item{@syn[var] assigns to a variable, which can be a @syn[let]-bound
 local or a function parameter.}
 @item{@syn[expr].@syn[fieldname] assigns to a structure field, where
 the expression must evaluate to a structure that has the given field
 nane.}
 @item{@code{@syn[expr][@syn[expr]]} assigns to a vector, where the first
 @syn[expr] evaluates to the vector and the second @syn[expr] evaluates
 to the index.}
]

@defdsslform{@syn[expr]}

An expression, evaluated for both side effect and, if at the tail end
of a function, its value.

@defdsslform{@defidform/inline[if] @syn[expr]: @syn[block]
             @defidform/inline[elif] @syn[expr]: @syn[block]
             @defidform/inline[else]: @syn[block]}

The DSSL2 conditional statement contains an @racket[if], 0 or more
@racket[elif]s, and optionally an @racket[else] for if none of the
conditions holds.

For example, we can have an @racket[if] with no @racket[elif] or
@racket[else] parts:

@verbatim[#:indent 4 #<<END
if should_greet:
    greet()
END
]

Or we can have several:

@verbatim[#:indent 4 #<<END
def fib(n):
    if n == 0:
        0
    elif n == 1:
        1
    elif n == 2:
        1
    elif n == 3:
        2
    else:
        fib(n - 1) + fib(n - 2)
END
]

@defdsslform{@defidform/inline[let] @syn[var] = @syn[expr]}

Declares and defines a local variable. Local variables may be declared in any
scope and last for that scope. A local variable may be re-assigned with the
assignment form, as in th third line here:

@verbatim[#:indent 4 #<<END
def sum(vec):
    let result = 0
    for v in vec: result = result + v
    return result
END
]

@defdsslform{@defidform/inline[let] @syn[var]}

Declares a local variable, which will be undefined until it is assigned:

@verbatim[#:indent 4 #<<END
let x
if y:
    x = f()
else:
    x = g()
println(x)
END
]

@defdsslform{@defidform/inline[for] @syn[var] in @syn[expr]: @syn[block]}

Loops over the values of the given @syn[expr], evaluating the
@syn[block] for each. The @syn[expr] can evaluate to a vector, a string,
or a natural number. If a vector, then this form iterates over the
values (not the indices) of the vector; if a string, this iterates over
the characters as 1-character strings; if a natural number @racket[n]
then it counts from @racket[0] to @racket[n - 1].

@verbatim[#:indent 4 #<<END
for person in people_to_greet:
    println("Hello, ~a!", person)
END
]

@defdsslform{@defidform/inline[for] @syn[var], @syn[var] in @syn[expr]: @syn[block]}

Loops over the indices and values of the given @syn[expr], evaluating
the @syn[block] for each. The @syn[expr] can evaluate to a vector, a
string, or a natural number. If a vector, then the first variable takes
on the indices of the vector while the second takes on the values; if a
string, then the first variable takes on the indices of the characters
while the second takes on the characters; if a natural number then both
variables count together.

@verbatim[#:indent 4 #<<END
for ix, person in people_to_greet:
    println("~a: Hello, ~a!", ix, person)
END
]

@defdsslform{@defidform/inline[pass]}

Does nothing.

@defdsslform{@defidform/inline[return] @syn[expr]}

Returns the value of the given @syn[expr] from the inner-most function.
Note that this is often optional, since the last expression in a
function will be used as its return value.

That is, these are equivalent:

@verbatim[#:indent 4 #<<END
def inc(x): x + 1

def inc(x): return x + 1
END
]

@defdsslform{@defidform/inline[while] @syn[expr]: @syn[block]}

Iterates the @syn[block] while the @syn[expr] evaluates to true. For example:

@verbatim[#:indent 4 #<<END
while !is_empty(queue):
    explore(dequeue(queue))
END
]

