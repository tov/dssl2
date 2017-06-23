#lang scribble/manual

@(require
        "util.rkt"
        (for-label dssl2))

@title{DSSL2: Data Structures Student Language}
@author{Jesse A. Tov <jesse@"@"eecs.northwestern.edu>}

@defmodulelang[dssl2]

@section[#:tag "dssl-syntax"]{Syntax of DSSL2}

@subsection{Compound statements and blocks}

DSSL2 uses alignment and indentation to delimit blocks. In particular,
compound statements such as @racket[if]-@racket[elif]-@racket[else] take
@syn[block]s for each condition, where a @syn[block] can be either one
simple statement followed by a newline, or a sequence of statements on
subsequent lines that are all indented by four additional spaces. Here
is an example of a decision tree function written using indentation:

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

@subsection{DSSL2 Formal Grammar}

The DSSL2 language has a number of statement and expression forms, which
are described in more depth below. Here they are summarized in
@hyperlink["https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form"]{
    Extended Backus-Naur Form}.

Non-terminal symbols are written in @syn{italic typewriter}, whereas
terminal symbols are in @q{colored typewriter}. Conventions include:

@itemlist[
 @item{@m["{"] @syn[x] @m["}*"] for repetition 0 or more times}
 @item{@m["{"] @syn[x] @m["}⁺"] for repetition 1 or more times}
 @item{@m["{"] @syn[x] @m["},*"] for repetition 0 or more times with commas in
 between}
 @item{@m["["] @syn[x] @m["]"] for optional}
]

The grammar begins by saying that a program is a sequence of zero or
more statements, where a statement is either a simple statement followed
by a newline, or a compound statement.

@racketgrammar*[
#:literals (def defstruct let lambda λ else if elif while for in
            break continue : True False =
            assert assert_eq pass return NEWLINE INDENT DEDENT)
[program (code:line @#,m["{"] statement @#,m["}*"])]
[statement   (code:line simple @#,q{NEWLINE})
             compound]
[simple
            (code:line assert expr)
            (code:line assert_eq expr @#,q{,} expr)
            break
            continue
            (code:line defstruct name @#,q{(} @#,m["{"] field @#,m["},*"] @#,q{)})
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
            (code:line def name @#,q{(} var @#,q{,} @#,m{...} @#,q{)} @#,q{:} block)
            (code:line if expr @#,q{:} block @#,m["{"] elif expr @#,q{:} block @#,m["}*"] @#,m["["] else expr @#,q{:} block @#,m["]"])
            (code:line for var @#,m{[} @#,q{,} var @#,m{]} @#,q{in} expr @#,q{:} block)
            (code:line while expr @#,q{:} block)
            ]
[block
        (code:line simple @#,q{NEWLINE})
        (code:line @#,q{NEWLINE} @#,q{INDENT} @#,m["{"] statement @#,m["}⁺"] @#,q{DEDENT})]
[expr lvalue
      number
      string
      True
      False
      (code:line expr @#,q{(} @#,m["{"] expr @#,m["},*"] @#,q{)})
      (code:line lambda @#,m["{"] var @#,m["},*"] @#,q{:} expr)
      (code:line @#,q{λ} @#,m["{"] var @#,m["},*"] @#,q{:} expr)
      (code:line expr @#,q{if} expr @#,q{else} expr)
      (code:line structname @#,q["{"] @#,m["{"] fieldname : expr @#,m["},*"] @#,q[" }"])
      (code:line @#,q{[} @#,m["{"] expr @#,m["},*"] @#,q{]})
      (code:line @#,q{[} expr @#,q{;} expr @#,q{]})
      (code:line @#,q{[} expr @#,q{for} var @#,m{[} @#,q{,} var @#,m{]} @#,q{in} expr @#,m{[} @#,q{if} expr @#,m{]} @#,q{]})
      (code:line expr BINOP expr)
      (code:line UNOP expr)
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

@subsection[#:tag "stm-forms"]{Statement Forms}

@defsmplform{@defidform/inline[assert] @syn[expr]}

Asserts that the given @syn[expr] evaluates to non-false. If the
expression evaluates false, signals an error.

@defsmplform{@defidform/inline[assert_eq] @syn[expr], @syn[expr]}

Asserts that the given @syn[expr]s evaluates to physically equal values.
If they are not equal, signals an error.

@defsmplform{@defidform/inline[break]}

When in a @racket[for] or @racket[while] loop, ends the (inner-most)
loop immediately.

@defsmplform{@defidform/inline[continue]}

When in a @racket[for] or @racket[while] loop, ends the current
iteration of the (inner-most) loop and begins the next iteration.

@defcmpdform{@defidform/inline[def] @syn[name](@syn[var], ...): @syn[block]}

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

@defsmplform{@defidform/inline[defstruct] @syn[structname](@syn[fieldname], ...)}

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

@defsmplform{@syn[lvalue] @defidform/inline[=] @syn[expr]}

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

@defsmplform{@syn[expr]}

An expression, evaluated for both side effect and, if at the tail end
of a function, its value.

@defcmpdform{@defidform/inline[if] @syn[expr]: @syn[block]
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

@defsmplform{@defidform/inline[let] @syn[var] = @syn[expr]}

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

@defsmplform{@defidform/inline[let] @syn[var]}

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

@defcmpdform{@defidform/inline[for] @syn[var] in @syn[expr]: @syn[block]}

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

@defcmpdform{@defidform/inline[for] @syn[var]@subscript{1}, @syn[var]@subscript{2} in @syn[expr]: @syn[block]}

Loops over the indices and values of the given @syn[expr], evaluating
the @syn[block] for each. The @syn[expr] can evaluate to a vector, a
string, or a natural number. If a vector, then @syn[var]@subscript{1}
takes on the indices of the vector while @syn[var]@subscript{2} takes on
the values; if a string, then @syn[var]@subscript{1} takes on the
indices of the characters while @syn[var]@subscript{2} takes on the
characters; if a natural number then both variables count together.

@verbatim[#:indent 4 #<<END
for ix, person in people_to_greet:
    println("~a: Hello, ~a!", ix, person)
END
]

@defsmplform{@defidform/inline[pass]}

Does nothing.

@defsmplform{@defidform/inline[return] @syn[expr]}

Returns the value of the given @syn[expr] from the inner-most function.
Note that this is often optional, since the last expression in a
function will be used as its return value.

That is, these are equivalent:

@verbatim[#:indent 4 #<<END
def inc(x): x + 1

def inc(x): return x + 1
END
]

@defcmpdform{@defidform/inline[while] @syn[expr]: @syn[block]}

Iterates the @syn[block] while the @syn[expr] evaluates to true. For example:

@verbatim[#:indent 4 #<<END
while !is_empty(queue):
    explore(dequeue(queue))
END
]

@subsection[#:tag "exp-forms"]{Expression Forms}

@defexpform{@syn{lvalue}}

These left-hand sides of assignments (@syn[=]) can also appear for their
values. That is, @code{v[i]} gets the @code{i}th element of vector
@code{v}.

@defexpform{@syn{number}}

Numeric literals include:

@itemlist[
  @item{Integers: @racket[0], @racket[3], @racket[18446744073709551617]}
  @item{Hexadedecimal, octal, and binary integers: @q{0xFFFF00},
      @q{0o0177}, @q{0b011010010}}
  @item{Floating point: @racket[3.5], @q{6.02E23}, @racket[1e-12]}
]

@defexpform{@syn{string}}

String literals are delimited by either single or double quotes:

@verbatim[#:indent 4 #<<END
def does_not_matter(double)
    if double:
        return "This is the same string."
    else:
        return 'This is the same string.'
END
]

The contents of each kind of string is treated the same, except that
each kind of quotation mark can contain the other kind unescaped:

@verbatim[#:indent 4 #<<END
def does_matter(double)
    if double:
        return "This isn't the same string."
    else:
        return '"This is not the same string" isn\'t the same string.'
END
]

Strings cannot contain newlines directly, but can contain newline
characters via the escape code @code{\n}. Other escape codes include:

@itemlist[
  @item{@code{\a} for ASCII alert (also @code{\x07})}
  @item{@code{\b} for ASCII backspace (also @code{\x08})}
  @item{@code{\f} for ASCII formfeed (also @code{\x0C})}
  @item{@code{\n} for ASCII newline (also @code{\x0A})}
  @item{@code{\r} for ASCII carriage return (also @code{\x0D})}
  @item{@code{\t} for ASCII tab (also @\code{\x09})}
  @item{@code{\v} for ASCII vertical tab (also @\code{\x0B})}
  @item{@code{\x@syn{hh}} in hex, for example @code{\x0A} is newline}
  @item{@code{\@syn{ooo}} in octal, for example @code{\010} is tab}
]

A backslash immediately followed by a newline causes both characters to
be ignored, which provides a way to wrap long strings across lines.

@defexpform{@defidform/inline[True]}

The true Boolean value.

@defexpform{@defidform/inline[False]}

The false Boolean value, the only value that is not considered true.

@defexpform{@syn[expr]@subscript{0}(@syn[expr]@subscript{1}, ..., @syn[expr]@subscript{k})}

Evaluates all the expressions; then applies the result of
@syn[expr]@subscript{0} with the results of the other expressions as
arguments.

For example,

@verbatim[#:indent 4 #<<END
fact(5)
END
]

calls the function @racket[fact] with argument @racket[5], and

@verbatim[#:indent 4 #<<END
ack(5 + 1, 5 + 2)
END
]

calls the function @racket[ack] with arguments @racket[6] and
@racket[7].

@defexpform{@defidform/inline[lambda] @syn[var]@subscript{1}, ..., @syn[var]@subscript{k}: @syn[expr]}

Creates an anonymous function with parameters @syn[var], @code{...} and
body @syn[expr]. For example, the function to add twice its first
argument to its second argument can be written

@verbatim[#:indent 4 #<<END
lambda x, y: 2 * x + y
END
]

@defexpform{@defidform/inline[λ] @syn[var], ...: @syn[expr]}

The same as @code{lambda @syn[var], ...: @syn[expr]}.

@defexpform{@syn[expr]@subscript{1} if @syn[expr]@subscript{2} else @syn[expr]@subscript{3}}

The ternary expression first evaluates the condition
@syn[expr]@subscript{2}. If true (any value but @racket[False]),
evaluates @syn[expr]@subscript{1} for its value; otherwise, if
@syn[expr]@subscript{2} was false, evaluates @syn[expr]@subscript{3}
for its value.

@defexpform{@syn[structname] { @syn[field]@subscript{1}: @syn[expr]@subscript{1}, ..., @syn[field]@subscript{k}: @syn[expr]@subscript{k} }}

Constructs a struct with the given name and the values of the given
expressions for its fields. The struct must have been declared with
those fields using @racket[defstruct].

@defexpform{[ @syn[expr]@subscript{0}, ..., @syn[expr]@subscript{k - 1} ]}

Creates a new vector of length @code{k} whose values are the values
of the expressions.

For example:

@verbatim[#:indent 4 #<<END
let vec = [ 1, 2, 3, 4, 5 ]
END
]

@defexpform{[ @syn[expr]@subscript{1}; @syn[expr]@subscript{2} ]}

Constructs a new vector whose length is the value of
@syn[expr]₂, filled with the value of @syn[expr]₁. That is,

@verbatim[#:indent 4 #<<END
[ 0; 5 ]
END
]

means the same thing as

@verbatim[#:indent 4 #<<END
[ 0, 0, 0, 0, 0 ]
END
]

@defexpform{[ @syn[expr]₁ for @syn[var] in @syn[expr]₂ ]}

@defexpform{[ @syn[expr]₁ for @syn[var]₁, @syn[var]₂ in @syn[expr]₂ ]}

@defexpform{[ @syn[expr]₁ for @syn[var] in @syn[expr]₂ if @syn[expr]₃ ]}

@defexpform{[ @syn[expr]₁ for @syn[var]₁, @syn[var]₂ in @syn[expr]₂ if @syn[expr]₃ ]}

@subsubsection{Operators}

@defexpform{@syn[expr]@subscript{1} @defidform/inline[**] @syn[expr]@subscript{2}}

Raises the value of @syn[expr]@subscript{1} to the power of the value of
@syn[expr]@subscript{2}, both of which must be numbers.

@defexpform{@syn[expr]@subscript{1} @defidform/inline[*] @syn[expr]@subscript{2}}

Multiplies the values of the expressions, which must be numbers.
