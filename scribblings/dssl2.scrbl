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
 @item{@racket[and]}
 @item{@racket[or] (not written with the backslashes)}
]

@italic{UNOP}s are @racket[!], @racket[~], @racket[+], @racket[-].

@subsection[#:tag "stm-forms"]{Statement Forms}

@defsmplform{@defidform/inline[assert] @syn[expr]}

Asserts that the given @syn[expr] evaluates to non-false. If the
expression evaluates false, signals an error.

@defsmplform{@defidform/inline[assert_eq] @syn[expr]₁, @syn[expr]₂}

Asserts that the given @syn[expr]s evaluates to structurally equal values.
If they are not equal, signals an error.

@defsmplform{@defidform/inline[break]}

When in a @racket[for] or @racket[while] loop, ends the (inner-most)
loop immediately.

@defsmplform{@defidform/inline[continue]}

When in a @racket[for] or @racket[while] loop, ends the current
iteration of the (inner-most) loop and begins the next iteration.

@defcmpdform{@defidform/inline[def] @syn[name](@syn[var]₁, ... @syn[var]@subscript{k}): @syn[block]}

Defines @syn[name] to be a function with formal parameters @syn[var]₁,
@code{...}, @syn[var]@subscript{k} and with body @syn[block].

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

@defsmplform{@defidform/inline[defstruct] @syn[structname](@syn[fieldname]₁, ..., @syn[fieldname]@subscript{k})}

Defines a new structure type @syn[structname] with fields given by
@syn[fieldname]₁, @code{...}, @syn[fieldname]@subscript{k}. For example,
to define a struct @racket[posn] with fields @racket[x] and @racket[y],
we write:

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

@defcmpdform{@defidform/inline[if] @syn[expr]@subscript{if}: @syn[block]@subscript{if}
             @defidform/inline[elif] @syn[expr]@subscript{i}: @syn[block]@subscript{i}
             @defidform/inline[else]: @syn[block]@subscript{else}}

The DSSL2 conditional statement contains an @racket[if], 0 or more
@racket[elif]s, and optionally an @racket[else] for if none of the
conditions holds.

First it evaluates the @racket[if] condition @syn[expr]@subscript{if}.
If non-false, it then evaluates @syn[block]@subscript{if} and finishes.
Otherwise, it evaluates each @racket[elif] condition
@syn[expr]@subscript{i} in turn; if each is false, it goes on to the
next, but when one is non-false then it finishes with the corresponding
@syn[block]@subscript{i}. Otherwise, if all of the conditions were false
and the optional @syn[block]@subscript{else} is included, evaluates
that.

For example, we can have an @racket[if] with no @racket[elif] or
@racket[else] parts:

@verbatim[#:indent 4 #<<END
if should_greet:
    greet()
END
]

The function @code{greet()} will be called if variable
@code{should_greet} is strue, and otherwise it will not.

Or we can have several @racket[elif] parts:

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

In this example, the recursive @racket[else] case happens when all four
conditions evaluate to false.

@defsmplform{@defidform/inline[let] @syn[var] = @syn[expr]}

Declares and defines a local variable. Local variables may be declared in any
scope and last for that scope. A local variable may be re-assigned with the
assignment form (@racket[=]), as in the third line here:

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

@defcmpdform{@defidform/inline[for] @syn[var] @q{in} @syn[expr]: @syn[block]}

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

@defcmpdform{@defidform/inline[for] @syn[var]₁, @syn[var]₂ @q{in} @syn[expr]: @syn[block]}

Loops over the indices and values of the given @syn[expr], evaluating
the @syn[block] for each. The @syn[expr] can evaluate to a vector, a
string, or a natural number. If a vector, then @syn[var]₁
takes on the indices of the vector while @syn[var]₂ takes on
the values; if a string, then @syn[var]₁ takes on the
indices of the characters while @syn[var]₂ takes on the
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

Iterates the @syn[block] while the @syn[expr] evaluates to non-false. For example:

@verbatim[#:indent 4 #<<END
while !is_empty(queue):
    explore(dequeue(queue))
END
]

@subsection[#:tag "exp-forms"]{Expression Forms}

@defexpform{@syn[var]}

The value of a variable, which must be a function parameter, bound with
@racket[let], or defined with @racket[def]. For example,

@verbatim[#:indent 4 #<<END
let x = 5
println(x)
END
]

prints “@code{5}”.

Lexically, a variable is a letter or underscore, followed by zero or
more letters, underscores, or digits, optionally ending in a question
mark or exclamation point.

@defexpform{@syn[expr].@syn[fieldname]}

Expression @syn[expr] must evaluate to struct value that has field
@syn[fieldname]; then this expression evaluates to the value of that
field of the struct.

@defexpform{@syn[expr]₁[@syn[expr]₂]}

Expression @syn[expr]₁ must evaluate to a vector @code{v}; @syn[expr]₂
must evaluate to an integer @code{n} between 0 and @code{len(v) - 1}.
Then this returns the @code{n}th element of vector @code{v}.

@defexpform{@syn{number}}

Numeric literals include:

@itemlist[
  @item{Decimal integers: @racket[0], @racket[3], @racket[18446744073709551617]}
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
  @item{A backslash immediately followed by a newline causes both characters to
      be ignored, which provides a way to wrap long strings across lines.}
]

Any other character following a backslash stands for itself.

@defexpform{@defidform/inline[True]}

The true Boolean value.

@defexpform{@defidform/inline[False]}

The false Boolean value, the only value that is not considered true.

@defexpform{@syn[expr]@subscript{0}(@syn[expr]₁, ..., @syn[expr]@subscript{k})}

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

@defexpforms[
  @list{@defidform/inline[lambda] @syn[var]₁, ..., @syn[var]@subscript{k}: @syn[expr]}
  @list{@q{λ} @syn[var]₁, ..., @syn[var]@subscript{k}: @syn[expr]}
]

Creates an anonymous function with parameters @syn[var]₁, @code{...},
@syn[var]@subscript{k} and body @syn[expr]. For example, the function to
add twice its first argument to its second argument can be written

@verbatim[#:indent 4 #<<END
lambda x, y: 2 * x + y
END
]

@defexpform{@syn[expr]₁ @q{if} @syn[expr]₂ @q{else} @syn[expr]₃}

The ternary expression first evaluates the condition
@syn[expr]₂. If non-false,
evaluates @syn[expr]₁ for its value; otherwise,
evaluates @syn[expr]₃ for its value.

@defexpform{@syn[structname] { @syn[field]₁: @syn[expr]₁, ..., @syn[field]@subscript{k}: @syn[expr]@subscript{k} }}

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

@defexpform{[ @syn[expr]₁; @syn[expr]₂ ]}

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

@defexpforms[
  @list{[ @syn[expr]₁ @q{for} @syn[var] @q{in} @syn[expr]₂ ]}
  @list{[ @syn[expr]₁ @q{for} @syn[var]₁, @syn[var]₂ @q{in} @syn[expr]₂ ]}
]

Vector comprehensions: produces a vector of the values of @syn[expr]₁
while iterating the variable(s) over @syn[expr]₂. In particular,
@syn[expr]₂ must be a vector @code{v}, a string @code{s}, or a
natural number @code{n}; in which case the iterated-over values are
the elements of @code{v}, the 1-character strings comprising
@code{s}, or counting from 0 to @code{n - 1}, respectively. If one
variable @syn[var] is provided, it takes on those values. If two are
provided, then @syn[var]₂ takes on those values, while @syn[var]₁
takes on the indices counting from 0 upward.

For example,

@verbatim[#:indent 4 #<<END
[ 10 * n for n in [ 5, 4, 3, 2, 1 ] ]
END
]

evaluates to

@verbatim[#:indent 4 #<<END
[ 50, 40, 30, 20, 10 ]
END
]

And

@verbatim[#:indent 4 #<<END
[ 10 * n + i for i, n in [ 5, 4, 3, 2, 1 ] ]
END
]

evaluates to

@verbatim[#:indent 4 #<<END
[ 50, 41, 32, 23, 14 ]
END
]

@defexpforms[
  @list{[ @syn[expr]₁ @q{for} @syn[var] @q{in} @syn[expr]₂ @q{if} @syn[expr]₃ ]}
  @list{[ @syn[expr]₁ @q{for} @syn[var]₁, @syn[var]₂ @q{in} @syn[expr]₂ @q{if} @syn[expr]₃ ]}
]

If the optional @syn[expr]₃ is provided, only elements for which
@syn[expr]₃ is non-false are included. That is, the variable(s) take on
each of their values, then @syn[expr]₃ is evaluated in the scope of the
variable(s). If it's non-false then @syn[expr]₁ is evaluated and
included in the resulting vector.

For example,

@verbatim[#:indent 4 #<<END
[ 10 * n for n in [ 5, 4, 3, 2, 1 ] if odd?(n) ]
END
]

evaluates to

@verbatim[#:indent 4 #<<END
[ 50, 30, 10 ]
END
]

@subsubsection{Operators}

Operators are described in order from tighest to loosest precedence.

@defexpform{@syn[expr]₁ @defidform/inline[**] @syn[expr]₂}

Raises the value of @syn[expr]₁ to the power of the value of
@syn[expr]₂, both of which must be numbers.

The @racket[**] operator is right-associative.

@defexpforms[
  @list{@defidform/inline[!]@syn[expr]}
  @list{@defidform/inline[~]@syn[expr]}
  @list{-@syn[expr]}
  @list{+@syn[expr]}
]

Logical negation, bitwise negation, numerical negation, and numerical identity.

@code{!}@syn[expr] evaluates @syn[expr], then returns @racket[True] if
the result was @racket[False], and @racket[False] for any other result.

@code{~}@syn[expr], @code{-}@syn[expr], and @code{+}@syn[expr] require
that @syn[expr] evaluate to a number. Then @code{~} flips every bit,
@code{-} negates it, and @code{+} returns it unchanged.

@defexpforms[
  @list{@syn[expr]₁ @defidform/inline[*] @syn[expr]₂}
  @list{@syn[expr]₁ @defidform/inline[/] @syn[expr]₂}
  @list{@syn[expr]₁ @defidform/inline[%] @syn[expr]₂}
]

Multiplies, divides, or modulos the values of the expressions, respectively.

@defexpforms[
  @list{@syn[expr]₁ @defidform/inline[+] @syn[expr]₂}
  @list{@syn[expr]₁ @defidform/inline[-] @syn[expr]₂}
]

Addition and subtraction.

@defexpforms[
  @list{@syn[expr]₁ @defidform/inline[<<] @syn[expr]₂}
  @list{@syn[expr]₁ @defidform/inline[>>] @syn[expr]₂}
]

Left and right bitwise shift.

@defexpform{@syn[expr]₁ @defidform/inline[&] @syn[expr]₂}

Bitwise and.

@defexpform{@syn[expr]₁ @defidform/inline[^] @syn[expr]₂}

Bitwise xor.

@defexpform{@syn[expr]₁ @defidform/inline[\|] @syn[expr]₂}

Bitwise or. (Not written with the backslash.)

@defexpforms[
  @list{@syn[expr]₁ @defidform/inline[==] @syn[expr]₂}
  @list{@syn[expr]₁ @defidform/inline[!=] @syn[expr]₂}
  @list{@syn[expr]₁ @defidform/inline[===] @syn[expr]₂}
  @list{@syn[expr]₁ @defidform/inline[!==] @syn[expr]₂}
  @list{@syn[expr]₁ @defidform/inline[<] @syn[expr]₂}
  @list{@syn[expr]₁ @defidform/inline[<=] @syn[expr]₂}
  @list{@syn[expr]₁ @defidform/inline[>] @syn[expr]₂}
  @list{@syn[expr]₁ @defidform/inline[>=] @syn[expr]₂}
]

Operator @racket[==] is structural equality, and @racket[!=] is its
negation. Operator @racket[===] is physical equality, and @racket[!==]
is its negation. To understand the difference, suppose that we create
two different vectors with the same contents. Those vectors are
structurally equal but not physically equal.

Operators @racket[<], @racket[<=], @racket[>], and @racket[>=] are the
standard inequalities for numbers, and compare pairs of strings in
lexicographic order.

@defexpform{@syn[expr]₁ @defidform/inline[and] @syn[expr]₂}

Short-circuiting logical and. First evaluates @syn[expr]₁; if the result
is @racket[False] then the whole conjunction is @racket[False];
otherwise, the result of the conjunction is the result of @syn[expr]₂.

@defexpform{@syn[expr]₁ @defidform/inline[or] @syn[expr]₂}

Short-circuiting logical or. First evaluates @syn[expr]₁; if the result
is non-false then the whole disjunction has that result; otherwise the
result of the conjunction is the result of @syn[expr]₂.

@section{Built-in functions and values}

@subsection{Type predicates}

@defprocform[procedure?]{(Any) -> Boolean}

Determines whether its argument is a procedure (function).

@defprocform[string?]{(Any) -> Boolean}

Determines whether its argument is a string.

@defprocform[number?]{(Any) -> Boolean}

Determines whether its argument is a number.

@defprocform[integer?]{(Any) -> Boolean}

Determines whether its argument is an integer.

@defprocform[vector?]{(Any) -> Boolean}

Determines whether its argument is a vector.

@subsection{Numeric operations}

@defprocform[floor]{(Number) -> Integer}

Rounds a number down to the largest integer that is no greater.

@defprocform[ceiling]{(Number) -> Integer}

Rounds a number up to the smallest integer that is no less.

@defprocforms[
    [int @list{(Number) -> Integer}]
    [int @list{(String) -> Integer}]
    [int @list{(Boolean) -> Integer}]
]

Returns the integer part of a number, by truncation. That is, the
decimal point and everything after it is removed. If given a string,
attempt to convert to a number before truncating, throwing an error if
the conversion fails. Booleans @racket[True] and @racket[False] conver
to @racket[1] and @racket[0], respectively.

@defprocforms[
  [float @list{(Number) -> Floating}]
  [float @list{(String) -> Floating}]
  [float @list{(Boolean) -> Floating}]
]

Converts an exact (integral or rational) number to the nearest
double-precision floating point value. If given a string, attempt to
convert to a number, throwing an error if the conversion fails. Booleans
@racket[True] and @racket[False] conver to @racket[1.0] and @racket[0.0],
respectively.

@subsubsection{Predicates}

@defprocform[zero?]{(Number) -> Boolean}

Determines whether its argument is zero.

@defprocform[positive?]{(Number) -> Boolean}

Determines whether its argument is greater than zero.

@defprocform[negative?]{(Number) -> Boolean}

Determines whether its argument is less than zero.

@defprocform[even?]{(Integer) -> Boolean}

Determines whether its argument is an even integer.

@defprocform[odd?]{(Integer) -> Boolean}

Determines whether its argument is an odd integer.

@subsection{String operations}

@defprocform[explode]{(String) -> Vector<String>}

Breaks a string into a vector of 1-character strings.

@defprocform[format]{(String, Any, ...) -> String}

Using its first argument as a template, interpolates the remaining
arguments, producing a string. The main recognized escape codes are
@code{~a} and @code{~s}. Both can be used to include any kind of data,
the difference being that @code{~s} quotes and escapes strings, whereas
@code{~a} includes them literally.

Additionally, @code{~n} can be used to insert a newline, and @code{~~}
inserts a literal @code{~}.

@defprocform[implode]{(Vector<String>) -> String}

Concatenates a vector of strings into a single string.

@subsection{Vector operations}

@defprocform[build_vector]{(n: Natural, f: (Natural) -> X) -> Vector<X>}

Creates a vector of size @code{n} whose elements are @code{f(0)},
@code{f(1)}, ..., @code{f(n - 1)}. Equivalent to

@verbatim[#:indent 4 #<<END
[ f(x) for x in n ]
END
]

@defprocform[filter]{(pred: (X) -> Boolean, vec: Vector<X>) -> Vector<X>}

Returns a vector containing the elements of @code{vec} for which
@code{pred} returns non-false. Equivalent to

@verbatim[#:indent 4 #<<END
[ x in vec if pred(x) ]
END
]

@defprocform[len]{(Vector<X>) -> Natural}

Returns the length of a vector.

@defprocform[map]{(f: (X) -> Y, vec: Vector<X>) -> Vector<Y>}

Returns a vector consisting of @code{f} applied to each element of
@code{vec}. Equivalent to

@verbatim[#:indent 4 #<<END
[ f(x) for x in vec ]
END
]

@subsection{I/O Functions}

@defprocforms[
  [print @list{(String, Any, ...) -> Void}]
  [println @list{(String, Any, ...) -> Void}]
]

The first argument is treated as a format string into which the
remaining arguments are interpolated, à la @racket[format]. Then the
result is printed. Function @racket[println] adds a newline at the end.

@subsection{Other functions}

@defprocform[identity]{(X) -> X}

The identity function, which just returns its argument.
