#lang scribble/manual

@require["common.rkt"]

@title[#:tag "exp-forms"]{Expression forms}

@section{Variable expressions}

@defexpform{@term[var_name]}

The value of a variable, which must be a function parameter, bound with
@racket[let], or defined with @racket[def]. For example,

@dssl2block|{
let x = 5
println('%p', x)
}|

prints “@code{5}”.

Lexically, a variable is a letter or underscore, followed by zero or
more letters, underscores, or digits, optionally ending in a question
mark or exclamation point.

@section{Literal expressions}

@defexpform{@term[number]}

A numeric literal.

@defexpform{@term[string]}

A string literal.

@defexpform{@defidform/inline[True]}

The true Boolean value.

@defexpform{@defidform/inline[False]}

The false Boolean value.

This is one of only two values that are not considered truthy for the
purpose of conditionals. The other non-truthy value is @racket[None].

@defexpform{@defidform/inline[None]}

A special value for representing missing data. This is also the result
of functions that are called just for their side effects, such as
@racket[print].

This is one of only two values that are not considered truthy for the
purpose of conditionals. The other non-truthy value is @racket[False].

@section{Functions and application expressions}

@defexpform{@nt_[expr]{0}(@nt_[expr]{1}, ..., @nt_[expr]{k})}

Evaluates all the expressions; @nt_[expr]{0} must evaluate to a
procedure. Then applies the result of @nt_[expr]{0} with the
results of the other expressions as arguments.

For example,

@dssl2block|{
fact(5)
}|

calls the function @racket[fact] with argument @racket[5], and

@dssl2block|{
ack(5 + 1, 5 + 2)
}|

calls the function @racket[ack] with arguments @racket[6] and
@racket[7].

Note that method calls are just object method lookup combined with
procedure applcation. That is, when you write

@dssl2block|{
q.enqueue(5)
}|

that means lookup the @c{enqueue} method in @c{q}, and then apply the
result to @racket[5].

@defexpforms[
  @list{@defidform/inline[lambda] @term_[var_name]{1}, ..., @term_[var_name]{k}: @nt[simple]}
  @list{λ @term_[var_name]{1}, ..., @term_[var_name]{k}: @nt[simple]}
]

Creates an anonymous function with parameters @term_[var_name]{1}, @c{...},
@term_[var_name]{k} and body @nt[simple]. For example, the function to
add twice its first argument to its second argument can be written

@dssl2block|{
lambda x, y: 2 * x + y
}|

@section[#:tag "indexing"]{Vectors and indexing expressions}

@defexpform{@nt_[expr]{1}[@nt_[expr]{2}]}

Expression @nt_[expr]{1} must evaluate to a vector or string @c{o};
@nt_[expr]{2} must evaluate to an integer @c{n} between 0 and
@code{o.len() - 1}. Then this returns the @c{n}th element of vector
@c{o} or the @c{n}th character of string @c{o}.

@defexpform{[ @nt_[expr]{1}, ..., @nt_[expr]{k} ]}

Creates a new vector of length @c{k} whose values are the values
of the expressions.

For example:

@dssl2block|{
let v = [ 1, 2, 3, 4, 5 ]
}|

@defexpform{[ @nt_[expr]{init}; @nt_[expr]{size} ]}

Constructs a new vector whose length is the value of
@nt_[expr]{size}, filled with the value of @nt_[expr]{init}. That is,

@dssl2block|{
[ 0; 5 ]
}|

means the same thing as

@dssl2block|{
[ 0, 0, 0, 0, 0 ]
}|

Note that @nt_[expr]{init} is not re-evaluated to produce each element,
so an expression like @code{[[0; 5]; 5]} produces a vector that contains
the same vector five times, not five different subvectors.

@defexpforms[
  @list{[ @nt_[expr]{elem} for @term[var_name] in @nt_[expr]{iter} ]}
  @list{[ @nt_[expr]{elem} for @term_[var_name]{1}, @term_[var_name]{2} in @nt_[expr]{iter} ]}
]

Vector comprehensions: produces a vector of the values of @nt_[expr]{elem}
while iterating the variable(s) over @nt_[expr]{iter}. In particular,
@nt_[expr]{iter} must be a vector @c{v}, a string @c{s}, or a
natural number @c{n}; in which case the iterated-over values are
the elements of @c{v}, the characters of
@c{s}, or counting from 0 to @code{n - 1}, respectively. If one
variable @term[var_name] is provided, it takes on those values. If two are
provided, then @term_[var_name]{2} takes on those values, while @term_[var_name]{1}
takes on the indices counting from 0 upward.

For example,

@dssl2block|{
[ 10 * n for n in [ 5, 4, 3, 2, 1 ] ]
}|

evaluates to

@dssl2block|{
[ 50, 40, 30, 20, 10 ]
}|

And

@dssl2block|{
[ 10 * n + i for i, n in [ 5, 4, 3, 2, 1 ] ]
}|

evaluates to

@dssl2block|{
[ 50, 41, 32, 23, 14 ]
}|

@defexpforms[
  @list{[ @nt_[expr]{elem} for @term[var_name] in @nt_[expr]{iter} if @nt_[expr]{cond} ]}
  @list{[ @nt_[expr]{elem} for @term_[var_name]{1}, @term_[var_name]{2} in @nt_[expr]{iter} if @nt_[expr]{cond} ]}
]

If the optional @nt_[expr]{cond} is provided, only elements for which
@nt_[expr]{cond} is non-false are included. That is, the variable(s) take on
each of their values, then @nt_[expr]{cond} is evaluated in the scope of the
variable(s). If it's non-false then @nt_[expr]{elem} is evaluated and
included in the resulting vector.

For example,

@dssl2block|{
[ 10 * n for n in [ 5, 4, 3, 2, 1 ] if odd?(n) ]
}|

evaluates to

@dssl2block|{
[ 50, 30, 10 ]
}|

@section{Structs and projection expressions}

@defexpform{@nt[expr].@term[prop_name]}

Expression @nt[expr] must evaluate to struct value that has field
@term[prop_name] or an object value that has method @term[prop_name]; then
this expression evaluates to the value of that field of the struct or
that method of the object.

@defexpform{@term[struct_name] { @term_[field_name]{1}: @nt_[expr]{1}, ..., @term_[field_name]{k}: @nt_[expr]{k} }}

Constructs a struct with the given name and the values of the given
expressions for its fields. The struct must have been declared with
those fields using @racket[struct].

If a variable with the same name as a field is in scope, omitting the
field value will use that variable:

@dssl2block|{
struct Foo:
    let bar
    let baz

let bar = 4
let baz = 5

assert_eq Foo { bar, baz: 9 }, Foo(4, 9)
}|

@section{Operator expressions}

Operators are described in order from tighest to loosest precedence.

@defexpform{@nt_[expr]{1} @defidform/inline[**] @nt_[expr]{2}}

Raises the value of @nt_[expr]{1} to the power of the value of
@nt_[expr]{2}, both of which must be numbers.

The @racket[**] operator is right-associative.

@defexpforms[
  @list{@defidform/inline[not] @nt[expr]}
  @list{@defidform/inline[~]@nt[expr]}
  @list{-@nt[expr]}
  @list{+@nt[expr]}
]

Logical negation, bitwise negation, numerical negation, and numerical identity.

@c{not} @nt[expr] evaluates @nt[expr], then returns @code{True} if
the result was @code{False} or @code{None}, and @code{False} for
any other result.

@c{~}@nt[expr] requires that @nt[expr] evalutes to an integer or
Boolean; it flips every bit of the number, or negates the Boolean.

@c{-}@nt[expr] and @c{+}@nt[expr] require that @nt[expr] evaluates to a
number. Then @c{-} negates the number, and @c{+} returns it unchanged.

@defexpforms[
  @list{@nt_[expr]{1} @defidform/inline[*] @nt_[expr]{2}}
  @list{@nt_[expr]{1} @defidform/inline[/] @nt_[expr]{2}}
  @list{@nt_[expr]{1} @defidform/inline[%] @nt_[expr]{2}}
]

Multiplies, divides, or modulos the values of the expressions, respectively.

@defexpform{@nt_[expr]{1} @defidform/inline[+] @nt_[expr]{2}}

Addition:

@itemlist[
  @item{Given two numbers, adds them.}
  @item{Given two strings, concatenates them.}
  @item{Given a string and another value, in any order, converts
        the other value to a string and concatenates them.}
]

Anything else is an error.

@defexpform{@nt_[expr]{1} @defidform/inline[-] @nt_[expr]{2}}

Subtracts two numbers.

@defexpforms[
  @list{@nt_[expr]{1} @defidform/inline[<<] @nt_[expr]{2}}
  @list{@nt_[expr]{1} @defidform/inline[>>] @nt_[expr]{2}}
]

Left and right bitwise shift.

@defexpform{@nt_[expr]{1} @defidform/inline[&] @nt_[expr]{2}}

Bitwise @emph{and} for integers; logical @emph{and} for Booleans.

@defexpform{@nt_[expr]{1} @defidform/inline[^] @nt_[expr]{2}}

Bitwise @emph{xor} for integers; logical @emph{xor} for Booleans.

@defexpform{@nt_[expr]{1} @defidform/inline[\|] @nt_[expr]{2}}

Bitwise @emph{or} for integers; logical @emph{or} for Booleans.
(Not written with the backslash.)

@defexpforms[
  @list{@nt_[expr]{1} @defidform/inline[==] @nt_[expr]{2}}
  @list{@nt_[expr]{1} @defidform/inline[!=] @nt_[expr]{2}}
  @list{@nt_[expr]{1} @defidform/inline[is] @nt_[expr]{2}}
  @list{@nt_[expr]{1} @defidform/inline[|is not|] @nt_[expr]{2}}
  @list{@nt_[expr]{1} @defidform/inline[<] @nt_[expr]{2}}
  @list{@nt_[expr]{1} @defidform/inline[<=] @nt_[expr]{2}}
  @list{@nt_[expr]{1} @defidform/inline[>] @nt_[expr]{2}}
  @list{@nt_[expr]{1} @defidform/inline[>=] @nt_[expr]{2}}
]

Operator @racket[==] is structural equality (except for classes that
override it), and @racket[!=] is its negation. Operator @racket[is] is
physical equality, and @racket[|is not|] (not written with the vertical
bars) is its negation. To understand the difference, suppose that we
create two different vectors with the same contents. Those vectors are
structurally equal but not physically equal.

Operators @racket[<], @racket[<=], @racket[>], and @racket[>=] are the
standard inequalities for numbers and characters, and compare strings in
lexicographic order.

@defexpform{@nt_[expr]{1} @defidform/inline[and] @nt_[expr]{2}}

Short-circuiting logical @emph{and}. First evaluates @nt_[expr]{1}; if
the result is @code{False} or @code{None} then the whole conjunction is
@code{False}; otherwise, the result of the conjunction is the result of
@nt_[expr]{2}.

@defexpform{@nt_[expr]{1} @defidform/inline[or] @nt_[expr]{2}}

Short-circuiting logical @emph{or}. First evaluates @nt_[expr]{1}; if
the result is non-false then the whole disjunction has that result;
otherwise the result of the disjunction is the result of @nt_[expr]{2}.

@defexpform{@nt_[expr]{then} if @nt_[expr]{cond} else @nt_[expr]{else}}

The ternary expression first evaluates the condition
@nt_[expr]{cond}. If non-false,
evaluates @nt_[expr]{then} for its value; otherwise,
evaluates @nt_[expr]{else} for its value.

For example:

@dssl2block|{
def parent(link):
    link.parent if rbn?(link) else None
}|
