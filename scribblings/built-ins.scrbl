#lang scribble/manual

@require["common.rkt"]

@title[#:tag "prims"]{Built-in functions, classes, methods, and constants}

@section{Primitive classes}

DSSL2 includes seven primitive classes for building up more complex data
structures. The constructors and methods of these classes are documented
in this subsection.

@defclassform[bool]

The primitive class for Boolean values, @code{True} and
@code{False}. The type predicate for @linkclass[bool] is
@racket[bool?].

Booleans support logical binary operators @racket[&] (@emph{and}),
@racket[\|] (@emph{or}, written without the backslash), and @racket[^]
(@emph{xor}), and logical unary negation @racket[~]. They also support
comparison with @racket[==], @racket[<], @racket[<=], etc. @code{False}
compares less than @code{True}.

@defprocforms[bool
    @list{(@code{AnyC}) -> @code{bool?}}
    @list{() -> @code{bool?}}
]

The constructor for @linkclass[bool].

In its one-argument form, converts any type to @racket[bool]. All values but
@code{False} and @code{None} convert to @code{True}.

In its no-argument form, returns @code{False}.

@defclassform[char]

The primitive class for representing a single character of text.
The type predicate for @linkclass[char] is @racket[char?].

A character can be converted to its integer value with the @racket[int]
constructor. Characters can be compared with @racket[==], @racket[<],
@racket[<=], etc.

@defprocforms[char
    @proto[char bool?]
    @proto[int? bool?]
    @proto[str? bool?]
    @proto[bool?]
]

The constructor for @linkclass[char].

Given a character, returns that character. Given an integer, returns the
character corresponding to the Unicode code point, or errors if the
integer is not a valid Unicode character. Given a one-character string,
returns the character of the string; any longer or shorter string is an
error.

@defclassform[int]

The primitive class for representing integral quantities of unlimited
size. The type predicate for @linkclass[int] is @racket[int?].

Integers support binary arithmethic operators @racket[+] (addition),
@racket[-] (subtraction), @racket[*] (multiplication), @racket[//]
(integer division), and @racket[/] (float division, which always
produces a @linkclass[float]).
They also support unary @racket[+]
(identity) and @racket[-] (negation).

They also support comparison with @racket[==], @racket[<], @racket[<=],
etc., and they can be compared against floats.

Integers support binary bitwise operators @racket[&] (bitwise
@emph{and}), @racket[\|] (bitwise @emph{or}), and @racket[^] (bitwise
@emph{xor}), and unary bitwise negation @racket[~].

@defprocforms[int
    @proto[num? int?]
    @proto[char? int?]
    @proto[str? int?]
    @proto[bool? int?]
    @proto[int?]
]

The constructor for @linkclass[int].

@itemlist[
@item{
Given a number, returns the integer part, by truncation.
That is, the decimal point and everything after it is removed.
}

@item{
    Given a character, returns the Unicode code point value.
}

@item{
Given a string, attempts to convert to a number before truncating,
throwing an error if the conversion fails.
}

@item{
Booleans @code{True} and @code{False} convert to @racket[1] and
@racket[0], respectively.
}

@item{
Given no arguments, returns @racket[0].
}
]

@defmethform[int abs]{@proto[int?]}

Returns the absolute value of the receiving integer.

@defmethform[int ceiling]{@proto[int?]}

Returns the same integer.

@defmethform[int floor]{@proto[int?]}

Returns the same integer.

@defmethform[int sqrt]{@proto[float?]}

Returns the square root of the receiving integer, as a @racket[float].

@defmethform[int cos]{@proto[float?]}

The cosine function.

@defmethform[int sin]{@proto[float?]}

The sine function.

@defmethform[int tan]{@proto[float?]}

The tangent function.

@defmethform[int acos]{@proto[float?]}

The inverse cosine function.

@defmethform[int asin]{@proto[float?]}

The inverse sine function.

@defmethforms[int atan
              [@proto[float?]]
              [@proto[num? float?]]]

The inverse tangent function.

Optionally takes an argument, in which case @code{y.atan(x)} computes the
angle from the origin to the Cartesian point (@code{x}, @code{y}).

@defmethforms[int log
              [@proto[float?]]
              [@proto[num? float?]]]

The logarithm function.

Without an argument, returns the natural logarithm of the receiver. When an
argument, returns the logarithm of the receiver using the argument as the base.

@defclassform[float]

The primitive class for representing approximations of real numbers (as
64-bit IEEE 754 numbers). The type predicate for @linkclass[float] is
@racket[float?].

Floats support binary arithmethic operators @racket[+] (addition),
@racket[-] (subtraction), @racket[*] (multiplication), and @racket[/]
(division); when combined with an instance of @linkclass[int],
the result will also be a float. They also support unary @racket[+]
(identity) and @racket[-] (negation).

They also support comparison with @racket[==], @racket[<], @racket[<=],
etc., and they can be compared against ints.

@defprocforms[float
  @proto[num? float?]
  @proto[str? float?]
  @proto[bool? float?]
  @proto[float?]
]

The constructor for @linkclass[float].

Converts an exact integer to the nearest
double-precision floating point value. If given a string, attempt to
convert to a number, throwing an error if the conversion fails. Booleans
@code{True} and @code{False} convert to @racket[1.0] and @racket[0.0],
respectively.

Given no arguments, returns @racket[0.0].

@defmethform[float abs]{@proto[float?]}

Returns the absolute value of the receiving float.

@defmethform[float ceiling]{@proto[int?]}

Returns smallest integer that is no less than the receiving float.
That is, it rounds up to the nearest integer.

@defmethform[float floor]{@proto[int?]}

Returns the largest integer that is no greater than the receiving float.
That is, it rounds down to the nearest integer.

@defmethform[float sqrt]{@proto[float?]}

Returns the square root of the receiving float.

@defmethform[float cos]{@proto[float?]}

The cosine function.

@defmethform[float sin]{@proto[float?]}

The sine function.

@defmethform[float tan]{@proto[float?]}

The tangent function.

@defmethform[float acos]{@proto[float?]}

The inverse cosine function.

@defmethform[float asin]{@proto[float?]}

The inverse sine function.

@defmethforms[float atan
              [@proto[float?]]
              [@proto[num? float?]]]

The inverse tangent function.

Optionally takes an argument, in which case @code{y.atan(x)} computes the
angle from the origin to the Cartesian point (@code{x}, @code{y}).

@defmethforms[float log
              [@proto[float?]]
              [@proto[num? float?]]]

The logarithm function.

Without an argument, returns the natural logarithm of the receiver. When an
argument, returns the logarithm of the receiver using the argument as the base.

@defclassform[proc]

The primitive class for representing functions.
The type predicate for @linkclass[proc] is @racket[proc?].

Procedures can be applied.

@defprocforms[proc
    @proto[proc? proc?]
    @proto[proc?]
]

The constructor for @linkclass[proc].

Given one procedure argument, returns it unchanged. Given no arguments,
returns the identity function.

@defmethform[proc compose]{@proto[proc? proc?]}

Composes the receiving procedure with the parameter procedure. For example,

@dssl2block|{
assert (λ x: x + 1).compose(λ x: x * 2)(5) == 11
}|

@defmethform[proc vec_apply]{@proto[vec? AnyC]}

Applies the receiving procedure using the contents of the given vector
as its arguments.

@defclassform[str]

The primitive class for representing textual data.
The type predicate for @linkclass[str] is @racket[str?].

A string can be indexed with square bracket notation, which returns an
instance of @linkclass[char]. Strings may be concatenated with the
@racket[+] operator. Concatenating a string with a non-string using
@racket[+] converts the non-string to a string first.

@defprocforms[str
    @proto[str? str?]
    @proto[AnyC str?]
    @proto[len:nat? c:char? str?]
    @proto[str?]
]

The constructor for @linkclass[str].

@itemlist[
@item{
Given one string argument, returns that argument unchanged.
}

@item{
Given one non-string argument, converts that argument to a string
according to the @q{%p} format specifier.
}

@item{

Given two arguments, a natural @c{len} and a character @c{c}, makes a
string of the given length @c{len}, repeating character @c{c}.
}

@item{
Given no arguments, returns the empty string.
}
]

@defmethform[str explode]{@proto["VecC[char?]"]}

Breaks the receiving string into a vector of its characters.

@defmethform[str format]{@proto[AnyC ... str?]}

Formats the given arguments, treating the receiving string
as a format string in the style of @racket[print].

For example,

@dssl2block|{
assert '%s or %p'.format('this', 'that') == "this or 'that'"
}|

@defmethform[str len]{@proto[nat?]}

Returns the length of the receiving string in characters.

@defclassform[vec]

The primitive vector class, for representing sequence of values of fixed size.

This section documents only the methods of the vector class.
Vector-specific syntax (@emph{i.e.,} square brackets for indexing and
construction) is @seclink["indexing"]{documented with the other
expression syntax}.

The type predicate for @linkclass[vec] is @racket[vec?].

@defprocforms[vec
    @proto[vec?]
    @proto[n:nat?  vec?]
    @proto[n:nat? "init:FunC[nat?, AnyC]" vec?]
]

The constructor for @linkclass[vec].

@itemlist[
@item{
Given no arguments, returns an empty, zero-length vector.
}

@item{
Given one argument, returns a vector of size @racket[n], filled with
@code{None}.
}

@item{
Given two arguments, returns a vector of the given size, with each
element initialized by applying the given function to its index.
That is, @code{vec(n, init)} is equivalent to
@code{[ init(i) for i in range(n) ]}.
}
]

@defmethform[vec filter]{@proto["pred?:FunC[AnyC, AnyC]" vec?]}

Filters the given vector, by returning a vector of only the elements
satisfying the given predicate @c{pred?}.

In particular,
@code|{
v.filter(pred?)
}|
is equivalent to
@code|{
[ x for x in v if pred?(x) ]
}|

@defmethform[vec implode]{@proto[vec?]}

If the receiver is a vector of characters, joins them into a string.
(Otherwise, an error is reported.)

@defmethform[vec len]{@proto[nat?]}

Returns the length of the vector.

@defmethform[vec map]{@proto["f:FunC[AnyC, AnyC]" vec?]}

Maps a function over a vector, returning a new vector.

In particular,
@code|{
v.map(f)
}|
is equivalent to
@code|{
[ f(x) for x in v ]
}|

@defclassform[range_iterator]

An iterator over a range of numbers, constructed by
@racket[range].

@section{Predicates}

@subsection{Basic type predicates}

@defprocform[bool?]{@proto[AnyC bool?]}

Determines whether its argument is a Boolean, that is
an instance of @linkclass[bool].

@defprocform[char?]{@proto[AnyC bool?]}

Determines whether its argument is an instance of @linkclass[char].

@defprocform[contract?]{@proto[AnyC bool?]}

Determines whether its argument is a contract. Contracts include many
constants (numbers, strings, Booleans), single-argument functions
(considered as predicates), and the results of contract combinators such
as @racket[OrC] and @racket[FunC].

See @secref["Contracts"] for more.

@defprocform[flat_contract?]{@proto[AnyC bool?]}

Determines whether its argument is a flat contract. Flat contracts are
contracts that answer right away and return the protected value unchanged
on success, rather than wrapping it. Flat contracts include constants
(numbers, strings, Booleans), single-argument functions, and the results
of contract combinators such as @racket[OrC] and @racket[AndC] applied to
flat contracts. Non-flat contracts include the results of @racket[FunC] and
@racket[VecC].

See @secref["Contracts"] for more.

@defprocform[int?]{@proto[AnyC bool?]}

Determines whether its argument is an instance of @linkclass[int].

@defprocform[float?]{@proto[AnyC bool?]}

Determines whether its argument is an instance of @linkclass[float].

@defprocform[proc?]{@proto[AnyC bool?]}

Determines whether its argument is an instance of @linkclass[proc].

@defprocform[str?]{@proto[AnyC bool?]}

Determines whether its argument is an instance of @linkclass[str].

@defprocform[vec?]{@proto[AnyC bool?]}

Determines whether its argument is an instance of @linkclass[vec].

@defprocform[range_iterator?]{@proto[AnyC bool?]}

Determines whether its argument is an instance of @linkclass[range_iterator].

@subsection{Numeric predicates}

@defprocform[num?]{@proto[AnyC bool?]}

Determines whether its argument is a number. This includes both
@linkclass[int] and @linkclass[float].

@defprocform[nat?]{@proto[AnyC bool?]}

Determines whether its argument is a non-negative integer.

@defprocform[zero?]{@proto[AnyC bool?]}

Determines whether its argument is a number equal to zero.

@defprocform[pos?]{@proto[AnyC bool?]}

Determines whether its argument is a number greater than zero.

@defprocform[neg?]{@proto[AnyC bool?]}

Determines whether its argument is a number less than zero.

@defprocform[even?]{@proto[AnyC bool?]}

Determines whether its argument is an even number.

@defprocform[odd?]{@proto[AnyC bool?]}

Determines whether its argument is an odd number.

@defprocform[nan?]{@proto[AnyC bool?]}

Determines whether its argument is the special @linkclass[float]
not-a-number value. This is useful, since @code{nan} is not necessarily
@racket[==] to other instances of @code{nan}.

@section{Comparison operations}

@defprocform[cmp]{@proto[AnyC AnyC "OrC(int?, NoneC)"]}

Compares two values of any type. If the values are incomparable, returns
@code{None}. Otherwise, returns an integer: less than 0 if the first
argument is less, 0 if equal, or greater than 0 if the first argument is
greater.

@defprocform[max]{@proto[AnyC AnyC ... AnyC]}

Returns the largest of the given arguments, using @racket[cmp] to determine
ordering. It is an error if the values are not comparable.

@defprocform[min]{@proto[AnyC AnyC ... AnyC]}

Returns the smallest of the given arguments, using @racket[cmp] to determine
ordering. It is an error if the values are not comparable.

@section{Randomness operations}

@defprocforms[random
  @proto[float?]
  @proto["limit:IntInC(1, 4294967087)" nat?]
  @proto[start:int? limit:int? nat?]
]

When called with zero arguments, returns a random floating point number
in the open interval (@racket[0.0], @racket[1.0]).

When called with one argument @racket[limit], returns a random
integer from the closed interval [@racket[0], @code{limit - 1}].

When called with two arguments @code{start} and @code{limit}, returns a
random integer from the closed interval [@code{start}, @code{limit - 1}].
The difference between the arguments can be no greater than
@racket[4294967087].

@defconstform[RAND_MAX]{@code{nat?}}

Defined to be @racket[4294967087], the largest parameter (or span) that
can be passed to @racket[random].

@defprocform[random_bits]{@proto[nat? nat?]}

Returns a natural number consisting of the requested number of random
bits. That is, given @code{n}, returns an integer in the closed interval
[@code{0}, @code{2 ** n - 1}]. This procedure may be slow, but it is not
limited by @racket[RAND_MAX] in the way that the @racket[random]
procedure is.

@section{I/O Functions}

@defprocform[print]{@proto[str? AnyC ... NoneC]}

The first argument is treated as a format string into which the
remaining arguments are interpolated, and then the result is printed.

The format string is a string that contains a formatting code for each
additional argument, as follows:

@itemlist[
    @item{@q{%p} – prints the object, formatted nicely if possible}
    @item{@q{%d} – prints the object in raw, debug mode, ignoring
                   user-defined printers}
    @item{@q{%s} – interpolates a string or character as itself, without
                   quoting; the same as @q{%p} for other types}
]

For example:

@dssl2block|{
let a = 3
let b = 4
print("%p + %p = %p", a, b, a + b)
}|

prints “3 + 4 = 7”.

@defprocforms[println
    @proto[str? AnyC ... NoneC]
    @proto[AnyC ... NoneC]
]

If the first argument is a string then @racket[println] is
like @racket[print], but adds a newline at the end.

If the first argument is a non-string or there are no arguments, then
@racket[println] prints all the arguments with commas in between and a
newline after, as if formatted by @racket["%p, …, %p\n"].

If DSSL2 supported user-defined varargs functions, it might be defined
as:

@verbatim|{
def println(*args):
    if args.len() == 0:
        print('\n')
    elif str?(args[0]):
        print(*args)
        print('\n')
    else:
        let fmt = ''
        for _ in args:
            fmt = '%p' if fmt == '' else fmt + ', %p'
        println(fmt, *args)
}|

@defprocforms[current_directory
    @proto[str?]
    @proto[str? bool?]
]

Given with no arguments, @racket[current_directory] returns the current
working directory as a string.

Called with one string argument @racket[s], @racket[current_directory]
attempts to change the working directory to the directory named by
@racket[s]. It is an error if the directory does not exist or the
operation isn’t permitted.

@defprocform[file_to_string]{@proto[str? str?]}

Given the name of a file, reads its entire contents and returns it as a
string.

It is an error if the file doesn’t exist or cannot be read.

@defprocform[string_to_file]{@proto[message:str? filename:str? NoneC]}

Writes string @racket[message] to file @racket[filename]. If the file
doesn’t exist, it is created; if it already exists then its contents are
replaced.

It is an error if the file cannot be written.

@section{Other functions}

@defprocforms[error
    @proto[str? AnyC ... NoneC]
    @proto[AnyC ... NoneC]
]

Terminates the program with an error message.

If a first argument is supplied and it is a string, then the error
message will be produced by interpolating the remaining arguments
into the string à la @racket[print].

If there are no arguments, or if the first argument is not a string,
then it formats all the arguments with commas in between and a “call” to
@racket[error] around it. This ensure that all calls to @racket[error]
produce some kind of sensible output.

@defprocform[len]{@proto[o:AnyC nat?]}

Equivalent to @code{o.len()}.

@defprocforms[range
    @proto[start:num? limit:num? step:num? range_iterator?]
    @proto[start:num? limit:num? range_iterator?]
    @proto[limit:num? range_iterator?]
]

Returns an iterator over the closed-open interval from
@racket[start] to @racket[stop] in increments of
@racket[step].

For example, @code|{range(0, 10, 2)}| counts upward from 0 to 10
by 2s, producing five values:

@dssl2block|{
assert [ n for n in range(0, 10, 2) ] == [ 0, 2, 4, 6, 8 ]
}|

Whereas @code|{range(50, 0, -15)}| counts downward from 50 to 0
by 15s, producing four values:

@dssl2block|{
assert [ n for n in range(50, 0, -15) ] == [ 50, 35, 20, 5 ]
}|

If two arguments are given then they are the start and the
limit, and if only one argument is given then it's the
limit. In the two- and one-argument forms the step defaults
to 1. In the one-argument form, the start defaults to 0. For
example:

@dssl2block|{
assert [ n for n in range(3, 7) ] == [ 3, 4, 5, 6 ]
assert [ n for n in range(7, 3) ] == []
assert [ n for n in range(7) ]    == [ 0, 1, 2, 3, 4, 5, 6 ]
}|

@defprocform[dir]{@proto[AnyC "VecC[str?]"]}

Given an object, returns a vector of the names of its methods.
Given a struct, returns a vector of the names of its fields.
