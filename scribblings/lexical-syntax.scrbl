#lang scribble/manual

@require["common.rkt"]

@title{Lexical syntax}

@section{Identifiers}

@italic{Name}s, used for variables, functions, structs, classes,
interfaces, fields, and methods, must start with a letter, followed by 0
or more letters or digits. The last character also may be @q{?} or
@q{!}.

@section{Numeric literals}

Numeric literals include:

@itemlist[
  @item{Decimal integers: @racket[0], @racket[3], @racket[18446744073709551617]}
  @item{Hexadedecimal, octal, and binary integers: @q{0xFFFF00},
      @q{0o0177}, @q{0b011010010}}
  @item{Floating point: @racket[3.5], @q{6.02E23}, @racket[1e-12], @racket[inf],
  @racket[nan]}
]

@section{String literals}

String literals are delimited by either single or double quotes:

@dssl2block|{
def does_not_matter(double):
    if double:
        return "This is the same string."
    else:
        return 'This is the same string.'
}|

The contents of each kind of string is treated the same, except that
each kind of quotation mark can contain the other kind unescaped:

@dssl2block|{
def does_matter(double):
    if double:
        return "This isn't the same string."
    else:
        return '"This is not the same string" isn\'t the same string.'
}|

Strings cannot contain newlines directly, but can contain newline
characters via the escape code @c{\n}. Other escape codes include:

@itemlist[
  @item{@c{\a} for ASCII alert (also @c{\x07})}
  @item{@c{\b} for ASCII backspace (also @c{\x08})}
  @item{@c{\f} for ASCII formfeed (also @c{\x0C})}
  @item{@c{\n} for ASCII newline (also @c{\x0A})}
  @item{@c{\r} for ASCII carriage return (also @c{\x0D})}
  @item{@c{\t} for ASCII tab (also @c{\x09})}
  @item{@c{\v} for ASCII vertical tab (also @c{\x0B})}
  @item{@c{\x@term{hh}} in hex, for example @c{\x0A} is newline}
  @item{@c{\@term{ooo}} in octal, for example @c{\011} is tab}
  @item{A backslash immediately followed by a newline causes both characters to
      be ignored, which provides a way to wrap long strings across lines.}
]

Any other character following a backslash stands for itself.

An alternative form for string literals uses three quotation marks of
either kind. The contents of such a string are treated literally, rather
than interpreting escapes, and they may contain any characters except
the terminating quotation mark sequence.

@dssl2block|{
let a_long_string = '''This string can contain ' and " and
even """ and newlines. Just not '' and one more.'''
}|

@section{Comments}

A comment in DSSL2 starts with the @q{#} character and continues to the
end of the line.

Long string literals can also be used to comment out long blocks of code.

