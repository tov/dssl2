#lang scribble/manual

@(require
        "util.rkt"
        (for-label dssl2)
        (for-label (prefix-in racket: racket)))

@title{DSSL2: Data Structures Student Language}
@author{Jesse A. Tov <jesse@"@"eecs.northwestern.edu>}

@defmodulelang[dssl2]

@section[#:tag "dssl-syntax"]{Syntax of DSSL2}

@subsection{Compound statements and blocks}

DSSL2 uses alignment and indentation to delimit @deftech{blocks}. In
particular, compound statements such as
@racket[if]-@racket[elif]-@racket[else] take @syn[block]s for each
condition, where a @syn[block] can be either one simple statement
followed by a newline, or a sequence of statements on subsequent lines
that are all indented by four additional spaces. Here is an example of a
tree insertion function written using indentation:

@codeblock|{
#lang dssl2

def insert!(t, k):
    if empty?(t): new_node(k)
    elif zero?(random(size(t) + 1)):
        root_insert!(t, k)
    elif k < t.key:
        t.left = insert!(t.left, k)
        fix_size!(t)
        t
    elif k > t.key:
        t.right = insert!(t.right, k)
        fix_size!(t)
        t
    else: t
}|

Each block follows a colon and newline, and is indented 4 spaces more
than the previous line. In particular, the block started by @racket[def]
is indented by 4 spaces, and the @racket[elif] blocks by
8. When a block is a simple statement, it can be placed on the same
line, as in the @racket[if] and @racket[else] cases.

Extraneous indentation is an error.

@subsection{Formal Grammar}

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
#:literals (def defstruct let lambda λ else if elif while for in test
            time object break continue : True False =
            assert assert_eq assert_error pass return NEWLINE INDENT DEDENT)
[program (code:line @#,m["{"] statement @#,m["}*"])]
[statement   (code:line simple @#,q{NEWLINE})
             compound]
[simple
            (code:line assert expr)
            (code:line assert_eq expr @#,q{,} expr)
            (code:line assert_error expr @#,m["["] @#,q{,} string_expr @#,m["]"])
            break
            continue
            (code:line defstruct struct_name @#,q{(} @#,m["{"] field_name @#,m["["] @#,q{:} contract_expr @#,m["]"] @#,m["},*"] @#,q{)})
            (code:line lvalue = expr)
            expr
            (code:line let var_name @#,m["["] @#,q{:} contract_expr @#,m["]"] @#,m["["] @#,q{=} expr @#,m["]"])
            (code:line pass)
            (code:line return @#,m["["] expr @#,m["]"])
            (code:line simple @#,q{;} simple)]
[lvalue var_name
        (code:line struct_expr @#,q{.} field_name)
        (code:line vec_expr @#,q["["] index_expr @#,q["]"])]
[compound
            (code:line def name @#,q{(} name @#,m["["] @#,q{:} ctc_expr @#,m["]"] @#,q{,} @#,m{...} @#,q{)} @#,m["["] @#,q{->} ctc_expr @#,m["]"] @#,q{:} block)
            (code:line if expr @#,q{:} block @#,m["{"] elif expr @#,q{:} block @#,m["}*"] @#,m["["] else @#,q{:} block @#,m["]"])
            (code:line for @#,m{[} var_name @#,q{,} @#,m{]} var_name @#,q{in} expr @#,q{:} block)
            (code:line test @#,m{[} expr @#,m{]} @#,q{:} block)
            (code:line time @#,m{[} expr @#,m{]} @#,q{:} block)
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
      (code:line lambda @#,m["{"] var_name @#,m["},*"] @#,q{:} simple)
      (code:line @#,q{λ} @#,m["{"] var_name @#,m["},*"] @#,q{:} simple)
      (code:line expr @#,q{if} expr @#,q{else} expr)
      (code:line struct_name @#,q["{"] @#,m["{"] field_name : expr @#,m["},*"] @#,q[" }"])
      (code:line object struct_name @#,q["{"] @#,m["{"] field_name : expr @#,m["},*"] @#,q[" }"])
      (code:line @#,q{[} @#,m["{"] expr @#,m["},*"] @#,q{]})
      (code:line @#,q{[} expr @#,q{;} expr @#,q{]})
      (code:line @#,q{[} expr @#,q{for} @#,m{[} var_name @#,q{,} @#,m{]} var_name @#,q{in} expr @#,m{[} @#,q{if} expr @#,m{]} @#,q{]})
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
 @item{@racket[or]}
]

@italic{UNOP}s are @racket[!], @racket[~], @racket[+], @racket[-].

@subsection{Lexical Syntax}

@subsubsection{Identifiers}

@italic{Name}s, used for variables, functions, and struct fields, must
start with a letter, followed by 0 or more letters or digits. The last
character also may be @q{?} or @q{!}.

@subsubsection{Numeric Literals}

Numeric literals include:

@itemlist[
  @item{Decimal integers: @racket[0], @racket[3], @racket[18446744073709551617]}
  @item{Hexadedecimal, octal, and binary integers: @q{0xFFFF00},
      @q{0o0177}, @q{0b011010010}}
  @item{Floating point: @racket[3.5], @q{6.02E23}, @racket[1e-12], @racket[inf],
  @racket[nan]}
]

@subsubsection{String Literals}

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
  @item{@c{\x@syn{hh}} in hex, for example @c{\x0A} is newline}
  @item{@c{\@syn{ooo}} in octal, for example @c{\011} is tab}
  @item{A backslash immediately followed by a newline causes both characters to
      be ignored, which provides a way to wrap long strings across lines.}
]

Any other character following a backslash stands for itself.

@subsubsection{Comments}

A comment in DSSL2 starts with the @q{#} character and continues to the
end of the line.

@subsection[#:tag "stm-forms"]{Statement Forms}

@defsmplform{@defidform/inline[assert] @syn[expr]}

Asserts that the given @syn[expr] evaluates to non-false. If the
expression evaluates false, signals an error.

@dssl2block|{
test "sch_member? finds 'hello'":
    let h = sch_new_sbox(10)
    assert !sch_member?(h, 'hello')
    sch_insert!(h, 'hello', 5)
    assert sch_member?(h, 'hello')
}|

@defsmplform{@defidform/inline[assert_eq] @syn[expr]₁, @syn[expr]₂}

Asserts that the given @syn[expr]s evaluates to structurally equal values.
If they are not equal, signals an error.

@dssl2block|{
test 'first_char_hasher':
    assert_eq first_char_hasher(''), 0
    assert_eq first_char_hasher('A'), 65
    assert_eq first_char_hasher('Apple'), 65
    assert_eq first_char_hasher('apple'), 97
}|

@defsmplform{@defidform/inline[assert_error] @syn[expr], @syn[string_expr]}

Asserts that the given @syn[expr] errors, and that the error message
contains the substring that results from evaluating @syn[string_expr].

@defsmplform{@redefidform/inline[assert_error] @syn[expr]}

Asserts that the given @syn[expr] errors without checking for a
particular error.

@defsmplform{@defidform/inline[break]}

When in a @racket[for] or @racket[while] loop, ends the (inner-most)
loop immediately.

@defsmplform{@defidform/inline[continue]}

When in a @racket[for] or @racket[while] loop, ends the current
iteration of the (inner-most) loop and begins the next iteration.

@defcmpdform{@defidform/inline[def] @syn[fun_name](@syn[var_name]₁, ... @syn[var_name]@subscript{k}): @syn[block]}

Defines @syn[fun_name] to be a function with formal parameters @syn[var_name]₁,
@c{...}, @syn[var_name]@subscript{k} and with body @syn[block].

For example,

@dssl2block|{
def fact(n):
    if n < 2:
        return 1
    else:
        return n * fact(n - 1)
}|

A function may have zero arguments, as in @racket[greet]:

@dssl2block|{
def greet(): println("Hello, world!")
}|

The body of a function is defined to be a @tech{block}, which means it
can be an indented sequence of statements, or a single simple statement
on the same line as the @racket[def].

Note that @racket[def]s can be nested:

@dssl2block|{
# rbt_insert! : X RbTreeOf[X] -> Void
def rbt_insert!(key, tree):
    # parent : RbLinkOf[X] -> RbLinkOf[X]
    def parent(link):
        link.parent if rbn?(link) else False

    # grandparent : RbLinkOf[X] -> RbLinkOf[X]
    def grandparent(link):
        parent(parent(link))

    # sibling : RbLinkOf[X] -> RbLinkOf[X]
    def sibling(link):
        let p = parent(link)
        if rbn?(p):
            if link === p.left: p.right
            else: p.left
        else: False

    # aunt : RbLinkOf[X] -> RbLinkOf[X]
    def aunt(link):
        sibling(parent(link))

    # . . .

    def set_root!(new_node): tree.root = new_node
    search!(tree.root, set_root!)
}|

@defsmplform{@defidform/inline[defstruct] @syn[struct_name](@syn[field_name]₁, ..., @syn[field_name]@subscript{k})}

Defines a new structure type @syn[struct_name] with fields given by
@syn[field_name]₁, @c{...}, @syn[field_name]@subscript{k}. For example,
to define a struct @racket[posn] with fields @racket[x] and @racket[y],
we write:

@dssl2block|{
defstruct posn(x, y)
}|

Then we can create a @racket[posn] using struct construction syntax and
select out the fields using dotted selection syntax:

@dssl2block|{
let p = posn { x: 3, y: 4 }
}|

@dssl2block|{
def magnitude(q):
    sqrt(q.x * q.x + q.y * q.y)
}|

It also possible to construct the struct by giving the fields in order
using function syntax:

@dssl2block|{
assert_eq magnitude(posn(3, 4)), 5
}|

Another example:

@dssl2block|{
# A RndBstOf[X] is one of:
# - False
# - Node(X, nat?, RndBstOf[X], RndBstOf[X])
defstruct Node(key, size, left, right)

# singleton : X -> RndBstOf[X]
def singleton(key):
    Node(key, 1, False, False)

# size : RndBstOf[X] -> nat?
def size(tree):
    tree.size if Node?(tree) else 0

# fix_size! : Node? -> Void
def fix_size!(node):
    node.size = 1 + size(node.left) + size(node.right)
}|

@defsmplform{@syn[lvalue] @defidform/inline[=] @syn[expr]}

Assignment. The assigned @syn[lvalue] can be in one of three forms:

@itemlist[
 @item{@syn[var_name] assigns to a variable, which can be a @syn[let]-bound
 local or a function parameter.}
 @item{@c{@syn[struct_expr].@syn[field_name]} assigns to a structure field, where
 the expression must evaluate to a structure that has the given field
 name.}
 @item{@c{@syn[vec_expr][@syn[index_expr]]} assigns to a vector element, where
 @c{@syn[vec_expr]} evaluates to the vector and @c{@syn[index_expr]}
 evaluates to the index of the element.}
]

This function assigns all three kinds of l-value:

@dssl2block|{
def sch_insert!(hash, key, value):
    let index = sch_bucket_index_(hash, key)
    let current = hash.buckets[index]
    while cons?(current):
        if key == current.first.key:
            # struct assignment:
            current.first.value = value
            return
        # variable assignment:
        current = current.rest
    # vector assignment:
    hash.buckets[index] = cons(sc_entry(key, value), hash.buckets[index])
}|

@defsmplform{@syn[expr]}

An expression, evaluated for both side effect and, if at the tail end
of a function, its value.

For example, this function returns the @racket[size] field of parameter
@racket[tree] if @racket[tree] is a @racket[Node], and @racket[0] otherwise:

@dssl2block|{
# size : RndBstOf[X] -> nat?
def size(tree):
    if Node?(tree): tree.size
    else: 0
}|

@defcmpdform{@defidform/inline[if] @syn[expr]@subscript{if}: @syn[block]@subscript{if}
             @defidform/inline[elif] @syn[expr]@subscript{i}: @syn[block]@subscript{i}
             @defidform/inline[else]: @syn[block]@subscript{else}}

The DSSL2 conditional statement contains an @racket[if], 0 or more
@racket[elif]s, and optionally an @racket[else] for if none of the
conditions holds.

First it evaluates the @racket[if] condition @syn[expr]@subscript{if}.
If non-false, it then evaluates @tech{block} @syn[block]@subscript{if}
and finishes. Otherwise, it evaluates each @racket[elif] condition
@syn[expr]@subscript{i} in turn; if each is false, it goes on to the
next, but when one is non-false then it finishes with the corresponding
@syn[block]@subscript{i}. Otherwise, if all of the conditions were false
and the optional @syn[block]@subscript{else} is included, evaluates
that.

For example, we can have an @racket[if] with no @racket[elif] or
@racket[else] parts:

@dssl2block|{
if should_greet:
    greet()
}|

The function @code{greet()} will be called if variable
@code{should_greet} is true, and otherwise it will not.

Or we can have several @racket[elif] parts:

@dssl2block|{
def rebalance_left_(key, balance, left0, right):
    let left = left0.node
    if !left0.grew?:
        insert_result(node(key, balance, left, right), False)
    elif balance == 1:
        insert_result(node(key, 0, left, right), False)
    elif balance == 0:
        insert_result(node(key, -1, left, right), True)
    elif left.balance == -1:
        insert_result(node(left.key, 0, left.left,
                           node(key, 0, left,right, right)),
                      False)
    elif left.balance == 1:
        insert_result(node(left.right.key, 0,
                           node(left.key,
                                -1 if left.right.balance == 1 else 0,
                                left.left,
                                left.right.left),
                           node(key,
                                1 if left.right.balance == -1 else 0,
                                left.right.right,
                                right)),
                      False)
    else: error('Cannot happen')
}|

@defsmplform{@defidform/inline[let] @syn[var_name] = @syn[expr]}

Declares and defines a local variable. Local variables may be declared in any
scope and last for that scope. A local variable may be re-assigned with the
assignment form (@racket[=]), as in the third line here:

@dssl2block|{
def sum(v):
    let result = 0
    for elem in v: result = result + elem
    return result
}|

@defsmplform{@redefidform/inline[let] @syn[var_name]}

Declares a local variable, which will be undefined until it is assigned:

@dssl2block|{
let x

if y:
    x = f()
else:
    x = g()

println(x)
}|

Accessing an undefined variable is an error.

@defcmpdform{@defidform/inline[for] @syn[var_name] @q{in} @syn[expr]: @syn[block]}

Loops over the values of the given @syn[expr], evaluating the
@tech{block} for each. The @syn[expr] can evaluate to a vector, a string,
or a natural number. If a vector, then this form iterates over the
values (not the indices) of the vector; if a string, this iterates over
the characters as 1-character strings; if a natural number @racket[n]
then it counts from @racket[0] to @racket[n - 1].

@dssl2block|{
for person in people_to_greet:
    println("Hello, ~a!", person)
}|

In this example hash function producer, the @racket[for] loops over the
characters in a string:

@dssl2block|{
# make_sbox_hash : -> [str? -> nat?]
# Returns a new n-bit string hash function.
def make_sbox_hash(n):
    let sbox = [ random_bits(n) for i in 256 ]
    def hash(input_string):
        let result = 0
        for c in input_string:
            let svalue = sbox[ord(c)]
            result = result ^ svalue
            result = (3 * result) % (2 ** n)
        return result
    hash
}|

@defcmpdform{@redefidform/inline[for] @syn[var_name]₁, @syn[var_name]₂ @q{in} @syn[expr]: @syn[block]}

Loops over the indices and values of the given @syn[expr], evaluating
the @tech{block} for each. The @syn[expr] can evaluate to a vector, a
string, or a natural number. If a vector, then @syn[var]₁
takes on the indices of the vector while @syn[var]₂ takes on
the values; if a string, then @syn[var]₁ takes on the
indices of the characters while @syn[var]₂ takes on the
characters; if a natural number then both variables count together.

@dssl2block|{
for ix, person in people_to_greet:
    println("~e: Hello, ~a!", ix, person)
}|

@defsmplform{@defidform/inline[pass]}

Does nothing.

@dssl2block|{
# account_credit! : num? account? -> VoidC
# Adds the given amount to the given account’s balance.
def account_credit!(amount, account):
    pass
#   ^ FILL IN YOUR CODE HERE
}|

@defsmplform{@defidform/inline[return] @syn[expr]}

Returns the value of the given @syn[expr] from the inner-most function.
Note that this is often optional, since the last expression in a
function will be used as its return value.

That is, these are equivalent:

@dssl2block|{
def inc(x): x + 1
}|

@dssl2block|{
def inc(x): return x + 1
}|

In this function, the first @racket[return] is necessary because it breaks out
of the loop and exits the function; the second @racket[return] is optional and
could be omitted.

@dssl2block|{
# : bloom-filter? str? -> bool?
def bloom_check?(b, s):
    for hash in b.hashes:
        let index = hash(s) % b.bv.size
        if !bv_ref(b.bv, index): return False
    return True
}|

@defsmplform{@redefidform/inline[return]}

Returns void from the current function.

@defcmpdform{@defidform/inline[test] @syn[expr]: @syn[block]}

Runs the code in @tech{block} as a test case named @syn[expr]
(which is optional). If an
assertion fails or an error occurs in @syn[block], the test case
terminates, failure is reported, and the program continues after the
@tech{block}.

For example:

@dssl2block|{
test "arithmetic":
    assert_eq 1 + 1, 2
    assert_eq 2 + 2, 4
}|

A @racket[test] @tech{block} can be used to perform just one check or a
long sequence of preparation and checks:

@dssl2block|{
test 'single-chaining hash table':
    let h = sch_new_1(10)
    assert !sch_member?(h, 'hello')

    sch_insert!(h, 'hello', 5)
    assert sch_member?(h, 'hello')
    assert_eq sch_lookup(h, 'hello'), 5
    assert !sch_member?(h, 'goodbye')
    assert !sch_member?(h, 'helo')

    sch_insert!(h, 'helo', 4)
    assert_eq sch_lookup(h, 'hello'), 5
    assert_eq sch_lookup(h, 'helo'), 4
    assert !sch_member?(h, 'hel')

    sch_insert!(h, 'hello', 10)
    assert_eq sch_lookup(h, 'hello'), 10
    assert_eq sch_lookup(h, 'helo'), 4
    assert !sch_member?(h, 'hel')
    assert_eq sch_keys(h), cons('hello', cons('helo', nil()))
}|

@defcmpdform{@defidform/inline[time] @syn[expr]: @syn[block]}

Times the execution of the @tech{block}, and then prints the results labeled
with the result of @syn[expr] (which isn’t timed, and which is optional).

For example, we can time how long it takes to create an array of
10,000,000 @racket[0]s:

@dssl2block|{
time '10,000,000 zeroes':
    [ 0; 10000000 ]
}|

The result is printed as follows:

@verbatim|{
10,000,000 zeroes: cpu: 309 real: 792 gc: 238
}|

This means it tooks 309 milliseconds of CPU time over 792 milliseconds of
wall clock time, with 238 ms of CPU time spent on garbage collection.

@defcmpdform{@defidform/inline[while] @syn[expr]: @syn[block]}

Iterates the @tech{block} while the @syn[expr] evaluates to non-false.
For example:

@dssl2block|{
while !is_empty(queue):
    explore(dequeue(queue))
}|

Here's a hash table lookup function that uses @racket[while], which it breaks
out of using @racket[break]:

@dssl2block|{
def sch_lookup(hash, key):
    let bucket = sch_bucket_(hash, key)
    let result = False
    while cons?(bucket):
        if key == bucket.first.key:
            result = bucket.first.value
            break
        bucket = bucket.rest
    return result
}|

@subsection[#:tag "exp-forms"]{Expression Forms}

@defexpform{@syn[var_name]}

The value of a variable, which must be a function parameter, bound with
@racket[let], or defined with @racket[def]. For example,

@dssl2block|{
let x = 5
println(x)
}|

prints “@code{5}”.

Lexically, a variable is a letter or underscore, followed by zero or
more letters, underscores, or digits, optionally ending in a question
mark or exclamation point.

@defexpform{@syn[struct_expr].@syn[field_name]}

Expression @syn[expr] must evaluate to struct value that has field
@syn[fieldname]; then this expression evaluates to the value of that
field of the struct.

@defexpform{@syn[vec_expr][@syn[index_expr]]}

Expression @syn[vec_expr] must evaluate to a vector @c{v}; @syn[index_expr]
must evaluate to an integer @c{n} between 0 and @code{len(v) - 1}.
Then this returns the @c{n}th element of vector @c{v}.

@defexpform{@defidform/inline[True]}

The true Boolean value.

@defexpform{@defidform/inline[False]}

The false Boolean value, the only value that is not considered true.

@defexpform{@syn[expr]@subscript{0}(@syn[expr]₁, ..., @syn[expr]@subscript{k})}

Evaluates all the expressions; then applies the result of
@syn[expr]@subscript{0} with the results of the other expressions as
arguments.

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

@defexpforms[
  @list{@defidform/inline[lambda] @syn[var_name]₁, ..., @syn[var_name]@subscript{k}: @syn[simple]}
  @list{@q{λ} @syn[var_name]₁, ..., @syn[var_name]@subscript{k}: @syn[simple]}
]

Creates an anonymous function with parameters @syn[var_name]₁, @c{...},
@syn[var_name]@subscript{k} and body @syn[simple]. For example, the function to
add twice its first argument to its second argument can be written

@dssl2block|{
lambda x, y: 2 * x + y
}|

@defexpform{@syn[true_expr] @q{if} @syn[cond_expr] @q{else} @syn[false_expr]}

The ternary expression first evaluates the condition
@syn[cond_expr]. If non-false,
evaluates @syn[true_expr] for its value; otherwise,
evaluates @syn[false_expr] for its value.

For example:

@dssl2block|{
def parent(link):
    link.parent if rbn?(link) else False
}|

@defexpform{@syn[struct_name] { @syn[field_name]₁: @syn[expr]₁, ..., @syn[field_name]@subscript{k}: @syn[expr]@subscript{k} }}

Constructs a struct with the given name and the values of the given
expressions for its fields. The struct must have been declared with
those fields using @racket[defstruct].

If a variable with the same name as a field is in scope, omitting the
field value will use that variable:

@dssl2block|{
defstruct Foo(bar, baz)

let bar = 4
let baz = 5

assert_eq Foo { bar, baz: 9 }, Foo(4, 9)
}|

@defexpform{@defidform/inline[object] @syn[struct_name] { @syn[field_name]₁: @syn[expr]₁, ..., @syn[field_name]@subscript{k}: @syn[expr]@subscript{k} }}

Creates a struct value without declaring the struct type with
@racket[defstruct]. In particular, creates a struct with the given name
@syn[struct_name] and the given fields and values, regardless of what
structs might be declared. The field names cannot have any repeats.

This is useful for one-off objects. For example, a simple 2-D point
object might be defined as:

@dssl2block|{
def Posn(x_, y_):
    def get_x(): x_
    def get_y(): y_
    def fmt(): format("(~e, ~e)", x_, y_)
    object Posn { get_x: get_x, get_y: get_y, fmt: fmt, }
}|

@defexpform{[ @syn[expr]@subscript{0}, ..., @syn[expr]@subscript{k - 1} ]}

Creates a new vector of length @c{k} whose values are the values
of the expressions.

For example:

@dssl2block|{
let v = [ 1, 2, 3, 4, 5 ]
}|

@defexpform{[ @syn[init_expr]; @syn[size_expr] ]}

Constructs a new vector whose length is the value of
@syn[size_expr], filled with the value of @syn[init_expr]. That is,

@dssl2block|{
[ 0; 5 ]
}|

means the same thing as

@dssl2block|{
[ 0, 0, 0, 0, 0 ]
}|

@defexpforms[
  @list{[ @syn[elem_expr] @q{for} @syn[var_name] @q{in} @syn[iter_expr] ]}
  @list{[ @syn[elem_expr] @q{for} @syn[var_name]₁, @syn[var_name]₂ @q{in} @syn[iter_expr] ]}
]

Vector comprehensions: produces a vector of the values of @syn[elem_expr]
while iterating the variable(s) over @syn[iter_expr]. In particular,
@syn[iter_expr] must be a vector @c{v}, a string @c{s}, or a
natural number @c{n}; in which case the iterated-over values are
the elements of @c{v}, the 1-character strings comprising
@c{s}, or counting from 0 to @code{n - 1}, respectively. If one
variable @syn[var_name] is provided, it takes on those values. If two are
provided, then @syn[var_name]₂ takes on those values, while @syn[var_name]₁
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
  @list{[ @syn[elem_expr] @q{for} @syn[var_name] @q{in} @syn[iter_expr] @q{if} @syn[cond_expr] ]}
  @list{[ @syn[elem_expr] @q{for} @syn[var_name]₁, @syn[var_name]₂ @q{in} @syn[iter_expr] @q{if} @syn[cond_expr] ]}
]

If the optional @syn[cond_expr] is provided, only elements for which
@syn[cond_expr] is non-false are included. That is, the variable(s) take on
each of their values, then @syn[cond_expr] is evaluated in the scope of the
variable(s). If it's non-false then @syn[elem_expr] is evaluated and
included in the resulting vector.

For example,

@dssl2block|{
[ 10 * n for n in [ 5, 4, 3, 2, 1 ] if odd?(n) ]
}|

evaluates to

@dssl2block|{
[ 50, 30, 10 ]
}|

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

@c{!}@syn[expr] evaluates @syn[expr], then returns @racket[True] if
the result was @racket[False], and @racket[False] for any other result.

@c{~}@syn[expr], @c{-}@syn[expr], and @c{+}@syn[expr] require
that @syn[expr] evaluate to a number. Then @c{~} flips every bit,
@c{-} negates it, and @c{+} returns it unchanged.

@defexpforms[
  @list{@syn[expr]₁ @defidform/inline[*] @syn[expr]₂}
  @list{@syn[expr]₁ @defidform/inline[/] @syn[expr]₂}
  @list{@syn[expr]₁ @defidform/inline[%] @syn[expr]₂}
]

Multiplies, divides, or modulos the values of the expressions, respectively.

@defexpform{@syn[expr]₁ @defidform/inline[+] @syn[expr]₂}

Addition:

@itemlist[
  @item{Given two numbers, adds them.}
  @item{Given two strings, concatenates them.}
  @item{Given a string and another value, in any order, converts
        the other value to a string and concatenates them.}
]

Anything else is an error.

@defexpform{@syn[expr]₁ @defidform/inline[-] @syn[expr]₂}

Subtracts two numbers.

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

@defprocform[proc?]{(AnyC) -> bool?}

Determines whether its argument is a procedure (function).

@defprocform[str?]{(AnyC) -> bool?}

Determines whether its argument is a string.

@defprocform[char?]{(AnyC) -> bool?}

Determines whether its argument is a string of length 1.

@defprocform[num?]{(AnyC) -> bool?}

Determines whether its argument is a number.

@defprocform[int?]{(AnyC) -> bool?}

Determines whether its argument is an integer.

@defprocform[float?]{(AnyC) -> bool?}

Determines whether its argument is a floating-point number.

@defprocform[vec?]{(AnyC) -> bool?}

Determines whether its argument is a vector.

@defprocform[bool?]{(AnyC) -> bool?}

Determines whether its argument is a bool?.

@defprocform[contract?]{(AnyC) -> bool?}

Determines whether its value is a contract. Contracts include many
constants (numbers, strings, Booleans), single-argument functions
(considered as predicates), and the results of contract combinators such
as @racket[OrC] and @racket[FunC].

@subsection{Numeric operations}

@defprocform[floor]{(num?) -> int?}

Rounds a number down to the largest integer that is no greater.

@defprocform[ceiling]{(num?) -> int?}

Rounds a number up to the smallest integer that is no less.

@defprocforms[int
    [@list{(num?) -> int?}]
    [@list{(str?) -> int?}]
    [@list{(bool?) -> int?}]
]

Returns the integer part of a number, by truncation. That is, the
decimal point and everything after it is removed. If given a string,
attempt to convert to a number before truncating, throwing an error if
the conversion fails. Booleans @racket[True] and @racket[False] convert
to @racket[1] and @racket[0], respectively.

@defprocforms[float
  [@list{(num?) -> float?}]
  [@list{(str?) -> float?}]
  [@list{(bool?) -> float?}]
]

Converts an exact integer to the nearest
double-precision floating point value. If given a string, attempt to
convert to a number, throwing an error if the conversion fails. Booleans
@racket[True] and @racket[False] convert to @racket[1.0] and @racket[0.0],
respectively.

@defprocforms[random
  [@list{() -> float?}]
  [@list{(IntInC(1, 4294967087)) -> nat?}]
  [@list{(int?, int?) -> nat?}]
]

When called with zero arguments, returns a random floating point number
in the open interval (@racket[0.0], @racket[1.0]).

When called with one argument @racket[limit], returns a random exact
integer from the closed interval [@racket[0], @racket[limit - 1]].

When called with two arguments @racket[min] and @racket[max], returns a
random exact integer from the closed interval [@racket[min], @racket[max - 1]].
The difference between the arguments can be no greater than
@racket[4294967087].

@defprocform[max]{(num?, num?, ...) -> num?}

Returns the largest of the given numbers.

@defprocform[min]{(num?, num?, ...) -> num?}

Returns the smallest of the given numbers.

@defprocform[quotient]{(nat?, nat?) -> nat?}

Returns the truncated quotient.

@defconstform[RAND_MAX]{nat?}

Defined to be @racket[4294967087], the largest parameter (or span) that
can be passed to @racket[random].

@defprocform[random_bits]{(nat?) -> nat?}

Returns a number consisting of the requested number of random bits.

@defprocform[remainder]{(nat?, nat?) -> nat?}

Returns the remainder of the truncated @racket[quotient].

@defprocform[sqrt]{(num?) -> float?}

Computes the square root of a number.

@subsubsection{Predicates}

@defprocform[zero?]{(num?) -> bool?}

Determines whether its argument is zero.

@defprocform[positive?]{(num?) -> bool?}

Determines whether its argument is greater than zero.

@defprocform[negative?]{(num?) -> bool?}

Determines whether its argument is less than zero.

@defprocform[even?]{(int?) -> bool?}

Determines whether its argument is an even integer.

@defprocform[odd?]{(int?) -> bool?}

Determines whether its argument is an odd integer.

@defprocform[nan?]{(num?) -> bool?}

Determines whether its argument is the IEEE 754 @racket[float?]
not-a-number value. This is useful, since @racket[nan] is not necessarily
@racket[==] to other instances of @racket[nan].

@subsection{String operations}

@defprocform[chr]{(nat?) -> str?}

Converts the code point of a character to the character that it
represents, as a one-character string. Inverse to @racket[ord].

@dssl2block|{
assert_eq chr(97), 'a'
}|

@defprocform[explode]{(str?) -> VectorOf[str?]}

Breaks a string into a vector of 1-character strings.

@defprocform[format]{(str?, AnyC, ...) -> str?}

Using its first argument as a template, interpolates the remaining
arguments, producing a string. The main recognized escape codes are
@c{~e} and @c{~a}. The former, @c{~e}, displays values the same way that
they are displayed in the interactions window, including quotation marks
around strings. The latter, @c{~a}, can be used to display strings
without quotation marks.

Additionally, @c{~n} can be used to insert a newline, and @c{~~}
inserts a literal @c{~}.

@defprocform[implode]{(VectorOf[str?]) -> str?}

Concatenates a vector of strings into a single string.

@defprocform[ord]{(str?) -> nat?}

Converts a character, represented as a one-character string, to its
code point. Inverse to @racket[chr].

@dssl2block|{
assert_eq ord('a'), 97
}|

@defprocform[strlen]{(str?) -> nat?}

Returns the length of a string in characters.

@subsection{Vector operations}

@defprocform[build_vector]{[X](n: nat?, f: FunC(nat?, X)) -> VectorOf[X]}

Creates a vector of size @c{n} whose elements are @code{f(0)},
@code{f(1)}, ..., @code{f(n - 1)}. Equivalent to

@dssl2block|{
[ f(x) for x in n ]
}|

@defprocform[filter]{[X](pred: FunC(X, bool?), v: VectorOf[X]) -> VectorOf[X]}

Returns a vector containing the elements of @c{v} for which
@c{pred} returns non-false. Equivalent to

@dssl2block|{
[ x for x in v if pred(x) ]
}|

@defprocform[len]{[X](VectorOf[X]) -> nat?}

Returns the length of a vector.

@defprocform[map]{[X, Y](f: FunC(X, Y), v: VectorOf[X]) -> VectorOf[X]}

Returns a vector consisting of @c{f} applied to each element of
@c{v}. Equivalent to

@dssl2block|{
[ f(x) for x in v ]
}|

@subsection{I/O Functions}

@defprocform[print]{(str?, AnyC, ...) -> Void}

The first argument is treated as a format string into which the
remaining arguments are interpolated, à la @racket[format]. Then the
result is printed.

@defprocform[println]{(str?, AnyC, ...) -> Void}

Like @racket[print], but adds a newline at the end.

@subsection{Other functions}

@defprocform[error]{(str?, AnyC, ...) -> Void}

Terminates the program with an error message. The error message must be
supplied as a format string followed by values to interpolate, in the
style of @racket[format].

@defprocform[identity]{[X](X) -> X}

The identity function, which just returns its argument.

@section{Contracts}

The contract system helps guard parts of a program against each other by
enforcing properties of function parameters, structure fields, and
variables. A number of DSSL2 values may be used as contracts, including:

@itemlist[
    @item{Numbers, which allow only themselves.}
    @item{Booleans, which allow only themselves.}
    @item{Strings, which allow only themselves.}
    @item{Functions of one argument, which are treated as flat
            contracts by applying as predicates.}
    @item{Contracts created using the contract combinators such as
            @racket[OrC] and @racket[FunC] described below.}
]

@subsection{Contract syntax}

@defcmpdform{@redefidform/inline[def] @syn_[name]{f}(@syn[name]₁: @syn[expr]₁, ... @syn_[name]{k}: @syn_[expr]{k}) -> @syn_[expr]{r}: @syn[block]}

Defines function @syn_[name]{f} while specifying contracts
@syn_[expr]{1} through @syn_[expr]{k} for the parameters, and contract
@syn_[expr]{r} for the result. For example:

@dssl2block|{
def pythag(x: num?, y: num?) -> num?:
    sqrt(x * x + y * y)
}|

Each of the contract positions is optional, and if omitted defaults to
@racket[AnyC].

@defcmpdform{@redefidform/inline[let] @syn[var_name] : @syn[contract_expr] = @syn[expr]}

Binds variable @syn[var_name] to the value of expression @syn[expr],
while applying the contract @syn[contract_expr]. Subsequent assignments
to the variable also must satisfy the contract. For example:

@dssl2block|{
let x : int? = 0
}|

Note that the @syn[expr] is optional, and the contract will not be
checked before the variable is assigned:

@dssl2block|{
let x : int?

x = 5
}|

@defsmplform{@redefidform/inline[defstruct] @syn[name](@syn_[name]{1}: @syn_[expr]{1}, ..., @syn_[name]{k}: @syn_[expr]{k})}

Defines a structure @syn[name] with the given contracts @syn_[expr]{i}
applied to the fields @syn_[name]{i}. This means that the contracts will
be applied both when constructing the structure and when mutating it.
For example:

@dssl2block|{
defstruct posn(x: float?, y: float?)
}|

Now constructing a @code{posn} will require both parameters to satisfy
the @racket[float?] predicate, as will assigning to either field.

It’s possible to include contracts on some fields without including them
on all, and the fields with omitted contracts default to @racket[AnyC].

@subsection{Contract combinators}

@defconstform[AnyC]{contract?}

A flat contract that accepts any value.

@defconstform[VoidC]{contract?}

A flat contract that accepts the result of @racket[pass] and other
statements that return no value (such as assignment and loops).

@defprocform[OrC]{(contract?, contract, ...) -> contract?}

Creates a contract that accepts a value if any of the arguments does.
For the details of how this works for higher-order contracts, see
@racket[racket:or/c].

@defprocform[AndC]{(contract?, contract, ...) -> contract?}

Creates a contract that accepts a value if all of the arguments do.
For the details of how this works for higher-order contracts, see
@racket[racket:and/c].

@defprocform[FunC]{(contract?, ..., contract?) -> contract?}

Creates a function contract with the given arguments and result. The
last argument is applied to the result, and all the other arguments are
contracts applied to the parameters.

@defprocform[NewForallC]{(str?) -> contract?}

Creates a new, universal contract variable, useful in constructing
parametric contracts.

@defprocform[NewExistsC]{(str?) -> contract?}

Creates a new, existential contract variable, useful in constructing
parametric contracts.

@defprocform[IntInC]{(low: OrC(int?, False), high: OrC(int?, False)) -> contract?}

Constructs a contract that accepts integers in the closed interval
[@c{low}, @c{high}]. If either end of the interval is @code{False},
that end of the interval is unchecked.

@defprocforms[apply_contract
    [@list{[X](contract?, X) -> X}]
    [@list{[X](contract?, X, pos: str?) -> X}]
    [@list{[X](contract?, X, pos: str?, neg: str?) -> X}]
]

Applies a contract to a value, optionally specifying the parties.
