#lang scribble/manual

@(require
        "helpers.rkt"
        (for-label dssl2)
        (for-label (prefix-in racket: racket)))

@title[#:style 'toc]{DSSL2: Data Structures Student Language}
@author{Jesse A. Tov <jesse@"@"eecs.northwestern.edu>}

@defmodulelang[dssl2]

@section{Syntax introduction}

@subsection{Compound statements and blocks}

DSSL2 uses alignment and indentation to delimit @deftech{blocks}. In
particular, compound statements such as
@racket[if]-@racket[elif]-@racket[else] take @nt[block]s for each
condition, where a @nt[block] can be either one simple statement
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

@subsection{Formal grammar}

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
  [simple    (assert expr)
             (assert_eq expr "," expr)
             (assert_error expr (~opt "," expr))
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
  [mod_spec
            'mod_name
            'mod_string]
  [compound  (class 'name opt_ctc_params opt_implements : class_block)
             (def 'name opt_ctc_params "(" (~many-comma 'name opt_ctc) ")"
               opt_res_ctc : block)
             (if expr ":" block
                 (~many elif expr ":" block)
                 (~opt else ":" block))
             (interface 'name opt_ctc_params ":" interface_block)
             (for (~opt 'name ",") 'name "in" expr ":" block)
             (struct 'name ":" struct_block)
             (test (~opt expr) ":" block)
             (time (~opt expr) ":" block)
             (while expr ":" block)]
  [block     (simple 'NEWLINE)
             ('NEWLINE 'INDENT (~many1 statement) 'DEDENT)]
  [class_block
             ('NEWLINE 'INDENT class_fields class_methods 'DEDENT)]
  [class_fields
             (~many field_def 'NEWLINE)]
  [class_methods
             (~many1 method_proto ":" block)]
  [interface_block
            pass
            ('NEWLINE 'INDENT (~many1 method_proto 'NEWLINE) 'DEDENT)]
  [method_proto
            (def 'name opt_ctc_params "(" 'name (~many "," 'name opt_ctc) ")" opt_res_ctc)]
  [struct_block
            pass
            ('NEWLINE 'INDENT (~many1 field_def 'NEWLINE) 'DEDENT)]
  [field_def
            (let 'name opt_ctc)]
  [opt_implements
            (~opt "(" (~many-comma 'name) ")")]
  [opt_ctc
            (~opt ":" ctc)]
  [opt_res_ctc
            (~opt "->" ctc)]
  [opt_ctc_params
            (~opt "[" (~many-comma 'name) "]")]
  [ctc      expr]
  [expr     'number
            'string
            True
            False
            lvalue
            (expr "if" expr "else" expr)
            (expr "(" (~many-comma expr) ")")
            (lambda (~many-comma 'name) ":" simple)
            ("λ" (~many-comma 'name) ":" simple)
            ('struct_name "{" (~many-comma 'name ":" expr) "}")
            ("[" (~many-comma expr) "]")
            ("[" expr ";" expr "]")
            ("[" expr "for" (~opt 'name ",") 'name "in" expr (~opt "if" expr) "]")
            (expr 'binop expr)
            ('unop expr)]
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

@section{Lexical syntax}

@subsection{Identifiers}

@italic{Name}s, used for variables, functions, structs, classes,
interfaces, fields, and methods, must start with a letter, followed by 0
or more letters or digits. The last character also may be @q{?} or
@q{!}.

@subsection{Numeric literals}

Numeric literals include:

@itemlist[
  @item{Decimal integers: @racket[0], @racket[3], @racket[18446744073709551617]}
  @item{Hexadedecimal, octal, and binary integers: @q{0xFFFF00},
      @q{0o0177}, @q{0b011010010}}
  @item{Floating point: @racket[3.5], @q{6.02E23}, @racket[1e-12], @racket[inf],
  @racket[nan]}
]

@subsection{String literals}

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

@subsection{Comments}

A comment in DSSL2 starts with the @q{#} character and continues to the
end of the line.

Long string literals can also be used to comment out long blocks of code.

@section[#:tag "stm-forms"]{Statement forms}

@subsection{Definition and assignment forms}

@defsmplform{@defidform/inline[let] @term[var_name] = @nt[expr]}

Declares and defines a local variable. Local variables may be declared in any
scope and last for that scope. A local variable may be re-assigned with the
assignment form (@racket[=]), as in the third line here:

@dssl2block|{
def sum(v):
    let result = 0
    for elem in v: result = result + elem
    return result
}|

@defsmplform{@redefidform/inline[let] @term[var_name]}

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

@defsmplform{@nt[lvalue] @defidform/inline[=] @nt_[expr]{rhs}}

Assigns the value of @nt_[expr]{rhs} to an @nt[lvalue].
The assigned @nt[lvalue] can be in one of three forms:

@itemlist[
 @item{@term[var_name] assigns to a variable, which can be a
 @racket[let]-defined variable or a function parameter.}
 @item{@c{@nt_[expr]{struct}.@term[field_name]} assigns to a structure field,
 where expression @nt_[expr]{struct} must evaluate to a structure that
 has a field named @term[field_name].}
 @item{@c{@nt_[expr]{vec}[@nt_[expr]{index}]} assigns to a vector element,
 where
 @nt_[expr]{vec} evaluates to the vector and @nt_[expr]{index}
 evaluates to the index of the element.}
]

This method assigns all three kinds of l-value:

@dssl2block|{
def insert!(self, key, value):
    let index = self._bucket_index_(key)
    let current = self._buckets[index]
    while cons?(current):
        if key == current.first.key:
            # struct assignment:
            current.first.value = value
            return
        # variable assignment:
        current = current.rest
    # vector assignment:
    self._buckets[index] = cons(sc_entry(key, value), self._buckets[index])
}|

@defcmpdform{@defidform/inline[def] @term[fun_name](@term_[var_name]{1}, ... @term_[var_name]{k}): @nt[block]}

Defines @term[fun_name] to be a function with formal parameters
@term_[var_name]{1}, @c{...}, @term_[var_name]{k} and with body
@nt[block].

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

The body of a function is defined to be a @nt[block], which means it can
be an indented sequence of @nt[statement]s, or a single @nt[simple]
statement on the same line as the @racket[def].

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
            if link is p.left: p.right
            else: p.left
        else: False

    # aunt : RbLinkOf[X] -> RbLinkOf[X]
    def aunt(link):
        sibling(parent(link))

    # . . .

    def set_root!(new_node): tree.root = new_node
    search!(tree.root, set_root!)
}|

@subsection{Loop and control forms}

@defsmplform{@defidform/inline[pass]}

Does nothing.

@dssl2block|{
# account_credit! : num? account? -> VoidC
# Adds the given amount to the given account’s balance.
def account_credit!(amount, account):
    pass
#   ^ FILL IN YOUR CODE HERE
}|

@defcmpdforms[
    [@list{@defidform/inline[if] @nt_[expr]{if}: @nt_[block]{if}}]
    [@~many[@defidform/inline[elif] @list{@nt_[expr]{elif}:} @nt_[block]{elif}]]
    [@~opt[@list{@defidform/inline[else]:} @nt_[block]{else}]]
]

The DSSL2 conditional statement contains an @racket[if], 0 or more
@racket[elif]s, and optionally an @racket[else] for if none of the
conditions holds.

First it evaluates the @racket[if] condition @nt_[expr]{if}.
If non-false, it then evaluates block @nt_[block]{if}
and finishes. Otherwise, it evaluates each @racket[elif] condition
@nt_[expr]{elif} in turn; if each is false, it goes on to the
next, but when one is non-false then it finishes with the corresponding
@nt_[block]{elif}. Otherwise, if all of the conditions were false
and the optional @nt_[block]{else} is included, evaluates
that.

For example, we can have an @racket[if] with no @racket[elif] or
@racket[else] parts:

@dssl2block|{
if should_greet:
    greet()
}|

The function @code{greet()} will be called if variable
@code{should_greet} is non-false, and otherwise it will not.

Or we can have several @racket[elif] parts:

@dssl2block|{
def rebalance_left_(key, balance, left0, right):
    let left = left0.node
    if not left0.grew?:
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

@defcmpdform{@defidform/inline[for] @term[var_name] in @nt[expr]: @nt[block]}

Loops over the values of the given @nt[expr], evaluating the
@nt[block] can evaluate to a vector, a string,
or a natural number. If a vector, then this form iterates over the
element values (not the indices) of the vector; if a string, this iterates over
the characters; if a natural number @racket[n]
then it counts from @racket[0] to @racket[n - 1].

@dssl2block|{
for person in people_to_greet:
    println("Hello, %s!", person)
}|

In this example hash function producer, the @racket[for] loops over the
characters in a string:

@dssl2block|{
# make_sbox_hash : -> [str? -> nat?]
# Returns a new n-bit string hash function.
def make_sbox_hash(n):
    let sbox = [ random_bits(n) for _ in 256 ]
    def hash(input_string):
        let result = 0
        for c in input_string:
            let svalue = sbox[int(c) % 256]
            result = result ^ svalue
            result = (3 * result) % (2 ** n)
        return result
    hash
}|

@defcmpdform{@redefidform/inline[for] @term_[var_name]{1}, @term_[var_name]{2} in @nt[expr]: @nt[block]}

Loops over the indices and values of the given @nt[expr], evaluating
the @nt[block] for each. The @nt[expr] can evaluate to a vector, a
string, or a natural number. If a vector, then @term_[var]{1}
takes on the indices of the vector while @term_[var]{2} takes on
the values; if a string, then @term_[var]{1} takes on the
indices of the characters while @term_[var]{2} takes on the
characters; if a natural number then both variables count together.

@dssl2block|{
for ix, person in people_to_greet:
    println("%p: Hello, %s!", ix, person)
}|

@defcmpdform{@defidform/inline[while] @nt[expr]: @nt[block]}

Iterates the @nt[block] while the @nt[expr] evaluates to non-false.
For example:

@dssl2block|{
while not queue.empty?():
    explore(queue.dequeue())
}|

Here's a hash table lookup method that uses @racket[while], which it breaks
out of using @racket[break]:

@dssl2block|{
def lookup(self, key):
    let bucket = self._find_bucket(key)
    let result = False
    while cons?(bucket):
        if key == bucket.first.key:
            result = bucket.first.value
            break
        bucket = bucket.rest
    return result
}|

@defsmplform{@defidform/inline[break]}

When in a @racket[for] or @racket[while] loop, ends the (inner-most)
loop immediately.

@defsmplform{@defidform/inline[continue]}

When in a @racket[for] or @racket[while] loop, ends the current
iteration of the (inner-most) loop and begins the next iteration.

@defsmplform{@nt[expr]}

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

@defsmplform{@defidform/inline[return] @nt[expr]}

Returns the value of the given @nt[expr] from the inner-most function.
Note that this is often optional, since the last expression in a
function will be used as its return value.

That is, these are equivalent:

@dssl2block|{
def inc(x): x + 1
}|

@dssl2block|{
def inc(x): return x + 1
}|

In this method, the first @racket[return] is necessary because it breaks out
of the loop and exits the function; the second @racket[return] is optional and
could be omitted.

@dssl2block|{
# : bloom-filter? str? -> bool?
def bloom_check?(self, s):
    for hash in self._hashes:
        let index = hash(s) % self._bv.len()
        if not self._bv[index]: return False
    return True
}|

@defsmplform{@redefidform/inline[return]}

Returns void from the current function.

@subsection{Data and program structuring forms}

@defcmpdforms[
    [@list{@defidform/inline[struct] @term[name]:}]
    [@indent{@redefidform/inline[let] @term_[field_name]{1}}]
    [@indent{...}]
    [@indent{@redefidform/inline[let] @term_[field_name]{k}}]
]

Defines a new structure type @term[struct_name] with fields given by
@term_[field_name]{1}, @c{...}, @term_[field_name]{k}. For example,
to define a struct @racket[posn] with fields @racket[x] and @racket[y],
we write:

@dssl2block|{
struct posn:
    let x
    let y
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
struct Node:
    let key
    let size
    let left
    let right

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

@defcmpdforms[
    [@list{@defidform/inline[class] @term[name] @~opt["(" @~many-comma[@term[interface_name]] ")"]}]
    [@indent{@redefidform/inline[let] @term_[field_name]{1}}]
    [@indent{...}]
    [@indent{@redefidform/inline[let] @term_[field_name]{k}}]
    [@indent{@redefidform/inline[def] @term_[meth_name]{0}(@term_[self]{0} @~many["," @term_[param_name]{0}]): @nt_[block]{0}}]
    [@indent{...}]
    [@indent{@redefidform/inline[def] @term_[meth_name]{n}(@term_[self]{n} @~many["," @term_[param_name]{n}]): @nt_[block]{n}}]
]

Defines a class named @term[name] with private fields
@term_[field_name]{1} through @term_[field_name]{k}, and methods
@term_[meth_name]{0} through @term_[meth_name]{n}. Defining a class
defines both a constructor function @term[name] and a type predicate
@c{@term[name]?}.

If optional @term[interface_name]s are given,
this form checks that the class implements those interfaces.
See @racket[interface] for more information
on how interfaces work.

A class has zero or more fields, which cannot be accessed from outside
the class. Fields may be accessed from methods via the “self” parameter,
as explained below.

A class has one or more methods. Methods are public and can be
accessed from outside the class, except for methods whose names begin
with a single underscore, which are private. Each method takes a
distinguished first parameter to refer to the instance of the object on
which it was called (the “self” parameter, also known as the receiver),
and zero or more other parameters. Inside a method, a field
@term[field_name] may be accessed as @c{@term[self].@term[field_name]},
where @term[self] is the name of that method's self parameter. Other
methods may also be called via the self parameter, and the self may be
returned or passed to other functions or methods.

To call a method from either inside or outside the class, we use dot
notation, writing the receiver, then the method names, and then the
non-self parameters:
@c{@term[object].@term[meth_name](@term[arg], ...)}.

Every class must have an “internal” constructor method named
@c{__init__}, which takes a self parameter and any other values needed
to initialize the class. The internal constructor method @emph{must}
initialize all the fields before it returns. Instances of a class may be
constructed via the “external” constructor function, whose name is the
same as the name of the class. It takes one fewer parameter than
@code{__init__}, because it is not passed a self parameter. DSSL2
arranges to create the new object and pass it to @code{__init__} for
initialization.

Here is an example of a simple class with one field and four methods:

@dssl2block|{
import cons

class Stack:
    let head

    def __init__(self):
        self.head = nil()

    def push(self, value):
        self.head = cons(value, self.head)

    def pop(self):
        self._check_non_empty()
        let result = self.head.car
        self.head = self.head.cdr
        result

    # Private helper method for emptiness check:
    def _check_non_empty(self):
        if nil?(self.head): error('Stack.pop: empty')
}|

Note that the internal constructor method @code{__init__} takes only a self
parameter, which means that the external constructor function @code{Stack}
takes none. Thus, we can use the constructor to create a new, empty
stack, and then we can access the stack via its @code{push} and @code{pop}
methods:

@dssl2block|{
let stack = Stack()
stack.push(4)
stack.push(5)
assert_eq stack.pop(), 5
assert_eq stack.pop(), 4
assert_error stack.pop()
}|

As an example of a class with a non-trivial constructor, here is a 2-D
position class that can change only in the @emph{y} dimension:

@dssl2block|{
class VerticalMovingPosn:
    let x
    let y

    def __init__(self, x, y):
        self.x = x
        self.y = y

    def get_x(self): self.x

    def get_y(self): self.y

    def set_y!(self, y): self.y = y
}|

We can create a @code{VerticalMovingPosn} as follows:

@dssl2block|{
let posn = VerticalMovingPosn(3, 4)
assert_eq posn.get_x(), 3
assert_eq posn.get_y(), 4
posn.set_y!(10)
assert_eq posn.get_x(), 3
assert_eq posn.get_y(), 10
}|

Note that @code{VerticalMovingPosn} takes two parameters because
@code{__init__} takes three: the self parameter and two more.

@defcmpdforms[
    [@list{@defidform/inline[interface] @term[name]:}]
    [@indent{@redefidform/inline[def] @term_[meth_name]{1}(@term_[self]{1} @~many["," @term_[param_name]{1}]}]
    [@indent{...}]
    [@indent{@redefidform/inline[def] @term_[meth_name]{k}(@term_[self]{k} @~many["," @term_[param_name]{k}])}]
]

Defines an interface named @term[name] with methods @term_[meth_name]{1}
through @term_[meth_name]{k}. Defining an interface binds three
identifiers:

@itemlist[
    @item{The interface named itself, @term[name], which can be mentioned
      in a @racket[class] to check the class against that interface.}
    @item{The interface predicate, @c{@term[name]?}, which checks for
      objects whose classes are declared to implement the interface.}
    @item{The @tech{interface contract}, @c{@term[name]!}, which modifies
      a protected object to disable methods not present in the interface.
      See @secref{Contracts} for more information on contracts.}
]

An interface specifies some number of methods, their names, and their
arities (numbers of parameters). It can then be used to check that a
class @emph{implements} the interface, meaning that it provides methods
with those names and arities.

For example, consider an interface for a simple container that supports
adding and removing elements, and checking whether the container is full:

@dssl2block|{
interface CONTAINER:
    def empty?(self)
    def full?(self)
    def add(self, value)
    def remove(self)
}|

The interface specifies a class with four methods:

@itemlist[
    @item{@c{empty?}, taking no non-self arguments,}
    @item{@c{full?}, taking no non-self arguments,}
    @item{@c{add}, taking one non-self argument, and}
    @item{@c{remove}, taking non-non-self arguments.}
]

(Note that the parameter names themselves don't matter—all that matters
is how many.)

We can implement the @c{CONTAINER} interface multiple ways. For example,
here we consider two classes that do so.

First, the @c{Cell} class implements a container with room for one
element, initially empty:

@dssl2block|{
class Cell (CONTAINER):
    let _contents
    let _empty?

    def __init__(self):
        self._contents = False
        self._empty?   = True

    def empty?(self):
        self._empty?

    def full?(self):
        not self.empty?()

    def add(self, value):
        if self.full?(): error("Cell.add: full")
        self._contents = value
        self._full? = True

    def remove(self):
        if self.empty?(): error("Cell.remove: empty")
        self._full? = False
        self._contents
}|

Second, the @c{VecStack} class implements a fixed-size stack
using a vector:

@dssl2block|{
class VecStack (CONTAINER):
    let _data
    let _len

    def __init__(self, capacity):
        self._data = [False; capacity]
        self._len  = 0

    def capacity(self):
        self._data.len()

    def len(self):
        self._len

    def empty?(self):
        self.len() == 0

    def full?(self):
        self.len() == self.capacity()

    def add(self, value):
        if self.full?(): error('VecStack.add: full')
        self._data[self._len] = value
        self._len = self._len + 1

    def remove(self):
        if self.empty?(): error('VecStack.remove: empty')
        size._len = self._len - 1
        let result = self._data[self._len]
        self._data[self._len] = False
        result
}|

Both classes @c{Cell} and @c{VecStack} implement the methods of
interface @c{CONTAINER} with the correct arities, so their definitions
succeed. Notice that @c{VecStack} has additional methods not mentioned
in the interface—this is okay! Because both classes @c{Cell} and
@c{VecStack} implement @c{CONTAINER}, @c{CONTAINER?} will answer
@code{True} for their instances.

Furthermore, instances of either class can be protected with the
@c{CONTAINER!} @tech{interface contract}, which disables methods that are
not declared in @c{CONTAINER}.

If a class does not implement the methods of a declared interface, it is
a static error. For example, consider this position class:

@dssl2block|{
class Posn (CONTAINER):
    let _x
    let _y

    def __init__(self, x, y):
        self._x = x
        self._y = y

    def x(self): _x
    def y(self): _y
}|

The definition of @c{Posn} is in error, because it does not implement the
methods of @c{CONTAINER}.

@defsmplforms[
    [@list{@defidform/inline[import] @term[mod_name]}]
    [@list{@redefidform/inline[import] @term[mod_string]}]
]

Imports the specified DSSL2 module. Modules may be from the standard
library, in which case the unquoted @term[mod_name] is used, or from the
current directory, in which case @term[mod_string] should be the name of
the file as a string literal.

A DSSL2 module is a @c{.rkt} file starting with @c{#lang dssl2} and
consisting of a sequence of DSSL2 @nt[statement]s. All definitions not
starting with a single underscore are public, and may be imported into
another DSSL2 module. The @racket[import] form may only be used at the
top level of a module.

@subsection{Testing and timing forms}

@defsmplform{@defidform/inline[assert] @nt[expr]}

Asserts that the given @nt[expr] evaluates to non-false. If the
expression evaluates false, signals an error.

@dssl2block|{
test "ScHash.member? finds 'hello'":
    let h = ScHash(10, make_sbox_hash())
    assert not h.member?('hello')
    h.insert!('hello', 5)
    assert h.member?('hello')
}|

@defsmplform{@defidform/inline[assert_eq] @nt_[expr]{1}, @nt_[expr]{2}}

Asserts that the given @nt[expr]s evaluates to structurally equal values.
If they are not equal, signals an error.

@dssl2block|{
test 'first_char_hasher':
    assert_eq first_char_hasher(''), 0
    assert_eq first_char_hasher('A'), 65
    assert_eq first_char_hasher('Apple'), 65
    assert_eq first_char_hasher('apple'), 97
}|

@defsmplform{@defidform/inline[assert_error] @nt_[expr]{fail}, @nt_[expr]{str}}

Asserts that the given @nt_[expr]{fail} errors, and that the error message
contains the substring that results from evaluating @nt_[expr]{str}.

@defsmplform{@redefidform/inline[assert_error] @nt[expr]}

Asserts that the given @nt[expr] errors without checking for a
particular error.

@defcmpdform{@defidform/inline[test] @nt[expr]: @nt[block]}

Runs the code in @nt[block] as a test case named @nt[expr]
(which is optional). If an
assertion fails or an error occurs in @nt[block], the test case
terminates, failure is reported, and the program continues after the
block.

For example:

@dssl2block|{
test "arithmetic":
    assert_eq 1 + 1, 2
    assert_eq 2 + 2, 4
}|

A @racket[test] @nt[block] can be used to perform just one check or a
long sequence of preparation and checks:

@dssl2block|{
test 'single-chaining hash table':
    let h = ScHash(10)
    assert not h.member?('hello')

    h.insert!('hello', 5)
    assert h.member?('hello')
    assert_eq h.lookup('hello'), 5
    assert not h.member?('goodbye')
    assert not h.member?('helo')

    h.insert!('helo', 4)
    assert_eq h.lookup('hello'), 5
    assert_eq h.lookup('helo'), 4
    assert not h.member?('hel')

    h.insert!('hello', 10)
    assert_eq h.lookup('hello'), 10
    assert_eq h.lookup('helo'), 4
    assert not h.member?('hel')
    assert_eq h.keys(h), cons('hello', cons('helo', nil()))
}|

@defcmpdform{@defidform/inline[time] @nt[expr]: @nt[block]}

Times the execution of the @nt[block], and then prints the results labeled
with the result of @nt[expr] (which isn’t timed, and which is optional).

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

@section[#:tag "exp-forms"]{Expression forms}

@subsection{Variable expressions}

@defexpform{@term[var_name]}

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

@subsection{Literal expressions}

@defexpform{@term[number]}

A numeric literal.

@defexpform{@term[string]}

A string literal.

@defexpform{@defidform/inline[True]}

The true Boolean value.

@defexpform{@defidform/inline[False]}

The false Boolean value, the only value that is not considered true.

@subsection{Functions and application expressions}

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

@subsection{Vectors and indexing expressions}

@defexpform{@nt_[expr]{1}[@nt_[expr]{2}]}

Expression @nt_[expr]{1} must evaluate to a vector @c{v} or string @c{s};
@nt_[expr]{2} must evaluate to an integer @c{n} between 0 and
@code{v.len() - 1}. Then this returns the @c{n}th element of vector
@c{v} or the @c{n}th character of string @c{s}.

@defexpform{[ @nt_[expr]{0}, ..., @nt_[expr]{k - 1} ]}

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

@defexpforms[
  @list{[ @nt_[expr]{elem} for @term[var_name] in @nt_[expr]{iter} ]}
  @list{[ @nt_[expr]{elem} for @term_[var_name]{1}, @term_[var_name]{2} in @nt_[expr]{iter} ]}
]

Vector comprehensions: produces a vector of the values of @nt_[expr]{elem}
while iterating the variable(s) over @nt_[expr]{iter}. In particular,
@nt_[expr]{iter} must be a vector @c{v}, a string @c{s}, or a
natural number @c{n}; in which case the iterated-over values are
the elements of @c{v}, the 1-character strings comprising
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

@subsection{Structs and projection expressions}

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

@subsection{Operator expressions}

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
the result was @code{False}, and @code{False} for any other result.

@c{~}@nt[expr], @c{-}@nt[expr], and @c{+}@nt[expr] require
that @nt[expr] evaluate to a number. Then @c{~} flips every bit,
@c{-} negates it, and @c{+} returns it unchanged.

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

Bitwise and.

@defexpform{@nt_[expr]{1} @defidform/inline[^] @nt_[expr]{2}}

Bitwise xor.

@defexpform{@nt_[expr]{1} @defidform/inline[\|] @nt_[expr]{2}}

Bitwise or. (Not written with the backslash.)

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

Operator @racket[==] is structural equality, and @racket[!=] is its
negation. Operator @racket[is] is physical equality, and @racket[|is not|]
(not written with the vertical bars)
is its negation. To understand the difference, suppose that we create
two different vectors with the same contents. Those vectors are
structurally equal but not physically equal.

Operators @racket[<], @racket[<=], @racket[>], and @racket[>=] are the
standard inequalities for numbers, and compare pairs of strings in
lexicographic order.

@defexpform{@nt_[expr]{1} @defidform/inline[and] @nt_[expr]{2}}

Short-circuiting logical and. First evaluates @nt_[expr]{1}; if the result
is @code{False} then the whole conjunction is @code{False};
otherwise, the result of the conjunction is the result of @nt_[expr]{2}.

@defexpform{@nt_[expr]{1} @defidform/inline[or] @nt_[expr]{2}}

Short-circuiting logical or. First evaluates @nt_[expr]{1}; if the result
is non-false then the whole disjunction has that result; otherwise the
result of the conjunction is the result of @nt_[expr]{2}.

@defexpform{@nt_[expr]{then} if @nt_[expr]{cond} else @nt_[expr]{else}}

The ternary expression first evaluates the condition
@nt_[expr]{cond}. If non-false,
evaluates @nt_[expr]{then} for its value; otherwise,
evaluates @nt_[expr]{else} for its value.

For example:

@dssl2block|{
def parent(link):
    link.parent if rbn?(link) else False
}|

@section[#:tag "prims"]{Built-in functions, classes, methods, and constants}

@subsection{Primitive classes}

DSSL2 includes seven primitive classes for building up more complex data
structures. The constructors and methods of these classes are documented
in this subsection.

@defclassform[bool]

The primitive class for Boolean values, @code{True} and
@code{False}. The type predicate for @linkclass[bool] is
@racket[bool?].

Booleans support logical binary operators @racket[&] (and), @racket[\|]
(or, written without the backslash), and @racket[^] (xor), and logical
unary negation @racket[~]. They also support comparison with
@racket[==], @racket[<], @racket[<=], etc. @code{False} compares less
than @code{True}.

@defprocforms[bool
    [@list{(@code{AnyC}) -> @code{bool?}}]
    [@list{() -> @code{bool?}}]
]

The constructor for @linkclass[bool].

In its one-argument form, converts any type to @racket[bool]. All values but
@code{False} convert to @code{True}.

In its no-argument form, returns @code{False}.

@defclassform[char]

The primitive class for representing a single character of text.
The type predicate for @linkclass[char] is @racket[char?].

A character can be converted to its integer value with the @racket[int]
constructor.

@defprocforms[char
    [@proto[char bool?]]
    [@proto[int? bool?]]
    [@proto[str? bool?]]
    [@proto[bool?]]
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
@racket[-] (subtraction), @racket[*] (multiplication), and @racket[/]
(integer division); when combined with an instance of @linkclass[float],
the result will also be a float. They also support unary @racket[+]
(identity) and @racket[-] (negation).

They also support comparison with @racket[==], @racket[<], @racket[<=],
etc., and they can be compared against floats.

Integers support binary bitwise operators @racket[&] (bitwise and),
@racket[\|] (bitwise or), and @racket[^] (bitwise xor), and unary
bitwise negation @racket[~].

@defprocforms[int
    [@proto[num? int?]]
    [@proto[char? int?]]
    [@proto[str? int?]]
    [@proto[bool? int?]]
    [@proto[int?]]
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
  [@proto[num? float?]]
  [@proto[str? float?]]
  [@proto[bool? float?]]
  [@proto[float?]]
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

@defclassform[proc]

The primitive class for representing functions.
The type predicate for @linkclass[proc] is @racket[proc?].

Procedures can be applied.

@defprocforms[proc
    [@proto[proc? proc?]]
    [@proto[proc?]]
]

The constructor for @linkclass[proc].

Given one procedure argument, returns it unchanged. Given no arguments,
returns the identity function.

@defmethform[proc compose]{@proto[proc? proc?]}

Composes the receiving procedure with the parameter procedure. For example,

@dssl2block|{
assert_eq (λ x: x + 1).compose(λ x: x * 2)(5), 11
}|

@defmethform[proc vec_apply]{@proto[vec? AnyC]}

Applies the receiving procedure using the contents of the given vector
as its arguments.

@defclassform[str]

The primitive class for representing textual data.
The type predicate for @linkclass[str] is @racket[str?].

A string can be indexed with square bracket notation, which returns an
instance of @linkclass{char}. Strings may be concatenated with the
@racket[+] operator. Concatenating a string with a non-string using
@racket[+] converts the non-string to a string first.

@defprocforms[str
    [@proto[str? str?]]
    [@proto[AnyC str?]]
    [@proto[len:nat? c:char? str?]]
    [@proto[str?]]
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

@defmethform[str explode]{@proto["VecC(char?)"]}

Breaks the receiving string into a vector of its characters.

@defmethform[str format]{@proto[AnyC ... str?]}

Formats the given arguments, treating the receiving string
as a format string in the style of @racket[print].

For example,

@dssl2block|{
assert_eq '%s or %p'.format('this', 'that'), "this or 'that'"
}|

@defmethform[str len]{@proto[nat?]}

Returns the length of the receiving string in characters.

@defclassform[vec]

The primitive vector class, for representing sequence of values of fixed size.
The type predicate for @linkclass[vec] is @racket[vec?].

A vector can be indexed and assigned with square bracket notation.

@defprocforms[vec
    [@proto[vec?]]
    [@proto[len:nat?  vec?]]
    [@proto[len:nat? "init:FunC(nat?, AnyC)" vec?]]
]

The constructor for @linkclass[vec].

@itemlist[
@item{
Given no arguments, returns an empty, zero-length vector.
}

@item{
Given one argument, returns a vector of the given size, filled with
@code{False}.
}

@item{
Given two arguments, returns a vector of the given size, with each
element initialized by applying the given function to its index.
That is, @code{vec(len, init)} is equivalent to
@code{[ init(i) for i in len ]}.
}
]

@defmethform[vec filter]{@proto["pred?:FunC(AnyC, AnyC)" vec?]}

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

@defmethform[vec map]{@proto["f:FunC(AnyC, AnyC)" vec?]}

Maps a function over a vector, returning a new vector.

In particular,
@code|{
v.map(f)
}|
is equivalent to
@code|{
[ f(x) for x in v ]
}|

@subsection{Predicates}

@subsubsection{Basic type predicates}

@defprocform[bool?]{@proto[AnyC bool?]}

Determines whether its argument is a Boolean, that is
an instance of @linkclass[bool].

@defprocform[char?]{@proto[AnyC bool?]}

Determines whether its argument is an instance of @linkclass[char].

@defprocform[contract?]{@proto[AnyC bool?]}

Determines whether its value is a contract. Contracts include many
constants (numbers, strings, Booleans), single-argument functions
(considered as predicates), and the results of contract combinators such
as @racket[OrC] and @racket[FunC].

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

@subsubsection{Numeric predicates}

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

Determines whether its argument is the special @linkclass{float}
not-a-number value. This is useful, since @code{nan} is not necessarily
@racket[==] to other instances of @code{nan}.

@subsection{Comparision operations}

@defprocform[cmp]{@proto[AnyC AnyC "OrC(int?, False)"]}

Compares two values of any type. If the values are incomparable, returns
@code{False}. Otherwise, returns an integer: less than 0 if the first
argument is less, 0 if equal, or 1 if the first argument is greater.

@defprocform[max]{@proto[AnyC AnyC ... AnyC]}

Returns the largest of the given arguments, using @racket[cmp] to determine
ordering. It is an error if the values are not comparable.

@defprocform[min]{@proto[AnyC AnyC ... AnyC]}

Returns the smallest of the given arguments, using @racket[cmp] to determine
ordering. It is an error if the values are not comparable.

@subsection{Randomness operations}

@defprocforms[random
  [@proto[float?]]
  [@proto["IntInC(1, 4294967087)" nat?]]
  [@proto[int? int? nat?]]
]

When called with zero arguments, returns a random floating point number
in the open interval (@racket[0.0], @racket[1.0]).

When called with one argument @racket[limit], returns a random exact
integer from the closed interval [@racket[0], @code{limit - 1}].

When called with two arguments @racket[min] and @racket[max], returns a
random exact integer from the closed interval [@racket[min], @code{max - 1}].
The difference between the arguments can be no greater than
@racket[4294967087].

@defconstform[RAND_MAX]{@code{nat?}}

Defined to be @racket[4294967087], the largest parameter (or span) that
can be passed to @racket[random].

@defprocform[random_bits]{@proto[nat? nat?]}

Returns a number consisting of the requested number of random bits.

@subsection{I/O Functions}

@defprocform[print]{@proto[str? AnyC ... VoidC]}

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
print("%p + %p = %p", a, b, a + b)
}|

@defprocform[println]{@proto[str? AnyC ... VoidC]}

Like @racket[print], but adds a newline at the end.

@subsection{Other functions}

@defprocform[error]{@proto[str? AnyC ... VoidC]}

Terminates the program with an error message. The error message must be
supplied as a format string followed by values to interpolate, in the
style of @racket[print].

@defprocform[dir]{@proto[AnyC "VecC(str?)"]}

Given an object, returns a vector of the names of its methods.
Given a struct, returns a vector of the names of its fields.

@section{Contracts}

The contract system helps guard parts of a program against each other by
enforcing properties of function and method parameters and results,
structure and class fields, and variables. A number of DSSL2 values may
be used as contracts, including:

@itemlist[
    @item{Booleans, which allow only themselves.}
    @item{Characters, which allow only themselves.}
    @item{Numbers, which allow only themselves.}
    @item{Strings, which allow only themselves.}
    @item{Functions of one argument, which are treated as flat
            contracts by applying as predicates.}
    @item{Contracts created using the contract combinators such as
            @racket[OrC] and @racket[FunC] described below.}
]

For example, to ensure that a function takes a string and returns a
number or @code{False}, we could use the contract
@code{FunC(str?, OrC(num?, False))}.

@subsection{Contract syntax}

@defcmpdform{@redefidform/inline[let] @term[var_name] : @nt[ctc] = @nt[expr]}

Binds variable @term[var_name] to the value of expression @nt[expr],
while applying the contract @nt[ctc]. Subsequent assignments
to the variable also must satisfy the contract. For example:

@dssl2block|{
let x : int? = 0
}|

Note that the @nt_[expr]{rhs} is optional, and the contract will not be
checked before the variable is assigned:

@dssl2block|{
let x : int?

x = 5
}|

@defcmpdform{@redefidform/inline[def] @term_[name]{f}(@term_[name]{1}: @nt_[ctc]{1}, ... @term_[name]{k}: @nt_[ctc]{k}) -> @nt_[ctc]{res}: @nt[block]}

Defines function @term_[name]{f} while specifying contract expressions
@nt_[ctc]{1} through @nt_[ctc]{k} for the parameters, and contract
expression @nt_[ctc]{res} for the result. For example:

@dssl2block|{
def pythag(x: num?, y: num?) -> num?:
    sqrt(x * x + y * y)
}|

Each of the contract positions is optional, and if omitted defaults to
@racket[AnyC].

@defcmpdforms[
    [@list{@redefidform/inline[struct] @term[name]:}]
    [@indent{@redefidform/inline[let] @term_[field_name]{1}: @nt_[ctc]{1}}]
    [@indent{...}]
    [@indent{@redefidform/inline[let] @term_[field_name]{k}: @nt_[ctc]{k}}]
]

Defines a structure @term[name] with the given contracts @nt_[ctc]{i}
applied to the fields @term_[field_name]{i}. This means that the contracts will
be applied both when constructing the structure and when mutating it.
For example:

@dssl2block|{
struct posn:
    let x: float?
    let y: float?
}|

Now constructing a @code{posn} will require both parameters to satisfy
the @racket[float?] predicate, as will assigning to either field.

It’s possible to include contracts on some fields without including them
on all, and the fields with omitted contracts default to @racket[AnyC].

@defcmpdforms[
    [@list{@redefidform/inline[class] @term[name] @~opt["[" @~many-comma[@term[ctc_param]] "]"] @~opt["(" @~many-comma[@term[interface_name]] ")"]:}]
    [@indent{@redefidform/inline[let] @term_[field_name]{1}: @nt_[ctc]{field_1}}]
    [@indent{...}]
    [@indent{@redefidform/inline[let] @term_[field_name]{k}: @nt_[ctc]{field_k}}]
    [@indent{@redefidform/inline[def] @term_[meth_name]{0}(@term_[self]{0} @~many["," @list{@term_[arg_name]{0}:} @nt_[ctc]{arg_0}]) -> @nt_[ctc]{res_0}: @nt_[block]{0}}]
    [@indent{...}]
    [@indent{@redefidform/inline[def] @term_[meth_name]{n}(@term_[self]{n} @~many["," @list{@term_[arg_name]{n}:} @nt_[ctc]{arg_n}]) -> @nt_[ctc]{res_n}: @nt_[block]{n}}]
]

Defines a class with contracts. See @racket[class] for the basics of
classes.

Contracts may be placed on:

@itemlist[
    @item{fields, which are checked when the field is assigned,}
    @item{non-self method parameters, which are checked when the method is invoked, and}
    @item{method results, which are checked when the method returns.}
]

Any contracts may be omitted, and default to @racket[AnyC]. No contract
may be placed on a method's self parameter, as that parameter is already
known to be an instance of the class.

If the class implements interfaces that have contracts, the interfaces'
contracts have no effect on the defined class.

A class may have some number of generic contract parameters,
@term[ctc_param]. These can be used to parameterize a class over other
contracts. When provided, they are in scope throughout the class, and
are added to the front of the external constructor's parameters.

For example, we can define a generic position class:

@dssl2block|{
class Posn[T]:
    let _x: T
    let _y: T

    def __init__(self, x: T, y: T):
        self._x = x
        self._y = y

    def x(self) -> T:
        self._x

    def y(self) -> T:
        self._y
}|

Now it is possible to make a position with @racket[int?] coordinates or
a position with @racket[float?] coordinates, by passing the coordinate
contract to the @code{Posn} constructor:

@dssl2block|{
let p = Posn(int?, 3, 4)
let q = Posn(float?, 3.0, 4.0)
}|

The @c{Posn} constructor and methods all check the given values
against the given contract.

When a class has generic contract parameters, it defines an additional
predicate factory, @c{@term{name}Of}. The predicate factory takes an
argument for each generic contract parameter, and returns a predicate
that checks for instances of the class that were creates with those same
contract parameters:

@dssl2block|{
assert Posn?(p)
assert Posn?(q)

assert PosnOf(int?)(p)
assert not PosnOf(int?)(q)

assert not PosnOf(float?)(p)
assert PosnOf(float?)(q)
}|

@defcmpdforms[
    [@list{@redefidform/inline[interface] @term[name] @~opt["[" @~many-comma[@term[ctc_param]] "]" ]:}]
    [@indent{@redefidform/inline[def] @term_[meth_name]{1}(@term_[self]{1} @~many["," @list{@term_[arg_name]{1}:} @nt_[ctc]{arg_1}]) -> @nt_[ctc]{res_1}}]
    [@indent{...}]
    [@indent{@redefidform/inline[def] @term_[meth_name]{k}(@term_[self]{n} @~many["," @list{@term_[arg_name]{k}:} @nt_[ctc]{arg_n}]) -> @nt_[ctc]{res_k}}]
]

Defines a interface with contracts. See @racket[interface] for the
basics of interfaces.

As with a class, contracts may be provided on method parameters (except
for self) and results. The contracts have no effect when a class
implements the interface, but do have an effect with the interface
is used to protect an instance of a class that implements it.

Defining an interface @term[name] binds three identifiers: @term[name],
@c{@term[name]?}, and @c{@term[name]!}. The first of these is used in
@racket[class] definitions to refer to the interface; the second is the
predicate, which recognizes instances of classes that implement the
interface. The third is the @deftech{interface contract}, which can be used
to protect instances of classes that implement the interface, to ensure
that their usage is only via the interface.

When an interface contract protects an object, first it checks the class
of the object. If the class is not declared to implement the interface,
an error is signaled. If the class does implement the interface, then a
@emph{protected object} is created. The protected object is like the
original object, except that only the methods of the interface are
usable; all other methods will error, blaming the caller, if called.
Furthermore, any contracts specified on methods in the interface are
applied to those methods of the protected object. (Reprotecting a
protected object with the same interface has no effect.)

Here is an example of an interface with one method and a class that
implements it:

@dssl2block|{
interface HAS_X:
    def get_x(self)

class Posn (HAS_X):
    let x
    let y

    def __init__(self, x, y):
        self.x = x
        self.y = y

    def get_x(self): self.x
    def get_y(self): self.y
}|

We might want to ensure that some client code uses an instance of
@c{Posn} only according to the @c{HAS_X} interface, without accessing
its other methods. We can use the @code{HAS_X!} interface contract to
protect a @c{Posn} instance in exactly this way. First, we create
an object and use it normally:

@dssl2block|{
let original = Posn(3, 4)

assert_eq original.get_x(), 3
assert_eq original.get_y(), 4

assert HAS_X?(original)
}|

Note that the interface predicate @c{HAS_X?} answers @code{True} for
instances of classes that implement @c{HAS_X}.

We can protect the original object by applying the @c{HAS_X!} interface
contract, as follows:

@dssl2block|{
let protected: HAS_X! = original
}|

Now, we can call @code{protected.get_x()} because interface @c{HAS_X}
defines the @c{get_x} method. But we cannot call @c{get_y} on
@c{protected}, because protecting it with @c{HAS_X!} makes all methods
other than @c{get_x} raise an error:

@dssl2block|{
assert_eq protected.get_x(), 3
assert_error protected.get_y()
}|

We can still access @c{get_y} on @code{original}:

@dssl2block|{
assert_eq original.get_x(), 3
assert_eq original.get_y(), 4
}|

As another example of how interface contracts can protect objects
against misuse, consider the following class @c{Counter}, which
implements an interface @c{STEPPABLE}:

@dssl2block|{
interface STEPPABLE:
    def step(self)

class Counter (STEPPABLE):
    let count
    def __init__(self, value: int?): self.count = value
    def get(self): self.count
    def step(self): self.count = self.count + 1
}|

The @c{STEPPABLE!} interface contract can be placed on a parameter to a
function to ensure that the function only uses the given object
according to the @c{STEPPABLE} interface. For example, we know that this
@c{advance} function can only use the @c{step} method to advance the
@c{Counter}:

@dssl2block|{
def advance(counter: STEPPABLE!, count: nat?):
    while count > 0:
        counter.step()
        count = count - 1
}|

A version of the function that attempts to do addition will fail because
the necessary methods aren't usable when the object is protected:

@dssl2block|{
def sneaky_advance(counter: STEPPABLE!, count: nat?):
    # Won't work because both .__init__ and .get are missing.
    counter.__init__(counter.get() + count)
}|

Like classes, interfaces can have generic contract parameters
@term[ctc_param]. When an interface has generic contract parameters,
these parameters are available to the contracts in the body of the
interface. The contract interface @c{@term[name]!} binds all the generic
contract parameters to @racket[AnyC]. The interface definition also
creates a generic interface contract factory @c{@term[name]_OF!}, which
takes one formal parameter for each generic contract parameter, and
returns an interface contract with the generic contract parameters
instantiated to the actual parameters of the generic interface contract
factory.

For example, here is a generic interface for a queue:

@dssl2block|{
interface QUEUE[T]:
    def empty(self) -> bool?
    def enqueue(self, value: T) -> VoidC
    def dequeue(self) -> OrC(False, T)
}|

This interface definition binds four identifiers:

@itemlist[
    @item{Interface name @c{QUEUE}, which can be used to declare
    that classes implement the interface.}

    @item{Predicate @c{QUEUE?}, which answers @code{True} for instances
    of classes that implement @c{QUEUE}, as well as @c{QUEUE} interface
    objects.}

    @item{Generic interface contract factory @c{QUEUE_OF!}, which takes
    one contract parameter. The resulting contract will protect an instance
    of any class that implements @c{QUEUE} by hiding all but the @c{empty?},
    @c{enqueue}, and @c{dequeue} methods, and applying the given contracts
    to each. For example, @code{QUEUE_OF!(int?)}, when protecting an object,
    ensures that its @c{enqueue} method is given an @c{int?}.}

    @item{Interface contract @c{QUEUE!}, equivalent to
    @code{QUEUE_OF!(AnyC)}.}
]

@subsection{Contract combinators}

@defconstform[AnyC]{@code{contract?}}

A flat contract that accepts any value.

@defconstform[VoidC]{@code{contract?}}

A flat contract that accepts the result of @racket[pass] and other
statements that return no value (such as assignment and loops).

@defprocform[VecC]{@proto[contract? contract?]}

Creates a contract that protects a vector to ensure that its elements
satisfy the given contract.

@defprocform[NotC]{@proto[contract? contract?]}

Creates a contract that inverts the sense of the given (flat) contract.

@defprocform[OrC]{@proto[contract? contract? ... contract?]}

Creates a contract that accepts a value if any of the arguments does.
For the details of how this works for higher-order contracts, see
@racket[racket:or/c].

@defprocform[AndC]{@proto[contract? contract? ... contract?]}

Creates a contract that accepts a value if all of the arguments do.
For the details of how this works for higher-order contracts, see
@racket[racket:and/c].

@defprocform[FunC]{@proto[arg:contract? ... res:contract? contract?]}

Creates a function contract with the given arguments and result. The
last argument is applied to the result, and all the other arguments are
contracts applied to the parameters.

@defprocform[NewForallC]{@proto[str? contract?]}

Creates a new, universal contract variable, useful in constructing
parametric contracts.

@defprocform[NewExistsC]{@proto[str? contract?]}

Creates a new, existential contract variable, useful in constructing
parametric contracts.

@defprocform[IntInC]{@proto["low:OrC(int?, False)"
                            "high:OrC(int?, False)"
                            contract?]}

Constructs a contract that accepts integers in the closed interval
[@c{low}, @c{high}]. If either end of the interval is @code{False},
that end of the interval is unchecked.

@defprocforms[apply_contract
    [@proto[#:all [X] contract? X X]]
    [@proto[#:all [X] contract? X pos:str? X]]
    [@proto[#:all [X] contract? X pos:str? neg:str? X]]
]

Applies a contract to a value, optionally specifying the parties.

You are unlikely to need to use this.
