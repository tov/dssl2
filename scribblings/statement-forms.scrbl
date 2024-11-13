#lang scribble/manual

@require["common.rkt"]

@title[#:tag "stm-forms"]{Statement forms}

@section{Definition and assignment forms}

@defsmplidform[let]{@term[var_name] = @nt[expr]}

Declares and defines a local variable. Local variables may be declared in any
scope and last for that scope. A local variable may be re-assigned with the
assignment form (@racket[=]), as in the third line here:

@dssl2block|{
def sum(v):
    let result = 0
    for elem in v: result = result + elem
    return result
}|

@defsmplidform[let #:re]{@term[var_name]}

Declares a local variable, which will be undefined until it is assigned:

@dssl2block|{
let x

if y:
    x = f()
else:
    x = g()

println('%p', x)
}|

Accessing an undefined variable is an error.

@defsmplform{@nt[lvalue] @k[= #:def] @nt_[expr]{rhs}}

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
def insert(self, key, value):
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

@defcmpdidform[def]{@term[fun_name](@term_[var_name]{1}, @~……, @term_[var_name]{k}): @nt[block]}

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
# rbt_insert : X RbTreeOf[X] -> None
def rbt_insert(key, tree):
    # parent : RbLinkOf[X] -> RbLinkOf[X]
    def parent(link):
        link.parent if rbn?(link) else None

    # grandparent : RbLinkOf[X] -> RbLinkOf[X]
    def grandparent(link):
        parent(parent(link))

    # sibling : RbLinkOf[X] -> RbLinkOf[X]
    def sibling(link):
        let p = parent(link)
        if rbn?(p):
            if link is p.left: p.right
            else: p.left
        else: None

    # aunt : RbLinkOf[X] -> RbLinkOf[X]
    def aunt(link):
        sibling(parent(link))

    # . . .

    def set_root(new_node): tree.root = new_node
    search(tree.root, set_root)
}|

@section{Loop and control forms}

@defsmplidform[pass]

Does nothing; produces @racket[None].

@dssl2block|{
# account_credit : num? account? -> NoneC
# Adds the given amount to the given account’s balance.
def account_credit(amount, account):
    pass
#   ^ FILL IN YOUR CODE HERE
}|

@defcmpdidform*[if
    @list{@nt_[expr]{if}: @nt_[block]{if}}
    @~many[@k[elif #:def] @list{@nt_[expr]{elif}:} @nt_[block]{elif}]
    @~opt[@list{@k[else #:def]:} @nt_[block]{else}]
]

The DSSL2 conditional statement contains an @racket[if], 0 or more
@racket[elif]s, and optionally an @racket[else] for if none of the
conditions holds.

First it evaluates the @racket[if] condition @nt_[expr]{if}.
If non-false (any value but @racket[False] or @racket[None]),
it then evaluates block @nt_[block]{if}
and finishes. Otherwise, it evaluates each @racket[elif] condition
@nt_[expr]{elif} in turn; if each is false, it goes on to the
next, but when one is non-false then it finishes with the corresponding
@nt_[block]{elif}. Otherwise, if all of the conditions were false
and the optional @nt_[block]{else} is included, it evaluates
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

@defcmpdidform[for]{@term[var_name] @k[in] @nt[expr]: @nt[block]}

Loops over the values of the given @nt[expr], evaluating the
@nt[block] for each. The @nt[expr] can evaluate to a vector, a string,
or a @code{range}. If a vector, then this form iterates over the
element values (not the indices) of the vector; if a string, this form
iterates over its characters.

@dssl2block|{
for person in people_to_greet:
    println("Hello, %s!", person)
}|

To count, iterate over a @linkclass[range_iterator]{@tt{range_iterator}},
which is most easily constructed using the @racket[range] function.
For example:

@dssl2block|{
# Replaces every element of vector `v` with its square.
def sqr_vec(v):
    for i in range(v.len()):
        v[i] = v[i] * v[i]
}|

In this example hash function producer, the @racket[for] loops over the
characters in a string:

@dssl2block|{
# make_sbox_hash : -> [str? -> nat?]
# Returns a new n-bit string hash function.
def make_sbox_hash(n):
    let sbox = [ random_bits(n) for _ in range(256) ]
    def hash(input_string):
        let result = 0
        for c in input_string:
            let svalue = sbox[int(c) % 256]
            result = result ^ svalue
            result = (3 * result) % (2 ** n)
        return result
    hash
}|

@defcmpdidform[for #:re]{@term_[var_name]{1}, @term_[var_name]{2} @k[in] @nt[expr]: @nt[block]}

Loops over the indices and values of the given @nt[expr], evaluating
the @nt[block] for each. The @nt[expr] can evaluate to a vector, a
string, or a natural number. If a vector, then @term_[var_name]{1}
takes on the indices of the vector while @term_[var_name]{2} takes on
the values; if a string, then @term_[var_name]{1} takes on the
indices of the characters while @term_[var_name]{2} takes on the
characters; if a natural number then both variables count together.

@dssl2block|{
for ix, person in people_to_greet:
    println("%p: Hello, %s!", ix, person)
}|

@defcmpdidform[while]{@nt[expr]: @nt[block]}

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
    let result = None
    while cons?(bucket):
        if key == bucket.first.key:
            result = bucket.first.value
            break
        bucket = bucket.rest
    return result
}|

@defsmplidform[break]

When in a @racket[for] or @racket[while] loop, ends the (inner-most)
loop immediately.

@defsmplidform[continue]

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

@defsmplidform[return]{@nt[expr]}

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

@defsmplidform[return #:re]{}

Returns @racket[None] from the current function.

@section{Data and program structuring forms}

@defcmpdidform*[struct
    @list{@term[name]:}
    @indent{@k[let] @term_[field_name]{1}}
    @indent[~...]
    @indent{@k[let] @term_[field_name]{k}}
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
    (q.x * q.x + q.y * q.y).sqrt()
}|

It also possible to construct the struct by giving the fields in order
using function syntax:

@dssl2block|{
assert magnitude(posn(3, 4)) == 5
}|

Another example:

@dssl2block|{
# A RndBstOf[X] is one of:
# - None
# - Node(X, nat?, RndBstOf[X], RndBstOf[X])
struct Node:
    let key
    let size
    let left
    let right

# singleton : X -> RndBstOf[X]
def singleton(key):
    Node(key, 1, None, None)

# size : RndBstOf[X] -> nat?
def size(tree):
    tree.size if Node?(tree) else 0

# fix_size : Node? -> Void
def fix_size(node):
    node.size = 1 + size(node.left) + size(node.right)
}|

@defcmpdidform*[
    class @list{@term[name] @~opt["(" @~many-comma[@term[interface_name]] ")"]}
    @indent{@k[let] @term_[field_name]{1}}
    @indent[~...]
    @indent{@k[let] @term_[field_name]{k}}
    @indent{@k[def] @term_[meth_name]{0}(@term_[self]{0} @~many["," @term_[param_name]{0}]): @nt_[block]{0}}
    @indent[~...]
    @indent{@k[def] @term_[meth_name]{n}(@term_[self]{n} @~many["," @term_[param_name]{n}]): @nt_[block]{n}}
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
notation, writing the receiver, then the method name, and then the
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

Here is an example of a simple class with one field and six methods:

@dssl2block|{
class Counter:
    let value

    def __init__(self):
        self.value = 0

    def read(self):
        return self.value

    def reset(self):
        self.value = 0

    def add(self, n):
        self.value = self.value + n

    def sub(self, n):
        self.value = self.value - n
        if self._negative?(): error('counter cannot be negative!')

    # Private helper method
    def _negative?(self):
        return self.value < 0
}|

Note that the internal constructor method @code{__init__} takes only a self
parameter, which means that the external constructor function @code{Counter}
takes none. Thus, we can use the constructor to create a new counter
initialized to zero, and then we can update the counter via its @code{add},
@code{sub}, and @code{reset} methods:

@dssl2block|{
let counter = Counter()
counter.add(1)
counter.add(4)
assert counter.read() == 5
counter.reset()
assert_error counter.sub(1)
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

    def set_y(self, y): self.y = y
}|

We can create a @code{VerticalMovingPosn} as follows:

@dssl2block|{
let posn = VerticalMovingPosn(3, 4)
assert posn.get_x() == 3
assert posn.get_y() == 4
posn.set_y(10)
assert posn.get_x() == 3
assert posn.get_y() == 10
}|

Note that @code{VerticalMovingPosn} takes two parameters because
@code{__init__} takes three: the self parameter and two more.

@defcmpdidform*[
    interface @list{@term[name]:}
    @indent{@k[def] @term_[meth_name]{1}(@term_[self]{1} @~many["," @term_[param_name]{1}]}
    @indent[~...]
    @indent{@k[def] @term_[meth_name]{k}(@term_[self]{k} @~many["," @term_[param_name]{k}])}
]

Defines an interface named @term[name] with methods @term_[meth_name]{1}
through @term_[meth_name]{k}. Defining an interface binds three
identifiers:

@itemlist[
    @item{The interface name itself, @term[name], which can be mentioned
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
adding and removing elements, and checking whether the container is
empty or full:

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
    @item{@c{remove}, taking no non-self arguments.}
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
        self._contents = None
        self._empty?   = True

    def empty?(self):
        self._empty?

    def full?(self):
        not self.empty?()

    def add(self, value):
        if self.full?(): error("Cell.add: full")
        self._contents = value
        self._empty? = False

    def remove(self):
        if self.empty?(): error("Cell.remove: empty")
        self._empty? = True
        self._contents
}|

Second, the @c{VecStack} class implements a fixed-size stack
using a vector:

@dssl2block|{
class VecStack (CONTAINER):
    let _data
    let _len

    def __init__(self, capacity):
        self._data = [None; capacity]
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
        self._data[self._len]
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

@defsmplidform*[
    import @list{@term[mod_name]}
    @list[import]{ @term[mod_string]}
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

@section{Testing and timing forms}

@defsmplidform[assert]{@nt[expr]}

Asserts that the given @nt[expr] evaluates to a truthy value. If the
expression evaluates @racket[False] or @racket[None], signals an error.

@dssl2block|{
test 'ScHash.member? finds "hello"':
    let h = ScHash(10, make_sbox_hash())
    assert not h.member?('hello')
    h.insert('hello', 5)
    assert h.member?('hello')

test 'first_char_hasher works':
    assert first_char_hasher('')      == 0
    assert first_char_hasher('A')     == 65
    assert first_char_hasher('Apple') == 65
    assert first_char_hasher('apple') == 97
}|

@defsmplidform[assert #:re]{@nt_[expr]{test}, @k[time] @k[<] @nt_[expr]{sec}}

Asserts that @nt_[expr]{test} evaluates to a truthy value in less than
@nt_[expr]{sec} seconds.

@defsmplidform*[assert_error
    @nt_[expr]{fail}
    @list{@assert_error @nt_[expr]{fail}, @nt_[expr]{str}}
    @list{@assert_error @nt_[expr]{fail}, @k[time] @k[<] @nt_[expr]{sec}}
    @list{@assert_error @nt_[expr]{fail}, @nt_[expr]{str}, @k[time] @k[<] @nt_[expr]{sec}}
]

Asserts that evaluating @nt_[expr]{fail} results in an error. If
@nt_[expr]{fail} evaluates without producing an error, the assertion
fails. Allows specifying an expected error message or timeout duration.

In the 2nd and 4th forms, expression @nt_[expr]{str} must evaluate to a
string, which must be a substring of the resulting error message for the
assertion to succeed. (The 1st and 3rd forms do not specify an error
message, so any error will cause the assertions to succeed.)

The 3rd and 4rd forms allow giving a timeout, in seconds, for evaluating
@nt_[expr]{fail}. Thus, expression @nt_[expr]{sec} must evaluate to a
positive number.

@defcmpdidform[test]{@nt[expr]: @nt[block]}

Runs the code in @nt[block] as a test case named @nt[expr]
(which is optional). If an
assertion fails or an error occurs in @nt[block], the test case
terminates, failure is reported, and the program continues after the
block.

For example:

@dssl2block|{
test "arithmetic":
    assert 1 + 1 == 2
    assert 2 + 2 == 4
}|

A @racket[test] @nt[block] can be used to perform just one check or a
long sequence of preparation and checks:

@dssl2block|{
test 'single-chaining hash table':
    let h = ScHash(10)
    assert not h.member?('hello')

    h.insert('hello', 5)
    assert h.member?('hello')
    assert h.lookup('hello') == 5
    assert not h.member?('goodbye')
    assert not h.member?('helo')

    h.insert('helo', 4)
    assert h.lookup('hello') == 5
    assert h.lookup('helo') == 4
    assert not h.member?('hel')

    h.insert('hello', 10)
    assert h.lookup('hello') == 10
    assert h.lookup('helo') == 4
    assert not h.member?('hel')
    assert h.keys(h) == cons('hello', cons('helo', nil()))
}|

@defcmpdidform[test #:re]{
  @nt_[expr]{name}, @k[time] @k[<] @nt_[expr]{sec}: @nt[block]}

Runs the code in @nt[block] as a @emph{timed} test case named
@nt_[expr]{name} (which is optional). The test fails if it takes longer
than @nt_[expr]{sec} seconds to run, or if any of the assertions in
@nt[block] fails.

@defcmpdidform[time]{@nt[expr]: @nt[block]}

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
10,000,000 zeroes: cpu: 309ms real: 792ms gc: 238ms
}|

This means it tooks 309 milliseconds of CPU time over 792 milliseconds of
wall clock time, with 238 ms of CPU time spent on garbage collection.

