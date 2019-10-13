#lang scribble/manual

@require["common.rkt"]

@title{Contracts}

The contract system helps guard parts of a program against each other by
enforcing properties of function and method parameters and results,
structure and class fields, and variables. In particular, contracts
specify properties of values that are checked at various points in the
program.

A contract may be @emph{flat}, in which case it is checked immediately
against a value. Or a contract may by @emph{higher-order}, in which case
it may have to wrap a value to perform further checking in the future.

A number of DSSL2 values may be used as contracts, including:

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
@code{FunC[str?, OrC(num?, False)]}.

@section{Contract syntax}

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

@defcmpdform{@redefidform/inline[def] @term_[name]{f}(@term_[name]{1}: @nt_[ctc]{1}, ..., @term_[name]{k}: @nt_[ctc]{k}) -> @nt_[ctc]{res}: @nt[block]}

Defines function @term_[name]{f} while specifying contract expressions
@nt_[ctc]{1} through @nt_[ctc]{k} for the parameters, and contract
expression @nt_[ctc]{res} for the result. For example:

@dssl2block|{
def pythag(x: num?, y: num?) -> num?:
    sqrt(x * x + y * y)
}|

Each of the contract positions is optional, and if omitted defaults to
@racket[AnyC].

Optionally, after @term_[name]{f} and before the left parenthesis,
@nt[opt_ctc_params] may appear: one or more comma-separated names,
enclosed in square brackets. These are optional contract parameters to
the function, which can appear in the contracts on the rest of the
parameters, the result, and anywhere in the body of the function.

For example, a vector mapping function might be defined with contract
parameters and contracts on its parameters as follows:

@dssl2block|{
def vec_map[T, U](f: FunC[T, U], v: VecC[T]) -> VecC[U]:
    [ f(e) for e in v ]
}|

When calling @code{vec_map}, it's possible to supply actual contracts
for @code{T} and @code{U} in square brackets; or omitting the square
brackets, the contracts both default to @racket[AnyC].

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
contracts. When provided, they are in scope throughout the class. The
external constructor receives the actual contracts for an instance of
the class as optional, square bracket parameters, giving before the
ordinary parameters that are passed to the internal constructor.

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
contract in square brackets to the @code{Posn} constructor:

@dssl2block|{
let p = Posn[int?](3, 4)
let q = Posn[float?](3.0, 4.0)
}|

Specifying contract parameters is optional, and if omitted, they default
to @racket[AnyC]. So we can write

@dssl2block|{
let r = Posn(3, 4.0)
}|

to get a @code{Posn[AnyC]}.

When a contract parameter is given, the @c{Posn} constructor and methods
all check the given values against the given contract.

When a class has generic contract parameters, its predicate can also be
instantiated with contracts, using square brackets:

@dssl2block|{
assert Posn?[int?](p)
assert not Posn?[int?](q)
assert not Posn?[int?](r)

assert not Posn?[float?](p)
assert Posn?[float?](q)
assert not Posn?[float?](r)
}|

If the predicate is not instantiated, it recognizes all objects of that
class regardless of how their contract parameters are instantiated:

@dssl2block|{
assert Posn?(p)
assert Posn?(q)
assert Posn?(r)
}|

Note that a predicate not being instantated is different from
instantiating it with @racket[AnyC]:

@dssl2block|{
assert not Posn?[AnyC](p)
assert not Posn?[AnyC](q)
assert Posn?[AnyC](r)
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

assert original.get_x() == 3
assert original.get_y() == 4

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
assert protected.get_x() == 3
assert_error protected.get_y(), \
    'interface HAS_X is protecting method get_y'
}|

We can still access @c{get_y} on @code{original}:

@dssl2block|{
assert original.get_x() == 3
assert original.get_y() == 4
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
interface. The interface contract @c{@term[name]!} takes its contract
parameters as optional square bracket parameters that default to
@racket[AnyC].

For example, here is a generic interface for a queue:

@dssl2block|{
interface QUEUE[T]:
    def empty(self) -> bool?
    def enqueue(self, value: T) -> NoneC
    def dequeue(self) -> OrC(False, T)
}|

This interface definition binds three identifiers:

@itemlist[
    @item{Interface name @c{QUEUE}, which can be used to declare
    that classes implement the interface.}

    @item{Predicate @c{QUEUE?}, which answers @code{True} for instances
    of classes that implement @c{QUEUE}, as well as @c{QUEUE} interface
    objects.}

    @item{Generic interface contract @c{QUEUE!}, which optionally takes
    one square-bracket contract parameter. The resulting contract will
    protect an instance of any class that implements @c{QUEUE} by
    disabling all but the @c{empty?}, @c{enqueue}, and @c{dequeue}
    methods, and applying the given contracts to each. For example,
    @code{QUEUE![int?]}, when protecting an object, ensures that its
    @c{enqueue} method is given an @c{int?}. Or @code{QUEUE!} can be
    used to protect an object, without specifying an element contract.}
]

@defexpform{@nt_[expr]{0}[@nt_[expr]{1}, ..., @nt_[expr]{k}]}

Indexes or instantiates the result of @nt_[expr]{0}.

If @nt_[expr]{0} evalutes to a vector or string, then @code{k} must
equal @code{1}, and this is an @seclink["indexing"]{indexing
expression}.

If @nt_[expr]{0} is a generic function, class, or contract, then
@nt_[expr]{1}, ..., @nt_[expr]{k} are the parameters used to instantiate
the generic function, class, or contract. For example, function contracts
are created using @racket[FunC] and square brackets:

@dssl2block|{
FunC[int?, char?, str?]
}|

Or here is an example of a generic pair class, which can be instantiated
to particular contracts for its components using square brackets:

@dssl2block|{
class Pair[T, U]:
    let _fst
    let _snd

    def __init__(self, fst: T, snd: U):
        self._fst = fst
        self._snd = snd

    def fst(self) -> T: self._fst
    def snd(self) -> U: self._snd

let p = Pair[int?, str?](5, 'six')
}|

@section{Contract combinators}

@defconstform[AnyC]{@code{contract?}}

A flat contract that accepts any value.

@defconstform[NoneC]{@code{contract?}}

A flat contract that accepts the value @racket[None], which is the
result of @racket[pass] and other statements that return no value (such
as assignment and loops). This is used as the result contract for
functions and methods that do not return a value.

@defprocform[VecC]{[@racket[contract?]]: @racket[contract?]}

Creates a contract that protects a vector to ensure that its elements
satisfy the given contract.

For example:

@dssl2block|{
let v: VecC[int?] = [2, 3, 4]
}|

Note that vector contracts are checked lazily, when elements are indexed
or assigned, rather than eagerly when first protected. So for example:

@dssl2block|{
let v: VecC[int?] = [2, 3, 'four'] # okay, not checked yet
assert v[1] == 3                   # passes check
assert_error v[2]                  # fails check
}|

Assigning a non-integer to an element of @code{v} is an error as well.

If the optional square bracket parameter is omitted, @racket[VecC] just
checks that the protected value is a vector.

@defprocform[FunC]{[@racket[contract?], ..., @racket[contract?]]: @racket[contract?]}

Creates a function contract with the given arguments and result. The
last argument is a contract applied to the result, and all the other
arguments are contracts applied to the parameters.

If the optional square bracket parameters are omitted, the resulting
contract checks for a procedure, but nothing further.

@defprocform[NotC]{@proto[contract? contract?]}

Creates a contract that inverts the sense of the given flat contract.

@defprocform[OrC]{@proto[contract? contract? ... contract?]}

Creates a contract that accepts a value if any of the arguments does.
For the details of how this works for higher-order contracts, see
@racket[racket:or/c].

@defprocform[AndC]{@proto[contract? contract? ... contract?]}

Creates a contract that accepts a value if all of the arguments do.
For the details of how this works for higher-order contracts, see
@racket[racket:and/c].

@defprocform[IntInC]{@proto["low:OrC(int?, False)"
                            "high:OrC(int?, False)"
                            contract?]}

Constructs a contract that accepts integers in the closed interval
[@c{low}, @c{high}]. If either end of the interval is @code{False},
that end of the interval is unchecked.

@defprocforms[apply_contract
    [@proto[contract? AnyC AnyC]]
    [@proto[contract? AnyC pos:str? AnyC]]
    [@proto[contract? AnyC pos:str? neg:str? AnyC]]
]

Applies a contract to a value, optionally specifying the parties.

You are unlikely to need to use this.
