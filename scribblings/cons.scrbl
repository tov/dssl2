#lang scribble/manual

@require["common.rkt"]

@title{The @tt{cons} library}

This library provides a representation for header-free singly-linked lists, as
well as a number of utility functions that operate on them.

These definitions are not part of the base DSSL2 language, and must be imported
explicitly using: @racket[import cons]

@section{The @tt{cons} struct}

The core definition of the @tt{cons} library is the @tt{cons} struct, which
represents a node in a singly-linked list:

@dssl2block|{
struct cons:
    let data
    let next: Cons.list?
}|

The end of the linked list must be marked with @racket[None]. A value of
@racket[None] on its own is therefore an empty linked list.

For example, the code:

@dssl2block|{
cons(1, cons(2, cons (3, None)))
}|

Produces a linked list with elements 1, 2, and 3, in that order.

@defprocform[cons]{@proto[AnyC Cons.list? Cons.list?]}

Constructs a @tt{cons} struct.

@O(1) time and space.

@defprocform[cons?]{@proto[AnyC bool?]}

A predicate that checks whether the given value is a @racket[cons] struct.

@O(1) time and space.

@section{The @tt{Cons.X} family of functions}

This library provides a number of utility functions for working with lists made
of @tt{cons} structs with names starting with @tt{Cons.}.

@defprocform[Cons.list?]{@proto[AnyC bool?]}

A predicate that checks whether the given value is a linked list made of
@racket[cons] structs, with @racket[None] at the end.

@O(1) time and space.

@defprocform[Cons.ListC]{[C: @racket[contract?]]: @racket[contract?]}

Constructs a contract for a list, given a contract @code{C} for the
elements. This contract copies the list while applying constract
@code{C} to each element.

@O(@n × @T{C}) time and @O(@n + @S{C}) space.

@defprocform[Cons.len]{@proto[Cons.list? nat?]}

Finds the length of a list.

@O(@n) time and @O(1) space.

@defprocform[Cons.app]{@proto[Cons.list? Cons.list? Cons.list?]}

Appends two lists producing a new list, and not modifying either of the input
lists. The resulting list will share structure with the second list.

@O(@n) time and space, where @n is the length of the first list.

@defprocform[Cons.rev]{@proto[Cons.list? Cons.list?]}

Reverses a list producing a new list, and not modifying the input list.

@O(@n) time and space.

@defprocform[Cons.rev_app]{@proto[Cons.list? Cons.list? Cons.list?]}

Reverses the first list, appending it onto the second.

@O(@n) time and space, where @n is the length of the first list.

@defprocform[Cons.concat]{@proto[Cons.list? Cons.list? Cons.list?]}

Destructively concatenates two lists, returning the concatenated list, and
modifying the first list.

@O(@n) time and @O(1) space, where @n is the length of the first list.

@defprocform[Cons.to_vec]{@proto[Cons.list? vec?]}

Converts a list to a vector.

@O(@n) time and space.

@defprocform[Cons.into_vec]{@proto[Cons.list? vec? nat? NoneC]}

Copies a list into a vector starting at the index given by the third
argument. Assumes there is enough space in the vector.

@O(@n) time and @O(1) space.

@defprocform[Cons.from_vec]{@proto[vec? Cons.list?]}

Creates a list from the elements of a vector.

@O(@n) time and space.

@defprocform[Cons.foreach]{(f: @racket[FunC][@racket[AnyC], @racket[AnyC]], Cons.list?) -> @racket[NoneC]}

Calls a visitor function on each element of the given list, in order.

@O(@n × @T{f}) time and @O(@S{f}) space.

@defprocform[Cons.foldr]{[Y](f: @racket[FunC][@racket[AnyC], Y, Y], Y, Cons.list?) -> Y}

Traverses a list from right to left, accumulating a result using the given
function.

@O(@n × @T{f}) time and @O(@n + @S{f}) space.

@defprocform[Cons.foldl]{[Y](f: @racket[FunC][Y, @racket[AnyC], Y], Y, Cons.list?) -> Y}

Traverses a list from left to right, accumulating a result using the
given function.

@O(@n × @T{f}) time and @O(@S{f}) space.

@defprocform[Cons.map]{(f: @racket[FunC][@racket[AnyC], @racket[AnyC]], Cons.list?) -> Cons.list?}

Maps over a list by applying a function to each element. @O(@n) time and @O(@n)
space (to allocate the new list).

@O(@n × @T{f}) time and @O(@n + @S{f}) space.

@defprocform[Cons.filter]{(pred?: @racket[FunC][@racket[AnyC], @racket[AnyC]], Cons.list?) -> Cons.list?}

Filters a list by applying a predicate to each element.

@O(@n × @T{pred?}) time and @O(@S{pred?}) space.

@defprocform[Cons.andmap]{(f: @racket[FunC][@racket[AnyC], @racket[AnyC]], Cons.list?) -> @racket[AnyC]}

Applies the given function to each element in turn, returning @racket[False] if
the result is @racket[False] for any element, and otherwise returning the
result of the function applied to the last element. Returns @racket[True] if
the list is empty.

@O(@n × @T{f}) time and @O(@S{f}) space.

@defprocform[Cons.ormap]{(f: @racket[FunC][@racket[AnyC], @racket[AnyC]], Cons.list?) -> @racket[AnyC]}

Applies the given function to each element in turn, returning the first
non-@racket[False] result, or @racket[False] if none is non-@racket[False].

@O(@n × @T{f}) time and @O(@S{f}) space.

@defprocform[Cons.sort]{[T](lt?: @racket[FunC][T, T, @racket[AnyC]], Cons.listC[T]) -> Cons.list?}

Sorts a list producing a new list, without modifying the input list.
Uses the given function as a “less than” comparison to determine the order.

This function uses insertion sort as its sorting algorithm.

@O(@emph{n}² × @T{lt?}) time and @O(@n + @S{lt?}) space.
