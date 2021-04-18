#lang scribble/manual

@require["common.rkt"]

@title{The @tt{Array} library}

This library provides a representation for a dynamic array, as well as a number of
utility functions that operate on them. The array library supports printing
(see note below), equality comparisons, square bracket index
referencing, and square bracket index setting, so feel free to get and set elements
in your array by using a[i].

Note regarding printing: if x is an object of class @tt{Array} that you wish to print, you
must do one of the following:

@dssl2block|{
println(x)
print("%p", x)
}|

Simply doing
@dssl2block|{
print(x)}|
will not work and will instead throw an error.

These definitions are not part of the base DSSL2 language, and must be imported
explicitly using: @racket[import Array]

@section{Class @tt{Array}}

The core of this library deals with the @tt{Array} class, which represents a dynamic array.
Henceforth, the term “array” will be used to refer to a dynamic array (the data structure),
while vector will be used to refer to the standard DSSL2 vector that has a fixed size.
@defprocform[Array]{[T: @racket[contract?]] (capacity: @racket[nat?]) -> Array?}

The @tt{Array} constructor takes 1 argument, which must be a natural number, and returns an empty
array with an initial maximum capacity equal to the argument. Optionally, it can take a
contract to restrict the types of the elements within the array. Examples are as follows:

@dssl2block|{
let x = Array(10)
let y = Array[int?](10)
}|

@defmethform[Array empty?]{@proto[bool?]}

Determines whether the array is empty.

@defmethform[Array len]{@proto[nat?]}

Returns the length of the array.

@defmethform[Array capacity]{@proto[nat?]}

Returns the current capacity of the array.

@defmethform[Array ensure_capacity]{@proto[n:nat? NoneC]}

Takes 1 argument, n, which must be a natural number. Ensures that the array has capacity
at least equal to n. Specifically, the new array has a capacity equal to the larger of
the following: n, and double of the old capacity. Does nothing if n is smaller or equal
to current capacity. 

@defmethform[Array get]{@proto[index:nat? value:T]}
Takes 1 argument (an index) which must be a natural number, and returns the value at that index of the
array. Throws an error if the index is out of bounds.

@defmethform[Array set]{@proto[index:nat? value:T NoneC]}
Takes 2 arguments (an index and a value) and sets the element at the index to the given value. Throws an
error if the index is out of bounds.

@defmethform[Array push_back]{@proto[value:T NoneC]}
Takes 1 argument (a value) and adds it to the back of the array. Allocates more capacity if necessary.
Equivalent to @racket[push].

@defmethform[Array pop_back]{@proto["OrC(T, NoneC)"]}
Returns the last element in the array, or None if the array is empty. Allocates more capacity if necessary.
Equivalent to @racket[pop].

@defmethform[Array push_front]{@proto[value:T NoneC]}
Takes 1 argument (a value) and adds it to the front of the array. Allocates more capacity if necessary.

@defmethform[Array pop_front]{@proto["OrC(T, NoneC)"]}
Returns the first element in the array, or None if the array is empty. Allocates more capacity if necessary.

@defmethform[Array push]{@proto[value:T NoneC]}
Shorthand for @racket[push_back].

@defmethform[Array pop]{@proto["OrC(T, NoneC)"]}
Shorthand for @racket[pop_back].

@defmethform[Array clear]{@proto[NoneC]}
Clears the contents of the array.

@defmethform[Array shrink_to_fit]{@proto[NoneC]}
If there is excess capacity in the array, removes the excess capacity so that the new capacity
is exactly equal to the current length of the array. Does nothing otherwise.

@defmethform[Array clone]{@proto[Array?]}
Returns a (near) copy of the array. The new array will have capacity equal to its length, rather
than the capacity of the old array. In other words, the new array is guaranteed not to have any excess capacity.

@defmethform[Array to_vec]{@proto[vec?]}
Converts the array to a vector. The vector has size exactly equal to the number of elements in it.

@defmethform[Array equals]{@proto[other:Array? bool?]}
Checks if 2 arrays are equal. Arrays are equal if they have the same length and every element in the first array
matches the corresponding element (index-wise) in the second array. Note: the capacities do not need to be equal. Equivalent
to using the @racket[==] comparison.

@defmethform[Array iterator]{@proto[iterator?]}
Returns an iterator over the number of elements in the array.


@section{Other procedures}

@defprocform[array @proto[Array?] ]

This procedure takes no arguments and returns an empty array of capacity 8.

@defprocform[array_of_vec @proto[v:vec? Array?] ]
Converts a vector to an array. The original vector remains unchanged.

@defprocform[build_array @proto[n:nat? init:FunC Array?] ]
Builds an array of length n. Each element of the array is initialized to init(i) where i is the index of the
element and init is the function provided.