#lang dssl2

import promise

# A StreamOf[A] is StreamCons(FunC(A), FunC(StreamOf[A]))
# where the functions are memoized.
#
# Don't use this directly; use `scons`.
struct StreamCons(first, rest)

# stream? : Any -> Boolean
# Shorter predicate for recognizing streams.
let stream? = StreamCons?

# scons : A FunC(StreamOf[A]) -> StreamOf[A]
# Creates a memoizing stream from the first element and a thunk producing
# the rest.
def scons(first, rest) -> stream?:
    let promise = delay(rest)
    StreamCons {
        first: λ: first,
        rest:  λ: force(promise)
    }

# smap : FunC(A, B) StreamOf[A] -> StreamOf[B]
# Maps a function over the elements of a stream.
def smap(f: FunC(AnyC, AnyC), stream: stream?):
    scons(f(stream.first()), λ: smap(f, stream.rest()))

# take_into_at : nat? nat? VectorOf[A] StreamOf[A] -> VoidC
# Takes `len` elements from the stream and places them into the vector
# starting at `start`.
def take_into_at(start: nat?, len: nat?, dst: vec?, stream: stream?):
    if len > 0:
        dst[start] = stream.first()
        take_into_at(start + 1, len - 1, dst, stream.rest())

# take : nat? StreamOf[A] -> VectorOf[A]
# Takes the first `n` elements of the given stream as a vector.
def take(n: nat?, stream: stream?):
    let result = [False; n]
    take_into_at(0, n, result, stream)
    result

# ones : StreamOf[nat?]
# A stream of 1s, forever.
let ones = scons(1, λ: ones)

# nats : StreamOf[nat?]
# The natural numbers starting with 1.
let nats = scons(0, λ: smap(λ x: x + 1, nats))

# unfold_stream : A FunC(A, A) -> StreamOf[A]
# Produces the stream by iterating `get_next` on the starting value `start`:
#   start, get_next(start), get_next(get_next(start)), …
def unfold_stream(start, get_next) -> stream?:
    scons(start, λ: unfold_stream(get_next(start), get_next))
