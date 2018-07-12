#lang dssl2

import promise

# Stream[T] is an abstract data type.
class Stream[T]:
    let _first
    let _rest
    
    # Stream: (T: contract?) T (-> Stream[T]) -> Stream[T]
    def __init__(self, first: T, rest: FunC[Stream?]):
        self._first = first
        self._rest  = Promise[Stream?](rest)

    # first: -> T
    def first(self): self._first
    
    # rest: -> Stream[T]
    def rest(self): self._rest.force()
    
    # take_into_at: nat? nat? VectorOf[T] -> VoidC
    def take_into_at(self, start: nat?, len: nat?, dst: vec?) -> VoidC:
        let src = self
        while len > 0:
            dst[start] = src.first()
            src        = src.rest()
            start      = start + 1
            len        = len - 1
    
    # take: nat? -> VectorOf[T]
    def take(self, len: nat?) -> vec?:
        let result = [False; len]
        self.take_into_at(0, len, result)
        result
    
    # skip: nat? -> Stream[T]
    def skip(self, count: nat?) -> Stream?:
        let result = self
        while count > 0:
            result = result.rest()
            count  = count - 1
        result
    
    # map[U]: (T -> U) -> Stream[U]
    def map[U](self, f: FunC[T, U]) -> Stream?[U]:
        def loop(stream):
            Stream[U](f(stream.first()), λ: loop(stream.rest()))
        loop(self)
    
# scons : AnyC (-> Stream?) -> Stream?
# Creates a memoizing stream from the first element and a thunk producing
# the rest.
def scons(first, rest: FunC[Stream?]):
    Stream(first, rest)

# ones : Stream[1]
# A stream of 1s, forever.
let ones = Stream[1](1, λ: ones)

# nats : Stream[nat?]
# The natural numbers starting with 1.
let nats = Stream[nat?](0, λ: nats.map[nat?](λ x: x + 1))

# unfold_stream_of : (T: contract?) T (T -> T) -> Stream[T]
# Produces the stream by iterating `get_next` on the starting value `start`:
#   start, get_next(start), get_next(get_next(start)), ...
def unfold_stream[T](start: T, get_next: FunC[T, T]) -> Stream?:
    def loop(start): Stream[T](start, λ: loop(get_next(start)))
    loop(start)
