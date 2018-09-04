#lang dssl2

interface ITERABLE[T]:
    def iterator(self)

interface ITERATOR[T] (ITERABLE[T]):
    def try_advance(self, visit: FunC[T, AnyC]) -> bool?
            
class range_iterator (ITERATOR):
    let low
    let high
    
    def __init__(self, low, high):
        self.low = low
        self.high = high
    
    def try_advance(self, visit):
        if self.low < self.high:
            visit(self.low)
            self.low = self.low + 1
            True
        else:
            False
    
    def iterator(self):
        self

let foo: ITERATOR! = range_iterator(0, 5)

foo.iterator()