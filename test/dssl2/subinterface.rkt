#lang dssl2

let r = range_iterator(0, 8)

def each(x):
    println('%p', x)
 
class Always:
    let value
    def __init__(self, value): self.value = value
    def __index_ref__(self, ix): value

let s = index_iterator("abc", 0, 3)
s.try_advance(each)
s.try_advance(each)
s.try_advance(each)
s.try_advance(each)