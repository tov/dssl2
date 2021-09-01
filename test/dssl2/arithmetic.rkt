#lang dssl2

assert 10 - 3 == 7
assert 10 * 3 == 30
assert 10 / 5 == 2
assert 10 / 20 == 0.5

assert (4).sqrt() == 2

def pythag(x: num?, y: num?) -> num?:
    (x * x + y * y).sqrt()
assert pythag(3,4) == 5

assert (8).log()/(2).log() == 3
assert (10000).log(10) == 4
