#lang dssl2

let v = [0, 1, 2, 3]
assert map(identity, v) == v

let w = map(lambda x: x + 2, v)
assert w == [2, 3, 4, 5]
