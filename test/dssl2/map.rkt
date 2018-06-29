#lang dssl2

let v = [0, 1, 2, 3]
assert v.map(identity) == v

let w = v.map(lambda x: x + 2)
assert w == [2, 3, 4, 5]
