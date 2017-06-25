#lang dssl2

let v = [2, 3, 4]

assert v[0] == 2
assert v[1] == 3

v[1] = 10

assert v[0] == 2
assert v[1] == 10
