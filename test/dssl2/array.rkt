#lang dssl2

import array

let a = array()

assert a.empty?()
assert a.len() == 0

a.push(2)
assert a.len() == 1
a.push(3)
assert a.len() == 2
a.push(4)
assert a.len() == 3

assert a.get(0) == 2
assert a.get(1) == 3
assert a.get(2) == 4

a.set(1, 10)

assert a.get(0) == 2
assert a.get(1) == 10
assert a.get(2) == 4
