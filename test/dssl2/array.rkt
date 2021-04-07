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

assert_error array()[0], 'Array index out of bounds: 0 >= 0'

let x = Array(5)
assert x.capacity() == 5
for i in range(8):
    x.push(i)

x[0] = 100

assert x[0] == 100
assert x[1] == 1

let y = x.clone()
assert (x == y)

x.push_front(99)
assert x[0] == 99
assert x[1] == 100
assert x[2] == 1
assert x.pop_front() == 99
assert x == y
assert x.pop_back() == 7
x.push(7)
assert x == y
x.shrink_to_fit()
assert x.capacity() == 8

let c = Array[int?](5)
assert_error c.push("string")
let d = c.clone()
assert_error d.push("string")