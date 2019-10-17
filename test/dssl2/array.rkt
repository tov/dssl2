#lang dssl2

import array

let a = array()

assert a.empty?()
assert_eq a.len(), 0

a.push(2)
assert_eq a.len(), 1
a.push(3)
assert_eq a.len(), 2
a.push(4)
assert_eq a.len(), 3

assert_eq a.get(0), 2
assert_eq a.get(1), 3
assert_eq a.get(2), 4

a.set(1, 10)

assert_eq a.get(0), 2
assert_eq a.get(1), 10
assert_eq a.get(2), 4
