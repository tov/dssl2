#lang dssl2

let a = [2, 3, 4]
let b = [2, 3, 4]

assert a is a
assert b is b
assert a is not b
assert b is not a
