#lang dssl2

let a = [2, 3, 4]
let b = [2, 3, 4]

assert a === a
assert b === b
assert a !== b
assert b !== a
