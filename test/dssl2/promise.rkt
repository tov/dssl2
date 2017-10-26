#lang dssl2

import promise

let prom = delay(λ: 5)
assert_eq force(prom), 5

let counter = 0
def up!():
    counter = counter + 1
    counter

assert_eq counter, 0
prom = delay(up!)
assert_eq counter, 0
assert_eq force(prom), 1
assert_eq counter, 1
assert_eq force(prom), 1
assert_eq counter, 1
