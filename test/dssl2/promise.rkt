#lang dssl2

import promise

let prom = delay(λ: 5)
assert prom.force() == 5

let counter = 0
def up!():
    counter = counter + 1
    counter

assert counter == 0
prom = delay(up!)
assert counter == 0
assert not prom.forced?()
assert prom.force() == 1
assert prom.forced?()
assert counter == 1
assert prom.force() == 1
assert counter == 1
