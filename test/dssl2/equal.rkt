#lang dssl2

assert [2, 3, 4] == [2, 3, 4]
assert [3, 3, 4] != [2, 3, 4]

assert 2 == 2
assert 2 != 3
assert 2 == 2.0
assert [2] == [2.0]

let cycle1 = [0, 1, 2]
cycle1[2] = cycle1

let cycle2 = [0, 1, 2]
cycle2[2] = cycle2

println("~e", cycle1)
# assert cycle1 == cycle2
