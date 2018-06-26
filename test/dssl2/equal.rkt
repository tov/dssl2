#lang dssl2

assert [2, 3, 4] == [2, 3, 4]
assert [3, 3, 4] != [2, 3, 4]

assert 2 == 2
assert 2 != 3
assert 2 == 2.0
assert [2] == [2.0]

struct posn(x, y)

assert posn(3, 4) == posn(3, 4)
assert posn(3, 4) != posn(3, 5)

let cycle1 = [0, 1, 2]
cycle1[2] = cycle1

let cycle2 = [0, 1, 2]
cycle2[2] = cycle2

assert cycle1 == cycle2
