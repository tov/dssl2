#lang dssl2

assert 5 & 6 == 4
assert 5 | 6 == 7
assert 5 ^ 6 == 3
assert ~5    == -6

assert 1 << 3 == 8
assert 8 >> 3 == 1
