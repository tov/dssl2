#lang dssl2

assert [ i for i in 5 ] == [0, 1, 2, 3, 4]
assert [ i for i in 5 if even?(i) ] == [0, 2, 4]
assert [ 2 * i for i in [0, 2, 9] ] == [0, 4, 18]
assert [ i + 10 * j for i, j in [0, 2, 9] ] == [0, 21, 92]
assert [ i + 10 * j for i, j in [0, 2, 9] if even?(i) ] == [0, 92]
