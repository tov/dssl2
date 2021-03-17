#lang dssl2

def vec_of_iter(iter):
    [ x for x in iter ]
 
assert vec_of_iter(range(5)) == [0, 1, 2, 3, 4]
assert vec_of_iter(range(0)) == []

assert vec_of_iter(range(3, 8)) == [3, 4, 5, 6, 7]
assert vec_of_iter(range(8, 3)) == []
assert vec_of_iter(range(8, 8)) == []

assert vec_of_iter(range(0, 10, 2)) == [0, 2, 4, 6, 8]
assert vec_of_iter(range(0, 9, 2))  == [0, 2, 4, 6, 8]
assert vec_of_iter(range(5, 0, -1)) == [5, 4, 3, 2, 1]

assert vec_of_iter(range(0.25, 3, 0.5)) == [0.25, 0.75, 1.25, 1.75, 2.25, 2.75]
