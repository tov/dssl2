#lang dssl2

assert (True && True) == True
assert (True && False) == False
assert (False && True) == False
assert (False && False) == False

assert (True && 5) == 5

def add_return(n, v):
    x = x + n
    return v

let x = 0
assert (add_return(1, False) && add_return(10, True)) == False
assert x == 1

x = 0
assert (add_return(1, True) && add_return(10, 12)) == 12
assert x == 11
