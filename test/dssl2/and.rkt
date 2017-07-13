#lang dssl2

assert (True and True) == True
assert (True and False) == False
assert (False and True) == False
assert (False and False) == False

assert (True and 5) == 5

def add_return(n, v):
    x = x + n
    return v

let x = 0
assert (add_return(1, False) and add_return(10, True)) == False
assert x == 1

x = 0
assert (add_return(1, True) and add_return(10, 12)) == 12
assert x == 11
