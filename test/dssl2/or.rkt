#lang dssl2

assert (True or True) == True
assert (True or False) == True
assert (False or True) == True
assert (False or False) == False

assert (True or 5) == True
assert (5 or True) == 5
assert (5 or False) == 5
assert (False or 5) == 5

def add_return(n, v):
    x = x + n
    return v

let x = 0
assert (add_return(1, True) or add_return(10, True)) == True
assert x == 1

x = 0
assert (add_return(1, False) or add_return(10, 12)) == 12
assert x == 11
