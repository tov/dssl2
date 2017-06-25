#lang dssl2

assert (True || True) == True
assert (True || False) == True
assert (False || True) == True
assert (False || False) == False

assert (True || 5) == True
assert (5 || True) == 5
assert (5 || False) == 5
assert (False || 5) == 5

def add_return(n, v):
    x = x + n
    return v

let x = 0
assert (add_return(1, True) || add_return(10, True)) == True
assert x == 1

x = 0
assert (add_return(1, False) || add_return(10, 12)) == 12
assert x == 11
