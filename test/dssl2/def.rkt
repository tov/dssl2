#lang dssl2

def nullary(): 5

assert nullary() == 5

def unary(x):
    return x + 1

assert unary(5) == 6

def binary(x, y):
    x + y + 1

assert binary(5, 6) == 12
