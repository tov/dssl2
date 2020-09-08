#lang dssl2

def f(x):
    return x
    let y

assert f(10) == 10

def g():
    def h():
        pass

assert g() is None
