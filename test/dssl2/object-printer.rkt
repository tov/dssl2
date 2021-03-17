#lang dssl2

class Posn:
    let _x
    let _y
    
    def __init__(self, x, y):
        self._x = x
        self._y = y
        
    def x(self): self._x
    def y(self): self._y
    def x!(self, x): self._x = x
    
    def __print__(self, print):
        print("(%p, %p)", self._x, self._y)

let a = Posn(3, 4)
assert str(a) == "(3, 4)"

a.x!(a)
assert str(a) == "#0=(#0#, 4)"
