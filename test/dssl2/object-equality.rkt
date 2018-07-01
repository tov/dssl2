#lang dssl2

class Posn1:
    let _x
    let _y
    
    def __init__(self, x, y):
        self._x = x
        self._y = y
        
    def x(self): self._x
    def y(self): self._y
    def x!(self, x): self._x = x

assert Posn1(3, 4) == Posn1(3, 4)
assert Posn1(3, 5) != Posn1(3, 4)
assert Posn1(2, 4) != Posn1(3, 4)

# Cycles:
let p1 = Posn1(3, 4)
let q1 = Posn1(3, 4)
p1.x!(p1)
q1.x!(q1)
assert p1 == q1

class Posn2:
    let _x
    let _y
    
    def __init__(self, x, y):
        self._x = x
        self._y = y
    
    def x(self): self._x
    def y(self): self._y
    def x!(self, x): self._x = x
    
    def __eq__(self, other):
        self._x == other.x()
        
assert Posn2(3, 4) == Posn2(3, 4)
assert Posn2(3, 5) == Posn2(3, 4)
assert Posn2(2, 4) != Posn2(3, 4)

# Cycles:
let p2 = Posn2(3, 4)
let q2 = Posn2(3, 4)
p2.x!(p2)
q2.x!(q2)
assert p2 == q2

