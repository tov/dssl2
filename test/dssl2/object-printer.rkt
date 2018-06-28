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
    
    def __print__(self, display, visit):
        display("(")
        visit(self._x)
        display(", ")
        visit(self._y)
        display(")")

let a = Posn(3, 4)
assert_eq format("~e", a), "(3, 4)"

a.x!(a)
assert_eq format("~e", a), "#0=(#0#, 4)"

struct Foo: pass
