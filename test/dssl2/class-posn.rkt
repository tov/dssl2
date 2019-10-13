#lang dssl2

# A Posn that can move vertically but is fixed horizontally.

class Posn:
    let x_: num?
    let y_: num?
    def __init__(foo, x, y):
        foo.x_ = x
        foo.y_ = y
    def x(self): self.x_
    def _x!(it, nx): it.x_ = nx   # private!
    def y!(self, ny): self.y_ = ny
    def y(bees): bees.y_
    def get_self(this): this

let p

p = Posn(3, 4)
assert Posn?(p)
assert_eq 3, p.x()
assert_eq 4, p.y()
p.y!(10)
assert_eq 3, p.x()
assert_eq 10, p.y()

p = Posn(3, 4)
assert_error p.x_
def assign_5():
    p.x = 5
assert_error assign_5()
assert_error p._x!

p = Posn(3, 4)
let get_y = p.y
let set_y = p.y!
assert_eq 4, get_y()
set_y(5)
assert_eq 5, get_y()
