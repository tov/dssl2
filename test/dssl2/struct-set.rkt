#lang dssl2

defstruct posn(x, y)

let p = posn { x: 3, y: 4 }

assert p.x == 3
assert p.y == 4

p.y = 10

assert p.x == 3
assert p.y == 10

