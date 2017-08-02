#lang dssl2

defstruct posn(x: float?, y: float?)

let p = posn(3.4, 4.0)
p.x = 'hi'
