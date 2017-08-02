#lang dssl2

defstruct posn(x: float?, y: float?)

let p = posn(3.4, 4.0)
let q = posn { y: 4.0, x: 3.4 }

assert_eq p, q

p.x = 5.0
