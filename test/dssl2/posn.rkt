#lang dssl2

struct posn:
    let x: float?
    let y: float?

let p = posn(3.4, 4.0)
let q = posn { y: 4.0, x: 3.4 }

assert p == q

p.x = 5.0

