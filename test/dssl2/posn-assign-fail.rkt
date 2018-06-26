#lang dssl2

struct posn:
    let x: float?
    let y: float?

let p = posn(3.4, 4.0)
p.x = 'hi'
