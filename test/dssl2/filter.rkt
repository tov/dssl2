#lang dssl2

let v = [0, 1, 2, 3]
let w = filter(even?, v)

assert w == [0, 2]
