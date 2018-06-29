#lang dssl2

let v = [0, 1, 2, 3]
let w = v.filter(even?)

assert w == [0, 2]
