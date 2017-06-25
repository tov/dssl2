#lang dssl2

let x = 9
let f = lambda y: x + y

assert f(3) == 12
