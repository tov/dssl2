#lang dssl2

struct AStruct:
    let f1
    let f2

let s = AStruct(5, 'six')

assert s.f1 == 5
assert s.f2 == 'six'
