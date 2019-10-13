#lang dssl2

struct AStruct:
    let f1
    let f2

let _a = AStruct(5, 6)
let _b = AStruct(5, '75')

assert_error AStruct(9)
assert_error AStruct(9, 10, 11)

struct AnotherStruct:
    pass

AnotherStruct()
assert_error AnotherStruct(True)

