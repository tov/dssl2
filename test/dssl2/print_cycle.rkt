#lang dssl2

let v = [0, 1, 2]
v[2] = v

assert_eq str(v), '#0=[0, 1, #0#]'
