#lang dssl2

let x

if False:
    x = 0
elif False:
    x = 1
elif False:
    x = 2
elif True:
    x = 3
else:
    x = 4

assert x == 3
