#lang dssl2

let sum = 0
let counter = 5

while counter > 0:
    sum = sum + counter
    counter = counter - 1

assert sum == 15
