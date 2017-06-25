#lang dssl2

let result = 0

for i in [1, 2, 4, 8, 16]:
    if i > 5: break
    result = result + i

assert result == 7
