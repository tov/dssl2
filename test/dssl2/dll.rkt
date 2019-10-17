#lang dssl2

import dll

let lst = Dll[int?]()

lst.push_back(3)
lst.push_back(4)

assert lst.front() == 3
assert lst.back() == 4

assert lst.len() == 2

assert lst.pop_front() == 3
assert lst.pop_front() == 4

assert lst.len() == 0

assert lst.pop_front() is None

assert lst.len() == 0
