#lang dssl2

import dll

let IntDll = MakeDll(int?)

let lst = IntDll.empty()

lst.push_back(3)
lst.push_back(4)

assert_eq lst.front(), 3
assert_eq lst.back(), 4

assert_eq lst.size(), 2

assert_eq lst.pop_front(), 3
assert_eq lst.pop_front(), 4

assert_eq lst.size(), 0

assert_eq lst.pop_front(), False

assert_eq lst.size(), 0