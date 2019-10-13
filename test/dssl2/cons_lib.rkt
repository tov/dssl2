#lang dssl2

import cons

let to_list = Cons.from_vec

def add2(x): x + 2
assert_eq Cons.map(add2, to_list([2, 3, 4])), to_list([4, 5, 6])

let lst = to_list([2, 3, 4, 5, 6])
assert_eq Cons.filter(even?, lst), to_list([2, 4, 6])
assert_eq Cons.filter(odd?, lst), to_list([3, 5])
