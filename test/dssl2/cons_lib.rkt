#lang dssl2

import cons

let list = Cons.from_vec

test 'Cons.map':
    def add2(x): x + 2
    assert_eq Cons.map(add2, list([2, 3, 4])), list([4, 5, 6])

test 'Cons.filter':
    let lst = list([2, 3, 4, 5, 6])
    assert_eq Cons.filter(even?, lst), list([2, 4, 6])
    assert_eq Cons.filter(odd?, lst), list([3, 5])
