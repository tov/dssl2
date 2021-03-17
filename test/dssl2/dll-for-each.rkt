#lang dssl2

import dll

let x = Dll[int?]()

x.push_back(2)
x.push_back(3)
x.push_back(4)

assert [2, 3, 4] == x.to_vec()
