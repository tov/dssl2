#lang dssl2

import dll

let IntDll = MakeDll(int?)

let x = IntDll.empty()

x.push_back(2)
x.push_back(3)
x.push_back(4)

def to_vec(dll: IntDll.Dll?) -> vec?:
    let result = [False; dll.size()]
    def each(i, x): result[i] = x
    dll.each_with_index(each)
    result
    
assert_eq to_vec(x), [2, 3, 4]