#lang dssl2

let v = [2, 3, 4]

assert v[0] == 2
assert v[1] == 3

# support `get` method for uniform interface between direct addressing dics
# and other dicts
assert v.get(0) == 2

v[1] = 10

assert v[0] == 2
assert v[1] == 10

# ditto for put
v.put(1, 12)
assert v.get(0) == 2
assert v.get(1) == 12

def vf (v: VecC[num?]) -> num?:
   return v [0]

assert vf([0]) == 0
assert vf([1, 2, 3]) == 1
assert_error vf(2)

def tf (v: TupC[num?, str?, num?]) -> num?:
   return v[0] + v[2]

assert tf([1, "a", 0]) == 1
assert_error tf([1, 2, 0])
assert_error tf([1, 2])
