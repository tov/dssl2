#lang dssl2

assert 2 < 3
assert not 2 < 2
assert 2 <= 3
assert 2 <= 2
assert 3 > 2
assert 3 >= 2

assert "a" < "b"
assert "a" <= "b"
assert not "a" > "b"
assert not "a" >= "b"

# equality on differing types is allowed
assert not 3 == "3"

# but not inequalities
assert_error 2 < "3"
assert_error 3 < "3"
assert_error 4 < "3"
assert_error 3 > "3"
assert_error 1 < None
assert_error None < 1
assert_error 1 > None
assert_error None > 1
assert_error 1 <= None
assert_error None <= 1
assert_error 1 >= None
assert_error None >= 1

class NumWrapper:
    let n
    def __init__ (self, n):
        self.n = n
    def get_n (self): return self.n
    def __cmp__ (self, other):
       return (self.n).__cmp__(other.get_n())

assert NumWrapper(1) == NumWrapper(1)
assert not NumWrapper(2) == NumWrapper(1)
assert not NumWrapper(1) == NumWrapper(2)
assert NumWrapper(1) < NumWrapper(2)
assert NumWrapper(1) <= NumWrapper(2)
assert not NumWrapper(2) < NumWrapper(2)
assert not NumWrapper(2) <= NumWrapper(1)
assert not NumWrapper(1) > NumWrapper(2)
assert not NumWrapper(1) >= NumWrapper(2)
assert NumWrapper(2) > NumWrapper(1)
assert NumWrapper(2) >= NumWrapper(2)
