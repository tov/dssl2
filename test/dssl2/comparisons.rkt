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

# New behavior in Object DSSL:
assert not 2 < "3"
assert not 3 < "3"
assert not 4 < "3"
assert not 3 == "3"
assert not 3 > "3"