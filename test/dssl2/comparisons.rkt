#lang dssl2

assert 2 < 3
assert !(2 < 2)
assert 2 <= 3
assert 2 <= 2
assert 3 > 2
assert 3 >= 2

assert "a" < "b"
assert "a" <= "b"
assert !("a" > "b")
assert !("a" >= "b")
