#lang dssl2

assert 1 + 1 == 2, time < 1
assert 1 + 1 == 2, time < 2
assert True, time < 3
assert True, time < inf
