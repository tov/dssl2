#lang dssl2

assert int?(6)
assert not int?(6.5)
assert not int?('hello')
