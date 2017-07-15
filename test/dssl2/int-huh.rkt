#lang dssl2

assert int?(6)
assert !int?(6.5)
assert !int?('hello')
