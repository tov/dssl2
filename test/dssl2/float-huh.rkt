#lang dssl2

assert float?(6.5)
assert !float?(6)
assert !float?('hello')
