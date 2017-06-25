#lang dssl2

assert number?(5)
assert number?(5.5)
assert number?(-5)
assert number?(inf)
assert number?(nan)

assert !number?('hello')
