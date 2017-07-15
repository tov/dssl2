#lang dssl2

assert num?(5)
assert num?(5.5)
assert num?(-5)
assert num?(inf)
assert num?(nan)

assert !num?('hello')
