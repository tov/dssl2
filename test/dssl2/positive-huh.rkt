#lang dssl2

assert (5).positive?()
assert (5.5).positive?()
assert not (0).positive?()
assert not (-4).positive?()
