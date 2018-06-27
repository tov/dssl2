#lang dssl2

assert str?('ehlo')
assert str?("ehlo")
assert not str?(str?)
