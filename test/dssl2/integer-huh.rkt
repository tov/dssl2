#lang dssl2

assert integer?(6)
assert !integer?(6.5)
assert !integer?('hello')
