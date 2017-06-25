#lang dssl2

assert string?('ehlo')
assert string?("ehlo")
assert !string?(string?)
