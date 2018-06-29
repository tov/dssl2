#lang dssl2

assert_eq char(65).to_int(), 65
assert_eq explode('hello').implode(), 'hello'
assert_eq [char(65)].implode(), 'A'
assert_eq [65].implode(), 'A'
assert_error [True].implode(), 'char'