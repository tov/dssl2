#lang dssl2

assert_eq char(65).__int__(), 65
assert_eq 'hello'.explode().implode(), 'hello'
assert_eq [char(65)].implode(), 'A'
assert_eq [65].implode(), 'A'
assert_error [True].implode(), 'char'