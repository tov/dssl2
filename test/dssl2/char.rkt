#lang dssl2

assert char(65).__int__() == 65
assert 'hello'.explode().implode() == 'hello'
assert [char(65)].implode() == 'A'
assert [65].implode() == 'A'
assert_error [True].implode(), 'char'
