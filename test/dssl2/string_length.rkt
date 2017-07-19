#lang dssl2

assert_eq string_length(''), 0
assert_eq string_length('0'), 1
assert_eq string_length('01234'), 5
