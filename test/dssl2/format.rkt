#lang dssl2

assert "a%db".format(5) == "a5b"
assert "a%pb".format(5) == "a5b"
assert "a%sb".format(5) == "a5b"

assert "a%db".format("hello") == "a'hello'b"
assert "a%pb".format("hello") == "a'hello'b"
assert "a%sb".format("hello") == "ahellob"

assert "%p + %p = %p".format(3, 4, 7) == "3 + 4 = 7"
assert "%%".format() == "%"
assert "~a".format() == "~a"

assert "%p".format(inf) == 'inf'
assert "%p".format(nan) == 'nan'

