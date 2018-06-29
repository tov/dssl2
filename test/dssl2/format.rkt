#lang dssl2

assert "a~eb".format(5) == "a5b"
assert "a~ab".format("hello") == "ahellob"
assert "a~eb".format("hello") == "a'hello'b"

assert "~e + ~e = ~e".format(3, 4, 7) == "3 + 4 = 7"

assert "~e".format(inf) == 'inf'
assert "~e".format(nan) == 'nan'

