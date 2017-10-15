#lang dssl2

assert format("a~eb", 5) == "a5b"
assert format("a~ab", "hello") == "ahellob"
assert format("a~eb", "hello") == "a'hello'b"

assert format("~e + ~e = ~e", 3, 4, 7) == "3 + 4 = 7"

assert format("~e", inf) == 'inf'
assert format("~e", nan) == 'nan'
