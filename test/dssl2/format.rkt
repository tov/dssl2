#lang dssl2

assert format("a~ab", 5) == "a5b"
assert format("a~ab", "hello") == "ahellob"
assert format("a~sb", "hello") == 'a"hello"b'

assert format("~a + ~a = ~a", 3, 4, 7) == "3 + 4 = 7"
