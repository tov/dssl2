#lang dssl2

assert 2 + 4 == 6
assert 2 + "four" == "2four"
assert "two" + 4 == "two4"
assert "two" + "four" == "twofour"

assert "" + True == 'True' # bad

assert inf + 'hi' == 'infhi'
