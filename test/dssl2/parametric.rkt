#lang dssl2

# Check that parametric contracts don't mess up non-parametric function names:
def foo_bar(): 5
assert_eq format('~e', foo_bar), '#<proc:foo_bar>'

def id[A](x: A) -> A: x

assert_eq id(5), 5

def choose[A, B](which: bool?, x: A, y: B) -> OrC(A, B):
    x if which else y

assert_eq choose(True, 4, 'hello'), 4
assert_eq choose(False, 4, 'hello'), 'hello'

# assert_eq format('~e', choose), 'choose'

def tricky[Y](good?: bool?, x: Y) -> Y:
    x if good? else 4

assert_eq tricky(True, 5), 5
assert_error tricky(False, 5), 'promised: Y'

tricky = λ x, y: x

assert_error tricky(True, 5), 'promised: Y'
