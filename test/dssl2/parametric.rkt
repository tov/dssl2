#lang dssl2

# Check that parametric contracts don't mess up non-parametric function names:
def foo_bar(): 5
assert str(foo_bar) == '#<proc:foo_bar>'

def id[A](x: A) -> A: x

assert id[int?](5) == 5
assert id(5) == 5
assert_error id[str?](5)

def choose[A, B](which: bool?, x: A, y: B) -> OrC(A, B):
    x if which else y

assert choose(True, 4, 'hello') == 4
assert choose(False, 4, 'hello') == 'hello'

def tricky[Y](good?: bool?, x: Y) -> Y:
    x if good? else 4

assert tricky[str?](True, 'five') == 'five'
assert_error tricky[str?](False, 'five'), 'promised: str?'

