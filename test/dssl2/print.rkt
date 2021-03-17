#lang dssl2

assert str(3) == '3'
assert str(3.0) == '3.0'
assert '%d %d'.format(True, False) == 'True False'
assert str('hello') == "hello"
assert '%d'.format('hello') == "'hello'"
assert '%d'.format('"hello"') == '\'"hello"\''
assert '%d'.format("'hello'") == '"\'hello\'"'

struct foo:
    let bar
    let baz

assert str(foo(3, 4)) == 'foo {bar: 3, baz: 4}'

assert str([3, 4]) == '[3, 4]'
assert str([foo(3, 4)]) == '[foo {bar: 3, baz: 4}]'
