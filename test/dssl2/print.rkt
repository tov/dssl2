#lang dssl2

assert_eq format('~e', 3), '3'
assert_eq format('~e', 3.0), '3.0'
assert_eq format('~e ~e', True, False), 'True False'
assert_eq format('~e', 'hello'), "'hello'"
assert_eq format('~e', '"hello"'), '\'"hello"\''
assert_eq format('~e', "'hello'"), '"\'hello\'"'

struct foo:
    let bar
    let baz

assert_eq format('~e', foo(3, 4)), 'foo {bar: 3, baz: 4}'

assert_eq format('~e', [3, 4]), '[3, 4]'
assert_eq format('~e', [foo(3, 4)]), '[foo {bar: 3, baz: 4}]'
