#lang dssl2

assert_error error(), 'error()'
assert_error error('foo bar'), 'foo bar'
assert_error error('(%p, %s)', 'a', 'b'), "('a', b)"
assert_error error(5), 'error(5)'
assert_error error(5, 6, '7'), "error(5, 6, '7')"
