#lang dssl2

test "error()":
    assert_error error(), 'error()'
    
test "error('foo bar')":
    assert_error error('foo bar'), 'foo bar'

test "error('(%p, %s)', 'a', 'b')":
    assert_error error('(%p, %s)', 'a', 'b'), "('a', b)"
    
test "error(5)":
    assert_error error(5), 'error(5)'
    
test "error(5, 6, '7')":
    assert_error error(5, 6, '7'), "error(5, 6, '7')"

