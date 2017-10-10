#lang dssl2

test 'no pattern':
    assert_error error('hello')

test 'matches':
    assert_error error('hello'), 'el'

test 'no error':
    def inner():
        assert_error 5
    assert_error inner(), 'did not error'

test 'does not match':
    def inner():
        assert_error error('hello'), 'Elf'
    assert_error inner(), 'didn’t match the pattern'
