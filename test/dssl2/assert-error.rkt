#lang dssl2

assert_error error('hello')

assert_error error('hello'), 'el'

def no_error():
    assert_error 5
assert_error no_error(), 'did not error'

def wrong_error():
    assert_error error('hello'), 'Elf'
assert_error wrong_error(), 'got a different error'
