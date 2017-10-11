#lang dssl2

# Crashes because not protected by a test. Should grade as 0/0.

assert_eq 3, 4

test 'hello': pass
