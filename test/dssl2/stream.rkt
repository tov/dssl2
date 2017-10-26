#lang dssl2

import stream

assert_eq take(5, ones), [1; 5]
assert_eq take(5, nats), [0, 1, 2, 3, 4]