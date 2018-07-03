#lang dssl2

import stream

assert_eq ones.take(5), [1; 5]
assert_eq nats.take(5), [0, 1, 2, 3, 4]
