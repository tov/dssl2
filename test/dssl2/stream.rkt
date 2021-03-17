#lang dssl2

import stream

assert ones.take(5) == [1; 5]
assert nats.take(5) == [0, 1, 2, 3, 4]
