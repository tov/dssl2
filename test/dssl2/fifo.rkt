#lang dssl2

import fifo

let q = Fifo()

assert q.empty?()
assert q.len() == 0
assert q.peek() is None

q.enqueue(5)

assert not q.empty?()
assert q.len() == 1
assert q.peek() == 5

q.enqueue('hello')
q.enqueue('world')

assert not q.empty?()
assert q.len() == 3
assert q.peek() == 5

assert q.dequeue() == 5

assert not q.empty?()
assert q.len() == 2
assert q.peek() == 'hello'

assert q.dequeue() == 'hello'

assert not q.empty?()
assert q.len() == 1
assert q.peek() == 'world'

assert q.dequeue() == 'world'

assert q.empty?()
assert q.len() == 0
assert q.peek() is None

assert q.dequeue() is None

# yay untyped
q.enqueue(None)

assert not q.empty?()
assert q.len() == 1
assert q.peek() is None

assert q.dequeue() is None

assert q.empty?()
assert q.len() == 0
assert q.peek() is None
