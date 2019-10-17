#lang dssl2

import fifo

let q = Fifo[int?]()

assert q.empty?()
assert q.len() == 0
assert q.peek() is None

q.enqueue(5)

assert not q.empty?()
assert q.len() == 1
assert q.peek() == 5

assert_error q.enqueue('hello')
assert_error q.enqueue(False)
assert_error q.enqueue(None)

q.enqueue(7)
q.enqueue(8)

assert not q.empty?()
assert q.len() == 3
assert q.peek() == 5

assert q.dequeue() == 5

assert not q.empty?()
assert q.len() == 2
assert q.peek() == 7

assert q.dequeue() == 7

assert not q.empty?()
assert q.len() == 1
assert q.peek() == 8

assert q.dequeue() == 8

assert q.empty?()
assert q.len() == 0
assert q.peek() == None

assert q.dequeue() == None
