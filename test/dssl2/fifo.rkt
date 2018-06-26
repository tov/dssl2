#lang dssl2

import fifo

let q = Fifo(AnyC)

assert q.empty?()
assert_eq q.size(), 0
assert_eq q.peek(), False

q.enqueue(5)

assert !q.empty?()
assert_eq q.size(), 1
assert_eq q.peek(), 5

q.enqueue('hello')
q.enqueue('world')

assert !q.empty?()
assert_eq q.size(), 3
assert_eq q.peek(), 5

assert_eq q.dequeue(), 5

assert !q.empty?()
assert_eq q.size(), 2
assert_eq q.peek(), 'hello'

assert_eq q.dequeue(), 'hello'

assert !q.empty?()
assert_eq q.size(), 1
assert_eq q.peek(), 'world'

assert_eq q.dequeue(), 'world'

assert q.empty?()
assert_eq q.size(), 0
assert_eq q.peek(), False

assert_eq q.dequeue(), False

# yay untyped
q.enqueue(False)

assert !q.empty?()
assert_eq q.size(), 1
assert_eq q.peek(), False

assert_eq q.dequeue(), False

assert q.empty?()
assert_eq q.size(), 0
assert_eq q.peek(), False
