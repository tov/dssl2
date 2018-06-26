#lang dssl2

import fifo

let q = Fifo(int?)

assert q.empty?()
assert_eq q.size(), 0
assert_eq q.peek(), False

q.enqueue(5)

assert !q.empty?()
assert_eq q.size(), 1
assert_eq q.peek(), 5

assert_error q.enqueue('hello')
    
assert_error q.enqueue(False)

q.enqueue(7)
q.enqueue(8)

assert !q.empty?()
assert_eq q.size(), 3
assert_eq q.peek(), 5

assert_eq q.dequeue(), 5

assert !q.empty?()
assert_eq q.size(), 2
assert_eq q.peek(), 7

assert_eq q.dequeue(), 7

assert !q.empty?()
assert_eq q.size(), 1
assert_eq q.peek(), 8

assert_eq q.dequeue(), 8

assert q.empty?()
assert_eq q.size(), 0
assert_eq q.peek(), False

assert_eq q.dequeue(), False
