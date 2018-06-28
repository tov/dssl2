#lang dssl2

interface CONTAINER:
    def empty?(self)
    def full?(self)
    def add(self, value)
    def remove(self)
    
class VectorStack (CONTAINER):
    let _size
    let _data
    
    def __init__(self, capacity):
        self._size = 0
        self._data = [False; capacity]

    def capacity(self): len(self._data)

    def size(self): self._size

    def empty?(self): self.size() == 0
    
    def full?(self): self.size() == self.capacity()
    
    def push(self, value):
        if self.full?(): error('VectorStack.push: full')
        self._data[self._size] = value
        self._size = self._size + 1
        
    def pop(self):
        if self.empty?(): error('VectorStack.pop: empty')
        self._size = self._size - 1
        let result = self._data[self._size]
        self._data[self._size] = False
        result
    
    def add(self, value): self.push(value)
    def remove(self): self.pop()

class RingBuffer (CONTAINER):
    let _start
    let _size
    let _data
    
    def __init__(self, capacity):
        self._start = 0
        self._size  = 0
        self._data  = [False; capacity]
    
    def capacity(self): len(self._data)
    
    def size(self): self._size
    
    def empty?(self): self.size() == 0
    
    def full?(self): self.size() == self.capacity()
    
    def enqueue(self, value):
        if self.full?(): error('RingBuffer.enqueue: full')
        let index = self._offset_start(self._size)
        self._data[index] = value
        self._size = self._size + 1
        
    def dequeue(self):
        if self.empty?(): error('RingBuffer.dequeue: empty')
        self._size = self._size - 1
        let result = self._data[self._start]
        self._data[self._start] = False
        self._start = self._offset_start(1)
        result

    def _offset_start(self, distance):
        (self._start + distance) % self.capacity()
        
    def add(self, value): self.enqueue(value)
    def remove(self): self.dequeue()

def vector_stack_test():
    let o = VectorStack(4)
    assert o.empty?()
    assert not o.full?()
    assert_eq o.size(), 0
    assert_eq o.capacity(), 4
    assert_error o.remove(), 'empty'
    o.push(2)
    assert not o.empty?()
    assert not o.full?()
    assert_eq o.size(), 1
    o.push(3)
    assert_eq o.size(), 2
    o.push(4)
    o.push(5)
    assert not o.empty?()
    assert o.full?()
    assert_error o.push(6), 'full'
    assert_eq o.pop(), 5
    assert not o.full?()
    o.push(7)
    assert_eq o.pop(), 7
    assert_eq o.pop(), 4
    assert_eq o.pop(), 3
    assert_eq o.pop(), 2
    assert_error o.pop(), 'empty'

def ring_buffer_test():
    let o = RingBuffer(4)
    assert o.empty?()
    assert not o.full?()
    assert_eq o.size(), 0
    assert_eq o.capacity(), 4
    assert_error o.dequeue(), 'empty'
    o.enqueue(2)
    assert not o.empty?()
    assert not o.full?()
    assert_eq o.size(), 1
    o.enqueue(3)
    assert_eq o.size(), 2
    o.enqueue(4)
    o.enqueue(5)
    assert not o.empty?()
    assert o.full?()
    assert_error o.enqueue(6), 'full'
    assert_eq o.dequeue(), 2
    assert not o.full?()
    o.enqueue(7)
    assert_eq o.dequeue(), 3
    assert_eq o.dequeue(), 4
    assert_eq o.dequeue(), 5
    assert_eq o.dequeue(), 7
    assert_error o.dequeue(), 'empty'

def container_test(make: FunC(nat?, CONTAINER)):
    let o = make(4)
    assert o.empty?()
    assert not o.full?()
    assert_error o.remove(), 'empty'
    assert_error o.capacity(), 'does not have method'
    o.add(2)
    assert not o.empty?()
    assert not o.full?()
    assert_eq o.remove(), 2
    assert o.empty?()
    o.add(2)
    o.add(3)
    o.add(4)
    o.add(5)
    assert not o.empty?()
    assert o.full?()
    assert_error o.add(6), 'full'
    assert o.remove()
    assert not o.full?()
    o.add(7)
    assert o.remove()
    assert o.remove()
    assert o.remove()
    assert o.remove()
    assert_error o.remove(), 'empty'

vector_stack_test()
ring_buffer_test()
container_test(VectorStack)
container_test(RingBuffer)