#lang dssl2

interface QUEUE[T]:
    def enqueue(self, element: T) -> NoneC
    def dequeue(self) -> T
    def empty?(self) -> bool?

class RingBuffer[T] (QUEUE):
    let data: VecC[OrC(T, NoneC)]
    let start: nat?
    let length: nat?

    def __init__(self, capacity):
        self.data = [None; capacity]
        self.start = 0
        self.length = 0

    def capacity(self):
        return self.data.len()

    def len(self):
        return self.length

    def empty?(self):
        return self.len() == 0

    def full?(self):
        return self.len() == self.capacity()

    def enqueue(self, element: T):
        if self.full?(): error('RingBuffer.enqueue: full')
        self.data[(self.start + self.length) % self.capacity()] = element
        self.length = self.length + 1

    def dequeue(self) -> T:
        if self.empty?(): error('RingBuffer.dequeue: empty')
        let result = self.data[self.start]
        self.data[self.start] = None
        self.length = self.length - 1
        self.start = (self.start + 1) % self.capacity()
        return result

def int_ring_buffer(capacity):
    return RingBuffer[int?](capacity)

# test 'RingBuffer creation':
#     let q = RingBuffer(8)
#     assert q.capacity() == 8
#     assert q.len() == 0
#     assert q.empty?()
#     assert not q.full?()

# test 'RingBuffer empty dequeue':
#     let q = RingBuffer(8)
#     assert_error q.dequeue()

# test 'RingBuffer enqueue and dequeue':
#     let q = RingBuffer(8)
#     q.enqueue(2)
#     assert q.len() == 1
#     q.enqueue(3)
#     assert q.len() == 2
#     assert q.dequeue() == 2
#     assert q.len() == 1
#     assert q.dequeue() == 3
#     assert q.empty?()

# test 'RingBuffer full enqueue':
#     let q = RingBuffer(8)
#     for i in range(8): q.enqueue(i)
#     assert_error q.enqueue(9)

# test 'RingBuffer wrap around':
#     let q = RingBuffer(4)
#     for i in range(4): q.enqueue(i)
#     assert q.full?()
#     assert q.dequeue() == 0
#     assert q.dequeue() == 1
#     q.enqueue(4)
#     q.enqueue(5)
#     assert q.full?()
#     assert q.dequeue() == 2
#     q.enqueue(6)
#     assert q.dequeue() == 3
#     assert q.dequeue() == 4
#     assert q.dequeue() == 5
#     assert q.dequeue() == 6
